// Client-side D3 (v7) rendering for the interactive lago_report() HTML report.
//
// This file is self-contained: it depends only on the vendored D3 v7 library
// (inst/js/d3.v7.min.js) and never touches Shiny, jQuery, or any external CDN,
// so the rendered report stays a single, offline, shareable HTML file. It is a
// SEPARATE file from inst/js/cost-curves.js (the Shiny cost visualizer); the
// small polynomial-cost math below is COPIED from there on purpose so the two
// stay independent and cost-curves.js can keep its Shiny wiring untouched.
//
// The file is split into two parts, mirroring cost-curves.js:
//   1. Pure math (costAt / marginalAt / sampleCurve). No DOM/d3 dependency, so
//      it can be unit-tested under node (tests/js/test-report-math.js).
//   2. Browser rendering, exposed as window.LAGOReport with two entry points:
//        LAGOReport.renderConfidenceSet(elementId, data)
//        LAGOReport.renderCostCurves(elementId, data)
//      Each takes the JSON the report serialized with jsonlite and a target
//      element id, and draws into that element. They are no-ops when d3 is
//      absent, so the report degrades gracefully.

(function (global) {
  "use strict";

  // ------------------------------------------------------------------
  // 1. Pure math (mirrors R's calculate_cost / calculate_derivative).
  //    Copied from cost-curves.js so this file has no dependency on it.
  // ------------------------------------------------------------------

  // Total cost at x: sum_k coef[k] * x^k, via Horner's method.
  function costAt(coefs, x) {
    var acc = 0;
    for (var k = coefs.length - 1; k >= 0; k--) {
      acc = acc * x + coefs[k];
    }
    return acc;
  }

  // Marginal cost (first derivative) at x: sum_k k * coef[k] * x^(k-1).
  function marginalAt(coefs, x) {
    var n = coefs.length;
    if (n <= 1) {
      return 0;
    }
    var acc = 0;
    for (var k = n - 1; k >= 1; k--) {
      acc = acc * x + coefs[k] * k;
    }
    return acc;
  }

  // Sample fn evenly over [lb, ub] into {x, y} points.
  function sampleCurve(coefs, lb, ub, n, fn) {
    var pts = [];
    var step = (ub - lb) / (n - 1);
    for (var i = 0; i < n; i++) {
      var x = i === n - 1 ? ub : lb + i * step;
      pts.push({ x: x, y: fn(coefs, x) });
    }
    return pts;
  }

  var math = {
    costAt: costAt,
    marginalAt: marginalAt,
    sampleCurve: sampleCurve
  };

  // Export for node (unit tests). Harmless in the browser.
  if (typeof module !== "undefined" && module.exports) {
    module.exports = math;
  }
  // Also expose on the global for browser debugging / reuse.
  if (global) {
    global.LAGOReportMath = math;
  }

  // ------------------------------------------------------------------
  // 2. Browser rendering (skipped under node, where d3 is undefined).
  // ------------------------------------------------------------------
  if (typeof d3 === "undefined") {
    // Still expose stub entry points so a caller never throws; they no-op.
    if (global) {
      global.LAGOReport = {
        renderConfidenceSet: function () {},
        renderCostCurves: function () {}
      };
    }
    return;
  }

  // Fixed internal coordinate system. Every <svg> is sized to 100% width with a
  // viewBox so it scales with the page and never measures zero.
  var W = 640;
  var H = 380;
  var MARGIN = { top: 34, right: 24, bottom: 46, left: 66 };
  var IW = W - MARGIN.left - MARGIN.right;
  var IH = H - MARGIN.top - MARGIN.bottom;

  var COLOR_POINT = "#0066cc"; // grid interventions in the confidence set
  var COLOR_REC = "#cc3300"; // recommended intervention (matches plot.lago)
  var COLOR_TOTAL = "#0066cc"; // total-cost curve
  var COLOR_MARGINAL = "#cc3300"; // marginal-cost curve

  var fmt = d3.format(".4~g");

  // Create a titled, axed SVG inside `sel` and return the plot group plus the
  // scales. Shared by the confidence-set and cost-curve charts.
  function makeSvg(sel, opts) {
    var svg = sel
      .append("svg")
      .attr("viewBox", "0 0 " + W + " " + H)
      .attr("width", "100%")
      .attr("preserveAspectRatio", "xMidYMid meet")
      .style("font-family", "sans-serif")
      .style("overflow", "visible")
      .style("max-width", "760px")
      .style("display", "block");

    var g = svg
      .append("g")
      .attr("transform", "translate(" + MARGIN.left + "," + MARGIN.top + ")");

    // axes
    g.append("g")
      .attr("transform", "translate(0," + IH + ")")
      .call(d3.axisBottom(opts.xScale).ticks(6))
      .selectAll("text")
      .style("font-size", "11px");
    if (opts.yScale) {
      g.append("g")
        .call(d3.axisLeft(opts.yScale).ticks(6))
        .selectAll("text")
        .style("font-size", "11px");
    }

    // axis titles
    g.append("text")
      .attr("x", IW / 2)
      .attr("y", IH + 38)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .text(opts.xlab);
    if (opts.ylab) {
      g.append("text")
        .attr("transform", "rotate(-90)")
        .attr("x", -IH / 2)
        .attr("y", -MARGIN.left + 16)
        .attr("text-anchor", "middle")
        .style("font-size", "12px")
        .text(opts.ylab);
    }

    // plot title
    svg
      .append("text")
      .attr("x", MARGIN.left)
      .attr("y", 18)
      .style("font-size", "13px")
      .style("font-weight", "600")
      .text(opts.title);

    return { svg: svg, g: g };
  }

  // A reusable SVG tooltip group that follows a point and word-wraps its lines.
  // Returns { show(lines, px, py), hide() }. `lines` is an array of strings.
  function makeTooltip(g) {
    var tip = g.append("g").style("display", "none").style("pointer-events", "none");
    var rect = tip
      .append("rect")
      .attr("fill", "white")
      .attr("stroke", "#ccc")
      .attr("rx", 3)
      .attr("opacity", 0.96);
    var text = tip.append("text").attr("x", 6).attr("y", 14).style("font-size", "11px");

    function show(lines, px, py) {
      text.selectAll("tspan").remove();
      lines.forEach(function (ln, i) {
        text
          .append("tspan")
          .attr("x", 6)
          .attr("dy", i === 0 ? 0 : 14)
          .text(ln);
      });
      var bb = text.node().getBBox();
      rect.attr("width", bb.width + 12).attr("height", bb.height + 8);
      // keep the tooltip inside the plot box
      var tx = px + 10 + bb.width + 12 > IW ? px - bb.width - 22 : px + 10;
      var ty = Math.min(Math.max(0, py - 10), IH - bb.height - 8);
      tip.attr("transform", "translate(" + tx + "," + ty + ")").style("display", null);
      tip.raise();
    }

    function hide() {
      tip.style("display", "none");
    }

    return { show: show, hide: hide };
  }

  // Nice numeric [min, max] padded by `frac`, always finite and non-degenerate.
  function paddedExtent(values, frac) {
    var lo = d3.min(values);
    var hi = d3.max(values);
    if (!isFinite(lo) || !isFinite(hi)) {
      return [0, 1];
    }
    if (lo === hi) {
      var d = Math.abs(lo) > 0 ? Math.abs(lo) * 0.1 : 1;
      return [lo - d, hi + d];
    }
    var pad = (hi - lo) * (frac == null ? 0.06 : frac);
    return [lo - pad, hi + pad];
  }

  // ---- confidence-set viz -------------------------------------------------
  //
  // data.components : column names in data.cs that hold the intervention
  //                   components (1 or 2 of them, matching rec_int).
  // data.cs         : array of row objects, one per grid intervention in the
  //                   95% confidence set. Each has the component columns plus
  //                   CI_lower_bound, CI_upper_bound, and cost.
  // data.rec_int    : the recommended intervention, one value per component.
  //
  // Two components -> scatter of the two components with the recommended point
  // highlighted. One component -> a 1-D strip (number line) of the doses. 3+
  // components are handled by the R fallback (static plot), so this no-ops.
  function renderConfidenceSet(elementId, data) {
    var host = document.getElementById(elementId);
    if (!host || !data || !data.cs || !data.components) {
      return;
    }
    var comps = data.components;
    var cs = data.cs;
    var rec = data.rec_int || [];
    var sel = d3.select(host);
    sel.selectAll("*").remove();

    if (comps.length === 2) {
      renderCsScatter(sel, comps, cs, rec, data.rec_int_cost);
    } else if (comps.length === 1) {
      renderCsStrip(sel, comps, cs, rec, data.rec_int_cost);
    }
    // 3+ components: intentionally nothing (R renders the static fallback).
  }

  // Format the shared tooltip lines for one confidence-set row.
  function csTooltipLines(comps, row) {
    var lines = comps.map(function (c) {
      return c + ": " + fmt(row[c]);
    });
    if (row.cost != null && isFinite(row.cost)) {
      lines.push("cost: " + fmt(row.cost));
    }
    if (row.CI_lower_bound != null && row.CI_upper_bound != null) {
      lines.push("95% CI: " + fmt(row.CI_lower_bound) + " - " + fmt(row.CI_upper_bound));
    }
    return lines;
  }

  function renderCsScatter(sel, comps, cs, rec, recCost) {
    var xKey = comps[0];
    var yKey = comps[1];

    var xs = cs.map(function (r) { return +r[xKey]; });
    var ys = cs.map(function (r) { return +r[yKey]; });
    if (rec.length >= 2) {
      xs.push(+rec[0]);
      ys.push(+rec[1]);
    }
    var xScale = d3.scaleLinear().domain(paddedExtent(xs)).range([0, IW]).nice();
    var yScale = d3.scaleLinear().domain(paddedExtent(ys)).range([IH, 0]).nice();

    var chart = makeSvg(sel, {
      title: "95% confidence set",
      xlab: xKey,
      ylab: yKey,
      xScale: xScale,
      yScale: yScale
    });
    var g = chart.g;
    var tip = makeTooltip(g);

    // subtitle
    chart.svg
      .append("text")
      .attr("x", MARGIN.left)
      .attr("y", 30)
      .style("font-size", "11px")
      .style("fill", "#555")
      .text("Red diamond: recommended intervention. Hover a point for details.");

    // confidence-set points
    g.selectAll("circle.cs-pt")
      .data(cs)
      .enter()
      .append("circle")
      .attr("class", "cs-pt")
      .attr("cx", function (d) { return xScale(+d[xKey]); })
      .attr("cy", function (d) { return yScale(+d[yKey]); })
      .attr("r", 5)
      .attr("fill", COLOR_POINT)
      .attr("fill-opacity", 0.55)
      .attr("stroke", "white")
      .attr("stroke-width", 0.75)
      .style("cursor", "pointer")
      .on("mouseover", function (event, d) {
        d3.select(this).attr("fill-opacity", 1).attr("r", 6.5);
        tip.show(csTooltipLines(comps, d), xScale(+d[xKey]), yScale(+d[yKey]));
      })
      .on("mouseout", function () {
        d3.select(this).attr("fill-opacity", 0.55).attr("r", 5);
        tip.hide();
      });

    // recommended intervention (red diamond)
    if (rec.length >= 2) {
      var rx = xScale(+rec[0]);
      var ry = yScale(+rec[1]);
      g.append("path")
        .attr("d", d3.symbol().type(d3.symbolDiamond).size(150)())
        .attr("transform", "translate(" + rx + "," + ry + ")")
        .attr("fill", COLOR_REC)
        .attr("stroke", "white")
        .attr("stroke-width", 1)
        .style("cursor", "pointer")
        .on("mouseover", function () {
          var lines = [
            "recommended",
            xKey + ": " + fmt(rec[0]),
            yKey + ": " + fmt(rec[1])
          ];
          if (recCost != null && isFinite(recCost)) {
            lines.push("cost: " + fmt(recCost));
          }
          tip.show(lines, rx, ry);
        })
        .on("mouseout", function () {
          tip.hide();
        });
    }
  }

  function renderCsStrip(sel, comps, cs, rec, recCost) {
    var xKey = comps[0];
    var xs = cs.map(function (r) { return +r[xKey]; });
    if (rec.length >= 1) {
      xs.push(+rec[0]);
    }
    var xScale = d3.scaleLinear().domain(paddedExtent(xs)).range([0, IW]).nice();
    var yLine = IH / 2;

    var chart = makeSvg(sel, {
      title: "95% confidence set",
      xlab: xKey,
      ylab: null,
      xScale: xScale,
      yScale: null
    });
    var g = chart.g;
    var tip = makeTooltip(g);

    chart.svg
      .append("text")
      .attr("x", MARGIN.left)
      .attr("y", 30)
      .style("font-size", "11px")
      .style("fill", "#555")
      .text("Each dot is a dose in the set. Red: recommended. Hover for the CI.");

    // baseline
    g.append("line")
      .attr("x1", 0)
      .attr("x2", IW)
      .attr("y1", yLine)
      .attr("y2", yLine)
      .attr("stroke", "#ccc");

    // dose points along the line
    g.selectAll("circle.cs-pt")
      .data(cs)
      .enter()
      .append("circle")
      .attr("class", "cs-pt")
      .attr("cx", function (d) { return xScale(+d[xKey]); })
      .attr("cy", yLine)
      .attr("r", 5)
      .attr("fill", COLOR_POINT)
      .attr("fill-opacity", 0.55)
      .attr("stroke", "white")
      .attr("stroke-width", 0.75)
      .style("cursor", "pointer")
      .on("mouseover", function (event, d) {
        d3.select(this).attr("fill-opacity", 1).attr("r", 6.5);
        tip.show(csTooltipLines(comps, d), xScale(+d[xKey]), yLine);
      })
      .on("mouseout", function () {
        d3.select(this).attr("fill-opacity", 0.55).attr("r", 5);
        tip.hide();
      });

    // recommended dose (red)
    if (rec.length >= 1) {
      var rx = xScale(+rec[0]);
      g.append("circle")
        .attr("cx", rx)
        .attr("cy", yLine)
        .attr("r", 7)
        .attr("fill", COLOR_REC)
        .attr("stroke", "white")
        .attr("stroke-width", 1.5)
        .style("cursor", "pointer")
        .on("mouseover", function () {
          var lines = ["recommended", xKey + ": " + fmt(rec[0])];
          if (recCost != null && isFinite(recCost)) {
            lines.push("cost: " + fmt(recCost));
          }
          tip.show(lines, rx, yLine);
        })
        .on("mouseout", function () {
          tip.hide();
        });
    }
  }

  // ---- cost-curve viz -----------------------------------------------------
  //
  // data.cost.components : one name per intervention component.
  // data.cost.coefs      : one polynomial-coefficient vector per component
  //                        (matches lago_optimization()'s cost_list_of_vectors).
  // data.cost.lb / .ub   : lower / upper bound per component.
  //
  // For each component two curves are drawn: total cost and marginal cost,
  // each with a hover read-out. Mirrors the visuals of cost-curves.js but is
  // static (no sliders / drag): the report shows the cost functions the
  // optimization actually used.
  function renderCostCurves(elementId, data) {
    var host = document.getElementById(elementId);
    if (!host || !data || !data.cost || !data.cost.coefs) {
      return;
    }
    var cost = data.cost;
    var comps = cost.components || [];
    var sel = d3.select(host);
    sel.selectAll("*").remove();

    cost.coefs.forEach(function (coefs, i) {
      var name = comps[i] != null ? comps[i] : "Component " + (i + 1);
      var lb = +cost.lb[i];
      var ub = +cost.ub[i];
      if (!isFinite(lb) || !isFinite(ub) || !(ub > lb)) {
        return; // skip a degenerate range instead of drawing garbage
      }
      var block = sel
        .append("div")
        .style("margin-bottom", "18px")
        .style("border-top", i === 0 ? "none" : "1px solid #eee")
        .style("padding-top", i === 0 ? "0" : "10px");
      costCurveChart(block, coefs, lb, ub, name, "total");
      costCurveChart(block, coefs, lb, ub, name, "marginal");
    });
  }

  // Draw one cost curve ("total" or "marginal") for a component into `sel`.
  function costCurveChart(sel, coefs, lb, ub, name, kind) {
    var isTotal = kind === "total";
    // sampleCurve calls fn(coefs, x), so pass the two-argument math function
    // itself; valueAt is the one-argument form used by the hover read-out.
    var curveFn = isTotal ? costAt : marginalAt;
    var valueAt = function (x) { return curveFn(coefs, x); };
    var color = isTotal ? COLOR_TOTAL : COLOR_MARGINAL;
    var ylab = isTotal ? "Total cost" : "Marginal cost";
    var title = (isTotal ? "Total cost - " : "Marginal cost - ") + name;

    var pts = sampleCurve(coefs, lb, ub, 200, curveFn);
    var ys = pts.map(function (p) { return p.y; });
    // always include 0 so a flat/zero curve and the baseline stay visible
    var yLo = Math.min(0, d3.min(ys));
    var yHi = d3.max(ys);
    if (!(yHi > yLo)) {
      yHi = yLo + 1;
    }
    var yPad = (yHi - yLo) * 0.12;

    var xScale = d3.scaleLinear().domain([lb, ub]).range([0, IW]);
    var yScale = d3
      .scaleLinear()
      .domain([yLo - (yLo < 0 ? yPad : 0), yHi + yPad])
      .range([IH, 0]);

    var chart = makeSvg(sel, {
      title: title,
      xlab: name,
      ylab: ylab,
      xScale: xScale,
      yScale: yScale
    });
    var g = chart.g;

    var lineGen = d3
      .line()
      .x(function (p) { return xScale(p.x); })
      .y(function (p) { return yScale(p.y); });

    g.append("path")
      .datum(pts)
      .attr("fill", "none")
      .attr("stroke", color)
      .attr("stroke-width", 2)
      .attr("d", lineGen);

    // hover: vertical guide + dot + tooltip, reading the exact curve value.
    var tip = makeTooltip(g);
    var focus = g.append("g").style("display", "none");
    var guide = focus
      .append("line")
      .attr("y1", 0)
      .attr("y2", IH)
      .attr("stroke", "#888")
      .attr("stroke-dasharray", "3,3");
    var dot = focus
      .append("circle")
      .attr("r", 4)
      .attr("fill", color)
      .attr("stroke", "white")
      .attr("stroke-width", 1.5);

    g.append("rect")
      .attr("width", IW)
      .attr("height", IH)
      .attr("fill", "none")
      .attr("pointer-events", "all")
      .on("mouseover", function () {
        focus.style("display", null);
      })
      .on("mouseout", function () {
        focus.style("display", "none");
        tip.hide();
      })
      .on("mousemove touchmove", function (event) {
        var mx = d3.pointer(event, this)[0];
        var xv = Math.max(lb, Math.min(ub, xScale.invert(mx)));
        var yv = valueAt(xv);
        var px = xScale(xv);
        var py = yScale(yv);
        guide.attr("x1", px).attr("x2", px);
        dot.attr("cx", px).attr("cy", py);
        tip.show([name + ": " + fmt(xv), ylab + ": " + fmt(yv)], px, py);
      });
  }

  // Public API.
  if (global) {
    global.LAGOReport = {
      renderConfidenceSet: renderConfidenceSet,
      renderCostCurves: renderCostCurves
    };
  }
})(typeof window !== "undefined" ? window : this);
