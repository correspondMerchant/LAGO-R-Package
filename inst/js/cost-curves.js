// Client-side D3 (v7) rendering of the cost-function curves for
// visualize_cost().
//
// Two SVGs are drawn per intervention component: the total cost curve and its
// derivative (marginal cost). The coefficient sliders drive the curves; when a
// slider moves the curves redraw entirely in the browser (no server round-trip
// for the redraw). The only trip back to R is the drag writeback: dragging the
// right endpoint of the total-cost curve rescales all of that component's
// coefficients and pushes the rescaled values back to the Shiny sliders.
//
// The file is split into two parts:
//   1. Pure math (costAt / marginalAt / validate / dragScale). These have no
//      dependency on the DOM, d3, or Shiny and are exported for node so the
//      curve math can be unit-tested independently (tests/js/test-cost-math.js).
//   2. Browser rendering + Shiny wiring, only run when d3 and Shiny exist.

(function (global) {
  "use strict";

  // ------------------------------------------------------------------
  // 1. Pure math (mirrors R's calculate_cost / calculate_derivative)
  // ------------------------------------------------------------------

  // Total cost at x: sum_k coef[k] * x^k. Evaluated with Horner's method,
  // which matches the R sum(coef * x^(0:degree)) to floating-point precision.
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
    // derivative coefficients are coef[k]*k for k = 1..n-1, i.e. a polynomial
    // of degree n-2 with coefficient d[j] = coef[j+1]*(j+1).
    var acc = 0;
    for (var k = n - 1; k >= 1; k--) {
      acc = acc * x + coefs[k] * k;
    }
    return acc;
  }

  // Sample a function evenly over [lb, ub] into {x, y} points.
  function sampleCurve(coefs, lb, ub, n, fn) {
    var pts = [];
    var step = (ub - lb) / (n - 1);
    for (var i = 0; i < n; i++) {
      var x = i === n - 1 ? ub : lb + i * step;
      pts.push({ x: x, y: fn(coefs, x) });
    }
    return pts;
  }

  // Validity check, mirroring the server-side conditions in visualize_cost.R:
  //   - total cost must be non-decreasing (diff >= -1e-10),
  //   - marginal cost must be non-negative (>= -1e-10),
  //   - total cost must be non-negative (>= -1e-10), matching the server's
  //     is_positive_valid / negative_cost_warning condition.
  // Returns {nonDecreasing, positiveMarginal, nonNegativeTotal}. The server
  // keeps using a 2000-point check for its authoritative text warnings; this
  // cheaper check drives the SVG colour cue only.
  function validate(coefs, lb, ub, n) {
    n = n || 500;
    var tol = -1e-10;
    var nonDecreasing = true;
    var positiveMarginal = true;
    var nonNegativeTotal = true;
    var prev = costAt(coefs, lb);
    var step = (ub - lb) / (n - 1);
    for (var i = 0; i < n; i++) {
      var x = i === n - 1 ? ub : lb + i * step;
      var c = costAt(coefs, x);
      if (c - prev < tol) {
        nonDecreasing = false;
      }
      prev = c;
      if (marginalAt(coefs, x) < tol) {
        positiveMarginal = false;
      }
      if (c < tol) {
        nonNegativeTotal = false;
      }
    }
    return {
      nonDecreasing: nonDecreasing,
      positiveMarginal: positiveMarginal,
      nonNegativeTotal: nonNegativeTotal
    };
  }

  // The drag-scaling rule.
  //
  //   current = costAt(coefs, ub)
  //   - if |current| is ~0 (a flat / all-zero curve), the ratio is undefined:
  //     a zero curve cannot be scaled to a nonzero target, so we do NOT scale
  //     (the caller snaps the handle back).
  //   - if current < 0 (an invalid, decreasing curve dips below zero at ub) we
  //     also refuse: scaling by a negative ratio would flip the whole curve.
  //   - target is clamped to >= 0 (a negative total cost is invalid).
  //   ratio = target / current; every coefficient is multiplied by ratio, so
  //   the shape is preserved and cost(ub) becomes exactly target.
  function dragScale(coefs, ub, target) {
    var current = costAt(coefs, ub);
    if (!isFinite(current) || Math.abs(current) < 1e-9 || current <= 0) {
      return { scaled: false, coefs: coefs.slice(), ratio: 1 };
    }
    if (!(target >= 0)) {
      target = 0;
    }
    var ratio = target / current;
    var out = coefs.map(function (c) {
      return c * ratio;
    });
    return { scaled: true, coefs: out, ratio: ratio };
  }

  var math = {
    costAt: costAt,
    marginalAt: marginalAt,
    sampleCurve: sampleCurve,
    validate: validate,
    dragScale: dragScale
  };

  // Export for node (unit tests). Harmless in the browser.
  if (typeof module !== "undefined" && module.exports) {
    module.exports = math;
  }
  // Also expose on the global for browser debugging / reuse.
  if (global) {
    global.LAGOCostMath = math;
  }

  // ------------------------------------------------------------------
  // 2. Browser rendering + Shiny wiring (skipped under node)
  // ------------------------------------------------------------------
  if (typeof d3 === "undefined" || typeof Shiny === "undefined") {
    return;
  }

  var $ = global.jQuery;

  // Fixed internal coordinate system. The <svg> is sized to 100% width with a
  // viewBox, so it scales with the card and never measures zero even when its
  // nav tab is hidden (a plain width measurement would be 0 for inactive tabs).
  var W = 640;
  var H = 300;
  var MARGIN = { top: 34, right: 24, bottom: 42, left: 66 };
  var IW = W - MARGIN.left - MARGIN.right;
  var IH = H - MARGIN.top - MARGIN.bottom;

  var COLOR_TOTAL = "#0066cc";
  var COLOR_MARGINAL = "#cc3300";
  var COLOR_INVALID = "#dc3545";

  var fmt = d3.format(".4~g");

  // Read a coefficient value straight from its slider's DOM, preferring the
  // ionRangeSlider result (the authoritative live value) and falling back to
  // the input's value attribute (correct before ionRangeSlider initialises).
  function readSlider(id) {
    var el = document.getElementById(id);
    if (!el) {
      return NaN;
    }
    if ($) {
      var irs = $(el).data("ionRangeSlider");
      if (irs && irs.result && isFinite(+irs.result.from)) {
        return +irs.result.from;
      }
    }
    return parseFloat(el.value);
  }

  // Gather a container's configuration from its data-* attributes and the
  // current slider values.
  function readConfig(container) {
    var comp = +container.getAttribute("data-component");
    var ncoef = +container.getAttribute("data-ncoef");
    var coefs = [];
    for (var i = 0; i < ncoef; i++) {
      coefs.push(readSlider("coef_" + comp + "_" + i));
    }
    return {
      comp: comp,
      ncoef: ncoef,
      lb: +container.getAttribute("data-lb"),
      ub: +container.getAttribute("data-ub"),
      unitCost: +container.getAttribute("data-unit-cost"),
      name: container.getAttribute("data-name") || "",
      coefs: coefs
    };
  }

  // Draw axes, gridlines, curve, and hover interaction into one SVG. Returns
  // references (path, xScale, yScale, plot group) so the total-cost chart can
  // attach a drag handle afterwards.
  function baseChart(svg, opts) {
    svg
      .attr("viewBox", "0 0 " + W + " " + H)
      .attr("width", "100%")
      .attr("preserveAspectRatio", "xMidYMid meet")
      .style("font-family", "sans-serif")
      .style("overflow", "visible");
    svg.selectAll("*").remove();

    var xScale = d3.scaleLinear().domain([opts.lb, opts.ub]).range([0, IW]);

    // y-domain from the data with headroom, always including 0 as a baseline
    // so the marginal reference line and a flat/zero curve are visible.
    var ys = opts.points.map(function (p) {
      return p.y;
    });
    if (opts.extraY != null && isFinite(opts.extraY)) {
      ys.push(opts.extraY);
    }
    var yMin = Math.min(0, d3.min(ys));
    var yMax = d3.max(ys);
    if (!(yMax > yMin)) {
      yMax = yMin + 1;
    }
    var pad = (yMax - yMin) * 0.12;
    var yScale = d3
      .scaleLinear()
      .domain([yMin - (yMin < 0 ? pad : 0), yMax + pad])
      .range([IH, 0]);

    var g = svg
      .append("g")
      .attr("transform", "translate(" + MARGIN.left + "," + MARGIN.top + ")");

    // invalid background tint
    if (opts.invalid) {
      g.append("rect")
        .attr("width", IW)
        .attr("height", IH)
        .attr("fill", "rgba(220,53,69,0.06)");
    }

    // axes
    g.append("g")
      .attr("transform", "translate(0," + IH + ")")
      .call(d3.axisBottom(xScale).ticks(6))
      .selectAll("text")
      .style("font-size", "11px");
    g.append("g")
      .call(d3.axisLeft(yScale).ticks(6))
      .selectAll("text")
      .style("font-size", "11px");

    // axis titles
    g.append("text")
      .attr("x", IW / 2)
      .attr("y", IH + 36)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .text(opts.xlab);
    g.append("text")
      .attr("transform", "rotate(-90)")
      .attr("x", -IH / 2)
      .attr("y", -MARGIN.left + 16)
      .attr("text-anchor", "middle")
      .style("font-size", "12px")
      .text(opts.ylab);

    // plot title
    svg
      .append("text")
      .attr("x", MARGIN.left)
      .attr("y", 18)
      .style("font-size", "13px")
      .style("font-weight", "600")
      .text(opts.title);

    var lineGen = d3
      .line()
      .x(function (p) {
        return xScale(p.x);
      })
      .y(function (p) {
        return yScale(p.y);
      });

    var path = g
      .append("path")
      .attr("class", "cc-curve")
      .datum(opts.points)
      .attr("fill", "none")
      .attr("stroke", opts.invalid ? COLOR_INVALID : opts.color)
      .attr("stroke-width", 2)
      .attr("d", lineGen);

    // ---- hover: vertical guide, dot on curve, tooltip ----
    var focus = g.append("g").style("display", "none");
    focus
      .append("line")
      .attr("class", "cc-guide")
      .attr("y1", 0)
      .attr("y2", IH)
      .attr("stroke", "#888")
      .attr("stroke-dasharray", "3,3");
    focus
      .append("circle")
      .attr("r", 4)
      .attr("fill", opts.color)
      .attr("stroke", "white")
      .attr("stroke-width", 1.5);
    var tipG = focus.append("g");
    var tipRect = tipG
      .append("rect")
      .attr("fill", "white")
      .attr("stroke", "#ccc")
      .attr("rx", 3)
      .attr("width", 150)
      .attr("height", 34)
      .attr("opacity", 0.95);
    var tipText = tipG
      .append("text")
      .attr("x", 6)
      .attr("y", 14)
      .style("font-size", "11px");

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
      })
      .on("mousemove touchmove", function (event) {
        var mx = d3.pointer(event, this)[0];
        var xv = Math.max(opts.lb, Math.min(opts.ub, xScale.invert(mx)));
        var yv = opts.valueAt(xv);
        var px = xScale(xv);
        var py = yScale(yv);
        focus.select("line.cc-guide").attr("x1", px).attr("x2", px);
        focus.select("circle").attr("cx", px).attr("cy", py);
        // keep tooltip inside the plot
        var tx = px + 8 + 150 > IW ? px - 158 : px + 8;
        var ty = Math.max(0, py - 40);
        tipG.attr("transform", "translate(" + tx + "," + ty + ")");
        tipText.selectAll("tspan").remove();
        tipText
          .append("tspan")
          .attr("x", 6)
          .attr("dy", 0)
          .text(opts.xlab + ": " + fmt(xv));
        tipText
          .append("tspan")
          .attr("x", 6)
          .attr("dy", 14)
          .text(opts.ylab + ": " + fmt(yv));
        var bb = tipText.node().getBBox();
        tipRect.attr("width", bb.width + 12).attr("height", bb.height + 8);
      });

    return { g: g, xScale: xScale, yScale: yScale, path: path, lineGen: lineGen };
  }

  // Full (re)draw of both charts for one container.
  function drawContainer(container) {
    var cfg = readConfig(container);
    if (cfg.coefs.some(function (c) { return !isFinite(c); })) {
      return; // a slider value is mid-edit / not ready
    }
    var v = validate(cfg.coefs, cfg.lb, cfg.ub, 500);

    var totalPts = sampleCurve(cfg.coefs, cfg.lb, cfg.ub, 200, costAt);
    var margPts = sampleCurve(cfg.coefs, cfg.lb, cfg.ub, 200, marginalAt);

    var sel = d3.select(container);
    var totalSvg = sel.select("svg.cc-total");
    if (totalSvg.empty()) {
      totalSvg = sel.append("svg").attr("class", "cc-total");
      sel.append("svg").attr("class", "cc-marginal").style("margin-top", "6px");
    }
    var margSvg = sel.select("svg.cc-marginal");

    // ---- total cost chart (with drag handle) ----
    var totalChart = baseChart(totalSvg, {
      points: totalPts,
      lb: cfg.lb,
      ub: cfg.ub,
      color: COLOR_TOTAL,
      // tint invalid when the total cost decreases OR dips below zero, so the
      // SVG cue matches the server's plot_warning + negative_cost_warning.
      invalid: !v.nonDecreasing || !v.nonNegativeTotal,
      title: "Total Cost Function - " + cfg.name,
      xlab: cfg.name,
      ylab: "Total Cost",
      valueAt: function (x) {
        return costAt(cfg.coefs, x);
      }
    });
    addDragHandle(container, cfg, totalChart);

    // ---- marginal cost chart (with unit-cost reference line) ----
    var margChart = baseChart(margSvg, {
      points: margPts,
      lb: cfg.lb,
      ub: cfg.ub,
      color: COLOR_MARGINAL,
      invalid: !v.positiveMarginal,
      extraY: cfg.unitCost,
      title: "Derivative of the Total Cost Function (Marginal Cost) - " + cfg.name,
      xlab: cfg.name,
      ylab: "Marginal Cost",
      valueAt: function (x) {
        return marginalAt(cfg.coefs, x);
      }
    });
    // dashed unit-cost reference line + annotation (matches the ggplot geom_hline)
    var uy = margChart.yScale(cfg.unitCost);
    margChart.g
      .append("line")
      .attr("x1", 0)
      .attr("x2", IW)
      .attr("y1", uy)
      .attr("y2", uy)
      .attr("stroke", "black")
      .attr("stroke-width", 1)
      .attr("stroke-dasharray", "6,4");
    margChart.g
      .append("text")
      .attr("x", IW)
      .attr("y", uy - 4)
      .attr("text-anchor", "end")
      .style("font-size", "11px")
      .text("Unit Cost: " + cfg.unitCost.toFixed(2));
  }

  // Attach a draggable handle to the right endpoint (x = ub) of the total-cost
  // curve. Dragging rescales all coefficients and, on release, writes them back
  // to R via Shiny.setInputValue.
  function addDragHandle(container, cfg, chart) {
    var ub = cfg.ub;
    var handleX = chart.xScale(ub);
    var handleY = chart.yScale(costAt(cfg.coefs, ub));

    var handle = chart.g
      .append("circle")
      .attr("class", "cc-drag-handle")
      .attr("cx", handleX)
      .attr("cy", handleY)
      .attr("r", 7)
      .attr("fill", COLOR_TOTAL)
      .attr("stroke", "white")
      .attr("stroke-width", 2)
      .style("cursor", "ns-resize");
    handle.append("title").text("Drag to rescale all coefficients");

    var yScale = chart.yScale;
    var yRange = yScale.range(); // [IH, 0]
    var working = cfg.coefs.slice();
    var scaled = false; // set true only when a drag actually rescaled the coefs

    // Never let a single drag collapse the curve irreversibly to zero. Dragging
    // the handle to the plot floor would set target = 0, scaling every
    // coefficient by 0; cost(ub) would then be 0 forever and dragScale's
    // |current|~0 no-op guard would make the handle un-draggable (recovery only
    // via the sliders / reset). So clamp the target to a small positive floor:
    // 0.01% of the starting cost at ub, but never below 1e-6. A drag to the
    // floor leaves a tiny, still-recoverable curve instead of a hard zero.
    var startCost = costAt(cfg.coefs, ub);
    var targetFloor = Math.max(1e-6, Math.abs(startCost) * 1e-4);

    var drag = d3
      .drag()
      .on("start", function () {
        container._dragging = true; // suppress full redraws mid-drag
      })
      .on("drag", function (event) {
        // clamp the pointer to the plot's y pixel range, invert to a target
        // cost, then clamp the target to a small positive floor so the curve
        // can never collapse irreversibly to zero.
        var py = Math.max(yRange[1], Math.min(yRange[0], event.y));
        var target = Math.max(targetFloor, yScale.invert(py));
        var res = dragScale(cfg.coefs, ub, target);
        if (!res.scaled) {
          return; // zero / non-positive curve: cannot scale, snap back
        }
        scaled = true;
        working = res.coefs;
        // optimistic local redraw of just this curve + handle (no rebuild, so
        // the drag gesture is not interrupted).
        var pts = sampleCurve(working, cfg.lb, ub, 200, costAt);
        chart.path.datum(pts).attr("d", chart.lineGen);
        handle.attr("cy", yScale(costAt(working, ub)));
      })
      .on("end", function () {
        container._dragging = false;
        // write the rescaled coefficients back to R exactly once, as an event
        // (priority:"event" so it fires even when a value repeats). The R
        // observeEvent updates the sliders; that update fires the sliders'
        // change events which trigger a normal client-side redraw. The redraw
        // path never calls setInputValue, so there is no feedback oscillation.
        // Guard on `scaled` (set inside the drag handler), NOT array identity:
        // `working` is a fresh slice so it is never === cfg.coefs, which made
        // the old `working !== cfg.coefs` guard always true. A plain click or a
        // no-op drag now writes nothing back.
        if (scaled) {
          Shiny.setInputValue(
            "dragged_coefs_" + cfg.comp,
            { coefs: working, nonce: Date.now() },
            { priority: "event" }
          );
        }
      });
    handle.call(drag);
  }

  // ------------------------------------------------------------------
  // Wiring: initial draw + client-side redraw on slider change
  // ------------------------------------------------------------------

  function containers() {
    return document.querySelectorAll(".lago-cost-curves");
  }

  function drawAll() {
    containers().forEach(function (c) {
      if (!c._dragging) {
        drawContainer(c);
      }
    });
  }

  function bindSliders() {
    containers().forEach(function (container) {
      if (container._bound) {
        return;
      }
      container._bound = true;
      var comp = container.getAttribute("data-component");
      var ncoef = +container.getAttribute("data-ncoef");
      for (var i = 0; i < ncoef; i++) {
        var id = "coef_" + comp + "_" + i;
        var el = document.getElementById(id);
        if (!el) {
          continue;
        }
        // ionRangeSlider fires native 'input'/'change' live during a slide, so
        // this redraw is fully client-side (no server round-trip).
        var handler = (function (cont) {
          return function () {
            if (!cont._dragging) {
              drawContainer(cont);
            }
          };
        })(container);
        el.addEventListener("input", handler);
        el.addEventListener("change", handler);
        if ($) {
          $(el).on("change.lagocc input.lagocc", handler);
        }
      }
    });
  }

  function init() {
    bindSliders();
    drawAll();
  }

  // Draw once the client is connected (sliders exist by then). Also redraw when
  // a nav tab is shown (an inactive tab may have laid out at an odd size) and on
  // window resize.
  $ ? $(document).on("shiny:connected", init) : document.addEventListener("DOMContentLoaded", init);
  if ($) {
    $(document).on("shown.bs.tab", function () {
      setTimeout(drawAll, 30);
    });
  }
  global.addEventListener("resize", function () {
    setTimeout(drawAll, 50);
  });
})(typeof window !== "undefined" ? window : this);
