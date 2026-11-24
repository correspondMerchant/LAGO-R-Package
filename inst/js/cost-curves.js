// Client-side D3 (v7) rendering of the cost-function curves for
// visualize_cost().
//
// Two SVGs are drawn per intervention component: the total cost curve and its
// derivative (marginal cost). The coefficient sliders drive the curves; when a
// slider moves the curves redraw entirely in the browser (no server round-trip
// for the redraw). The only trip back to R is the drag writeback: the total-cost
// curve carries a draggable handle at each of several anchor points, and
// dragging one reshapes the curve (refits the polynomial through the moved point
// via solveVandermonde) and pushes the reshaped coefficients back to the Shiny
// sliders on release.
//
// The file is split into two parts:
//   1. Pure math (costAt / marginalAt / sampleCurve / shiftCoefs / validate /
//      dragScale / anchorXs / solveVandermonde / formatSig / fmtValue /
//      fmtCost). These have no
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

  // Re-expand sum_k coef[k] x^k in t = (x - lb) / (ub - lb), which runs from 0
  // to 1 over the bounds, mirroring .shift_coefs() in visualize_cost.R. Far
  // from 0 the terms coef[k] x^k are large and nearly cancel, so evaluating in
  // x loses most of the digits, while in t the terms are about as large as the
  // cost. err[j] bounds the round-off in d[j]: a few units of the machine
  // epsilon times the sum of the sizes of the terms that make up d[j].
  var ROUNDOFF = 8 * 2.220446049250313e-16;
  function choose(k, j) {
    var r = 1;
    for (var i = 1; i <= j; i++) {
      r = r * (k - j + i) / i;
    }
    return r;
  }
  function shiftCoefs(coefs, lb, ub) {
    var n = coefs.length;
    var span = ub - lb;
    var lbPow = [1];
    var spanPow = [1];
    for (var p = 1; p < n; p++) {
      lbPow.push(lbPow[p - 1] * lb);
      spanPow.push(spanPow[p - 1] * span);
    }
    var d = [];
    var err = [];
    for (var j = 0; j < n; j++) {
      var sum = 0;
      var abs = 0;
      for (var k = j; k < n; k++) {
        var term = choose(k, j) * coefs[k] * lbPow[k - j];
        sum += term;
        abs += Math.abs(term);
      }
      d.push(sum * spanPow[j]);
      err.push(ROUNDOFF * abs * spanPow[j]);
    }
    return { d: d, err: err };
  }

  // Validity check, mirroring the server-side .cost_validity() in
  // visualize_cost.R:
  //   - total cost must never fall below its highest value so far,
  //   - marginal cost must be non-negative,
  //   - total cost must be non-negative,
  //   - the round-off in the coefficients and the evaluation must be at most
  //     0.1% of the rise of the curve (accurate). On narrow bounds far from 0
  //     the terms of a cubic nearly cancel, and a curve fitted through dragged
  //     points can then miss its points by more than it rises.
  // The curve is evaluated in the form of shiftCoefs(), and each check allows
  // only the round-off of that evaluation, so a fall that adds up over many
  // small steps, or a fall of a small cost, is still caught.
  // Returns {nonDecreasing, positiveMarginal, nonNegativeTotal, accurate}. The server
  // keeps using a 2000-point check for its authoritative text warnings; this
  // cheaper check drives the SVG colour cue only.
  function validate(coefs, lb, ub, n) {
    n = n || 500;
    var sh = shiftCoefs(coefs, lb, ub);
    var d = sh.d;
    var m = d.length;
    var coefErr = 0;
    var evalErr = 0;
    var margErr = 0;
    for (var j = 1; j < m; j++) {
      coefErr += sh.err[j];
      evalErr += ROUNDOFF * Math.abs(d[j]);
      margErr += j * sh.err[j] + ROUNDOFF * j * Math.abs(d[j]);
    }
    var totalErr = sh.err[0] + ROUNDOFF * Math.abs(d[0]) + coefErr + evalErr;
    var nonDecreasing = true;
    var positiveMarginal = true;
    var nonNegativeTotal = true;
    var highest = -Infinity;
    var lowTotal = Infinity;
    var highTotal = -Infinity;
    var level = 0;
    for (var i = 0; i < n; i++) {
      var t = i === n - 1 ? 1 : i / (n - 1);
      // the rise above the value at lb (Horner in t without d[0]), and the
      // derivative in t, which is the marginal cost times (ub - lb)
      var rise = 0;
      var marg = 0;
      for (var k = m - 1; k >= 1; k--) {
        rise = (rise + d[k]) * t;
        marg = marg * t + k * d[k];
      }
      highest = Math.max(highest, rise);
      if (rise < highest - (coefErr + 2 * evalErr)) {
        nonDecreasing = false;
      }
      if (marg < -margErr) {
        positiveMarginal = false;
      }
      if (d[0] + rise < -totalErr) {
        nonNegativeTotal = false;
      }
      lowTotal = Math.min(lowTotal, d[0] + rise);
      highTotal = Math.max(highTotal, d[0] + rise);
      level = Math.max(level, Math.abs(d[0] + rise));
    }
    // the size the round-off is measured against: the rise of the curve, or a
    // millionth of its level for a flat curve
    var size = Math.max(highTotal - lowTotal, 1e-6 * level);
    return {
      nonDecreasing: nonDecreasing,
      positiveMarginal: positiveMarginal,
      nonNegativeTotal: nonNegativeTotal,
      accurate: totalErr <= 1e-3 * size
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

  // n evenly spaced anchor x-positions across [lb, ub] (endpoints included).
  // These are the draggable control points; n of them (one per coefficient)
  // uniquely determine a degree-(n-1) polynomial.
  function anchorXs(lb, ub, n) {
    if (n <= 1) {
      return [ub];
    }
    var xs = [];
    var step = (ub - lb) / (n - 1);
    for (var i = 0; i < n; i++) {
      xs.push(i === n - 1 ? ub : lb + i * step);
    }
    return xs;
  }

  // Solve for the polynomial coefficients c[0..n-1] (in the same ascending-power
  // order as costAt) whose curve passes through the points (xs[i], ys[i]). It is
  // the inverse of costAt: dragging an anchor to a new y and re-solving reshapes
  // the curve to pass through the new point while holding the other anchors
  // fixed. The x values are first mapped to t = (x - mid) / half in [-1, 1], so
  // the system is well scaled for any bounds (very narrow bounds near 0 made the
  // raw powers of x tiny, and every solve failed). The system V d = y with
  // V[i][k] = t[i]^k is solved by Gaussian elimination with partial pivoting,
  // and d is then expanded back into powers of x. Returns null if the system is
  // singular (e.g. duplicated x) or the result is non-finite.
  function solveVandermonde(xs, ys) {
    var n = xs.length;
    var lo = Math.min.apply(null, xs);
    var hi = Math.max.apply(null, xs);
    var mid = (lo + hi) / 2;
    var half = (hi - lo) / 2;
    if (!(half > 0)) {
      half = 1;
    }
    // augmented matrix [V | y]
    var m = [];
    for (var i = 0; i < n; i++) {
      var row = [];
      var tp = 1;
      var t = (xs[i] - mid) / half;
      for (var k = 0; k < n; k++) {
        row.push(tp);
        tp *= t;
      }
      row.push(ys[i]);
      m.push(row);
    }
    for (var col = 0; col < n; col++) {
      // partial pivot
      var piv = col;
      for (var r = col + 1; r < n; r++) {
        if (Math.abs(m[r][col]) > Math.abs(m[piv][col])) {
          piv = r;
        }
      }
      // the entries are at most 1 in size, so this threshold is relative
      if (Math.abs(m[piv][col]) < 1e-12) {
        return null;
      }
      var tmp = m[col];
      m[col] = m[piv];
      m[piv] = tmp;
      // eliminate
      for (var r2 = 0; r2 < n; r2++) {
        if (r2 === col) {
          continue;
        }
        var f = m[r2][col] / m[col][col];
        for (var c2 = col; c2 <= n; c2++) {
          m[r2][c2] -= f * m[col][c2];
        }
      }
    }
    var d = [];
    for (var i2 = 0; i2 < n; i2++) {
      d.push(m[i2][n] / m[i2][i2]);
    }
    // expand sum_k d[k] ((x - mid) / half)^k into powers of x, using
    // ((x - mid) / half)^k = sum_j choose(k, j) x^j (-mid)^(k - j) / half^k
    var coefs = [];
    for (var j = 0; j < n; j++) {
      coefs.push(0);
    }
    for (var k2 = 0; k2 < n; k2++) {
      var scale = d[k2] / Math.pow(half, k2);
      var binom = 1;
      for (var j2 = 0; j2 <= k2; j2++) {
        // binom is choose(k2, j2)
        coefs[j2] += scale * binom * Math.pow(-mid, k2 - j2);
        binom = binom * (k2 - j2) / (j2 + 1);
      }
    }
    for (var i3 = 0; i3 < n; i3++) {
      if (!isFinite(coefs[i3])) {
        return null;
      }
    }
    return coefs;
  }

  // A number for a chart label, to 4 significant digits, so a small unit cost
  // such as 0.001 does not read as 0.00.
  function formatSig(v) {
    if (!isFinite(v)) {
      return String(v);
    }
    return String(+v.toPrecision(4));
  }

  // The number formatters of the report, copied from lago-report.js so the
  // designer rounds as the report and the R text do. A test checks that the
  // two copies agree.
  // The decimal digits of a finite, nonzero number, as 15 significant digits
  // (all a double holds without noise), with the sign and the power of 10 of
  // the first digit, the way R's .lago_dec_digits() gets them from sprintf().
  // toExponential() rounds an exact tie up, while sprintf() and jsonlite, which
  // writes the report data, round it to even, so a tie is settled to even
  // here too. The 31 digits of toExponential(30) show whether it is a tie.
  function decDigits(v) {
    var a = Math.abs(v);
    // 17 significant digits, rounded to 15 the way R's .lago_dec_digits()
    // does: 50 in the 16th and 17th digits goes to even, more goes up
    var s = a.toExponential(16);
    var all = s.charAt(0) + s.slice(2, 18);
    var exp = parseInt(s.slice(19), 10);
    var head = all.slice(0, 15);
    var rest = all.slice(15, 17);
    var up = rest > "50" || (rest === "50" && +head.charAt(14) % 2 === 1);
    var kept = +head + (up ? 1 : 0);
    if (kept >= 1e15) {
      kept = kept / 10;
      exp += 1;
    }
    var digits = String(kept);
    return { neg: v < 0, digits: digits, exp: exp };
  }

  // v rounded to n significant digits (sig true) or to n decimals (sig false),
  // from the decimal digits above, with a cut digit of 5 rounded up, away from
  // 0, as R's .lago_round_dec() does. toFixed() and toPrecision() settle a cut
  // digit of 5 by the binary value instead, so 0.10005 and 12.3455 would round
  // differently from the R text of the report.
  function roundDec(v, n, sig) {
    v = +v;
    if (!isFinite(v) || v === 0) {
      return v;
    }
    var p = decDigits(v);
    var keep = Math.min(sig ? n : n + p.exp + 1, 15);
    var next = keep >= 0 && keep < 15 ? +p.digits.charAt(keep) : 0;
    var kept = keep > 0 ? +p.digits.slice(0, keep) : 0;
    if (next >= 5) {
      kept += 1;
    }
    var r = Number((p.neg ? "-" : "") + kept + "e" + (p.exp + 1 - keep));
    // a value rounded to 0 is 0, not -0
    return r === 0 ? 0 : r;
  }

  // Write a rounded number the way R's .lago_num_text() writes it in the
  // report text: in full from 1e-6 up to 1e21, and in e notation such as 1e-7
  // and 1.5e+21 outside that range, with at most 15 significant digits.
  function numText(v) {
    v = +v;
    if (!isFinite(v)) {
      return String(v);
    }
    if (v === 0) {
      return "0";
    }
    var p = decDigits(v);
    var d = p.digits.replace(/0+$/, "");
    var k = d.length;
    var e = p.exp;
    var out;
    if (e < -6 || e > 20) {
      out = (k > 1 ? d.charAt(0) + "." + d.slice(1) : d) + "e" + (e >= 0 ? "+" : "-") + Math.abs(e);
    } else if (e >= k - 1) {
      out = d + new Array(e - k + 2).join("0");
    } else if (e < 0) {
      out = "0." + new Array(-e).join("0") + d;
    } else {
      out = d.slice(0, e + 1) + "." + d.slice(e + 1);
    }
    return (p.neg ? "-" : "") + out;
  }

  // A component value, rounded like R's .lago_fmt_value(): 4 decimals, or 4
  // significant digits for values closer to 0 than 0.1 (values closer to 0
  // than 1e-6 keep 4 decimals, so they show as 0).
  function fmtValue(v) {
    v = +v;
    var a = Math.abs(v);
    return numText(a >= 0.1 || a < 1e-6 || !isFinite(v) ? roundDec(v, 4, false) : roundDec(v, 4, true));
  }

  // A cost, rounded like R's .lago_fmt_cost(): 5 significant figures.
  function fmtCost(v) {
    return numText(roundDec(v, 5, true));
  }

  // A slider reports its value rounded to its step. The exact value the server
  // set is used while the slider sits within half a step of it, as the server
  // reads it, so a small high order coefficient is not drawn as 0.
  // The server sends the stale reading with 16 significant digits, so it can
  // differ from the slider's reading in the last digit. Two readings are the
  // same when they agree to a relative tolerance of 1e-12.
  function sameReading(value, stale) {
    return stale !== undefined && (value === stale ||
      Math.abs(value - stale) <= 1e-12 * Math.max(Math.abs(value), Math.abs(stale)));
  }

  function preferExact(value, exact) {
    if (exact && isFinite(value) &&
      (Math.abs(value - exact.value) <= exact.step / 2 || sameReading(value, exact.stale))) {
      return exact.value;
    }
    return value;
  }

  // a slider reading through its exact value. The stale reading after a range
  // edit lasts until the slider reports anything else.
  function readThroughExact(value, exact) {
    if (exact && isFinite(value) && exact.stale !== undefined && !sameReading(value, exact.stale)) {
      exact.stale = undefined;
    }
    return preferExact(value, exact);
  }

  var math = {
    costAt: costAt,
    marginalAt: marginalAt,
    sampleCurve: sampleCurve,
    shiftCoefs: shiftCoefs,
    validate: validate,
    dragScale: dragScale,
    anchorXs: anchorXs,
    solveVandermonde: solveVandermonde,
    formatSig: formatSig,
    fmtValue: fmtValue,
    fmtCost: fmtCost,
    preferExact: preferExact,
    readThroughExact: readThroughExact,
    sameReading: sameReading
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

  // the exact coefficient values the server set, by slider id
  var exactCoefs = {};
  function readExact(id, value) {
    return readThroughExact(value, exactCoefs[id]);
  }
  Shiny.addCustomMessageHandler("lago-exact-coef", function (m) {
    // a new scale for the slider, as when a form switch shows the row in
    // another unit. It is set before the slider is read again, so the slider's
    // old reading is read in the new unit, as the server reads it.
    if (m.scale !== null && m.scale !== undefined) {
      var el = document.getElementById(m.id);
      if (el) {
        el.setAttribute("data-lago-scale", m.scale);
      }
    }
    if (m.value === null || m.value === undefined) {
      delete exactCoefs[m.id];
    } else {
      exactCoefs[m.id] = {
        value: +m.value,
        step: +m.step,
        stale: m.stale === null || m.stale === undefined ? undefined : +m.stale
      };
    }
    drawAll();
  });

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

  // Read a coefficient value straight from its slider's DOM, preferring the
  // ionRangeSlider result (the authoritative live value) and falling back to
  // the input's value attribute (correct before ionRangeSlider initialises).
  // The slider shows the coefficient times its row's scale (data-lago-scale).
  function readSlider(id) {
    var el = document.getElementById(id);
    if (!el) {
      return NaN;
    }
    var scale = +el.getAttribute("data-lago-scale") || 1;
    if ($) {
      var irs = $(el).data("ionRangeSlider");
      if (irs && irs.result && isFinite(+irs.result.from)) {
        return readExact(id, +irs.result.from / scale);
      }
    }
    return readExact(id, parseFloat(el.value) / scale);
  }

  // Gather a container's configuration from its data-* attributes and the
  // current slider values.
  function readConfig(container) {
    var comp = +container.getAttribute("data-component");
    var ncoef = +container.getAttribute("data-ncoef");
    // the namespace prefix of the slider and drag ids, empty in the stand alone
    // app and the module id inside lago_app()
    var ns = container.getAttribute("data-ns") || "";
    var coefs = [];
    for (var i = 0; i < ncoef; i++) {
      coefs.push(readSlider(ns + "coef_" + comp + "_" + i));
    }
    return {
      comp: comp,
      ns: ns,
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
          .text(opts.xlab + ": " + fmtValue(xv));
        tipText
          .append("tspan")
          .attr("x", 6)
          .attr("dy", 14)
          .text(opts.ylab + ": " + fmtCost(yv));
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
      // tint invalid when the total cost decreases, dips below zero, or is not
      // accurate, so the SVG cue matches the server's warnings.
      invalid: !v.nonDecreasing || !v.nonNegativeTotal || !v.accurate,
      title: "Total Cost Function - " + cfg.name,
      xlab: cfg.name,
      ylab: "Total Cost",
      valueAt: function (x) {
        return costAt(cfg.coefs, x);
      }
    });
    addDragHandles(container, cfg, totalChart);

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
      .text("Unit Cost: " + formatSig(cfg.unitCost));
  }

  // Attach draggable handles to the total-cost curve so the user can RESHAPE it,
  // not just rescale it. One handle sits at each of n evenly spaced anchor
  // x-positions (n = number of coefficients), so the handles uniquely pin a
  // degree-(n-1) polynomial. Dragging one handle vertically holds the other
  // anchors' current cost values fixed and re-solves the polynomial through the
  // new set of points (solveVandermonde), reshaping the curve locally; on
  // release the reshaped coefficients are written back to R via
  // Shiny.setInputValue (the same path the sliders update through).
  function addDragHandles(container, cfg, chart) {
    var yScale = chart.yScale;
    var yRange = yScale.range(); // [IH, 0]
    var n = cfg.coefs.length;
    var xs = anchorXs(cfg.lb, cfg.ub, n);
    // The anchor cost values for the current curve; the fixed reference the
    // non-dragged anchors keep during a drag.
    var baseY = xs.map(function (x) {
      return costAt(cfg.coefs, x);
    });

    xs.forEach(function (xa, idx) {
      var handle = chart.g
        .append("circle")
        .attr("class", "cc-drag-handle")
        .attr("cx", chart.xScale(xa))
        .attr("cy", yScale(baseY[idx]))
        .attr("r", 6)
        .attr("fill", COLOR_TOTAL)
        .attr("stroke", "white")
        .attr("stroke-width", 2)
        .style("cursor", "ns-resize");
      handle.append("title").text("Drag to reshape the cost curve");

      var ys; // the anchor y-values held during this drag
      var working = cfg.coefs.slice();
      var reshaped = false;

      var drag = d3
        .drag()
        .on("start", function () {
          container._dragging = true; // suppress full redraws mid-drag
          ys = baseY.slice();
        })
        .on("drag", function (event) {
          // clamp the pointer to the plot's y pixel range, invert to a target
          // cost for this anchor, then re-solve the polynomial through all
          // anchors (this one moved, the rest fixed).
          var py = Math.max(yRange[1], Math.min(yRange[0], event.y));
          ys[idx] = yScale.invert(py);
          var solved = solveVandermonde(xs, ys);
          if (!solved) {
            return; // singular system, ignore this step
          }
          reshaped = true;
          working = solved;
          // optimistic local redraw of just this curve + handle (no rebuild, so
          // the drag gesture is not interrupted). The other handles already sit
          // on the curve because it passes through their fixed anchor values.
          var pts = sampleCurve(working, cfg.lb, cfg.ub, 200, costAt);
          chart.path.datum(pts).attr("d", chart.lineGen);
          // recolor live to match the post-release cue: red when the reshape
          // makes the total cost decrease, go negative or miss its points
          // (same condition baseChart uses for the total chart), instead of
          // staying blue until the drag ends.
          var v = validate(working, cfg.lb, cfg.ub, 500);
          chart.path.attr(
            "stroke",
            !v.nonDecreasing || !v.nonNegativeTotal || !v.accurate ?
              COLOR_INVALID : COLOR_TOTAL
          );
          handle.attr("cy", yScale(ys[idx]));
        })
        .on("end", function () {
          container._dragging = false;
          // write the reshaped coefficients back to R exactly once, as an event
          // (priority:"event" so it fires even when a value repeats). The R
          // observeEvent updates the sliders; that update fires the sliders'
          // change events which trigger a normal client-side redraw. The redraw
          // path never calls setInputValue, so there is no feedback oscillation.
          // A plain click (no drag) leaves reshaped false and writes nothing.
          if (reshaped) {
            Shiny.setInputValue(
              cfg.ns + "dragged_coefs_" + cfg.comp,
              { coefs: working, nonce: Date.now() },
              { priority: "event" }
            );
          }
        });
      handle.call(drag);
    });
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
      var ns = container.getAttribute("data-ns") || "";
      // Bind every rendered slider row (data-maxncoef), not just the current
      // form's data-ncoef, so sliders a Linear/Cubic form switch reveals also
      // trigger a client-side redraw. Falls back to data-ncoef if maxncoef is
      // absent.
      var ncoef =
        +container.getAttribute("data-maxncoef") ||
        +container.getAttribute("data-ncoef");
      for (var i = 0; i < ncoef; i++) {
        var id = ns + "coef_" + comp + "_" + i;
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

  // ionRangeSlider fakes a click at the handle when its track gets the focus,
  // which moves the value by a pixel's worth of the range when a keyboard user
  // only tabs through. Focus now only makes the slider the one the arrow keys
  // move, which is what the fake click was for, and leaves its value alone.
  function keepValueOnFocus() {
    if (!$ || !$.fn || !$.fn.ionRangeSlider) {
      return;
    }
    $("input.js-range-slider").each(function () {
      var inst = $(this).data("ionRangeSlider");
      if (!inst) {
        return;
      }
      var proto = Object.getPrototypeOf(inst);
      if (proto && !proto._lagoFocusPatched) {
        proto.pointerFocus = function () {
          // what the fake click set up for the arrow keys: this slider, its
          // single handle, and the pointer at the handle's center
          this.current_plugin = this.plugin_count;
          if (this.options.type === "single" && this.coords) {
            this.target = "single";
            this.coords.p_gap = this.coords.p_handle / 2;
            this.coords.p_pointer = this.coords.p_single_fake + this.coords.p_gap;
          }
        };
        proto._lagoFocusPatched = true;
      }
      // a slider made before the patch bound the old handler, so rebind it.
      // A slider made or rebuilt later binds the patched one itself.
      if (inst.$cache && inst.$cache.line) {
        var ev = "focus.irs_" + inst.plugin_count;
        inst.$cache.line.off(ev).on(ev, inst.pointerFocus.bind(inst));
      }
    });
  }

  function init() {
    keepValueOnFocus();
    bindSliders();
    drawAll();
  }

  // Redraw hook for the server: the Linear/Cubic form toggle updates each
  // container's data-ncoef and then calls this to repaint the curves with the
  // new coefficient count.
  // init binds the sliders of curves added after the page loaded, such as the
  // cost designer that lago_app() opens in a dialog, and draws them.
  global.LAGOCostCurves = { redraw: drawAll, init: init };

  // Draw once the client is connected (sliders exist by then). Also redraw when
  // a nav tab is shown (an inactive tab may have laid out at an odd size) and on
  // window resize.
  $ ? $(document).on("shiny:connected", init) : document.addEventListener("DOMContentLoaded", init);
  if ($) {
    $(document).on("shown.bs.tab", function () {
      setTimeout(drawAll, 30);
    });
    // a dialog lays out its content only once it is shown
    $(document).on("shown.bs.modal", function () {
      setTimeout(init, 30);
    });
  }
  global.addEventListener("resize", function () {
    setTimeout(drawAll, 50);
  });
})(typeof window !== "undefined" ? window : this);
