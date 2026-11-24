// Pure-JS unit test of the cost-curve math used by inst/js/cost-curves.js.
//
// Run with:  node tests/js/test-cost-math.js
//
// It proves, independently of Shiny/D3/browser:
//   1. costAt(x) polynomial eval matches R's calculate_cost (reference values
//      generated in R with the same coefficients),
//   2. marginalAt(x) matches R's calculate_derivative,
//   3. the drag-scaling rule produces a curve whose cost at ub equals the
//      target, and correctly no-ops on a zero / non-positive curve.
//
// The reference values below were produced in R (LAGOtrials' calculate_cost /
// calculate_derivative) for coefs = c(0.5, 1.2, 0.03, 0.004).

var assert = require("assert");
var m = require("../../inst/js/cost-curves.js");

var coefs = [0.5, 1.2, 0.03, 0.004];
var xs = [0, 1, 2.5, 5, 7.3, 10];

// R reference (calculate_cost)
var R_COST = [
  0.5, 1.734, 3.75, 7.75, 12.414768, 19.5
];
// R reference (calculate_derivative)
var R_DERIV = [
  1.2, 1.272, 1.425, 1.8, 2.27748, 3.0
];

var TOL = 1e-9;
var passed = 0;

function check(name, cond) {
  if (!cond) {
    console.error("FAIL: " + name);
    process.exit(1);
  }
  passed++;
  console.log("ok  - " + name);
}

// 1. cost matches R
xs.forEach(function (x, i) {
  var got = m.costAt(coefs, x);
  check(
    "costAt(" + x + ") = " + got + " ~ R " + R_COST[i],
    Math.abs(got - R_COST[i]) < TOL
  );
});

// 2. derivative matches R
xs.forEach(function (x, i) {
  var got = m.marginalAt(coefs, x);
  check(
    "marginalAt(" + x + ") = " + got + " ~ R " + R_DERIV[i],
    Math.abs(got - R_DERIV[i]) < TOL
  );
});

// derivative is consistent with a finite-difference of cost (sanity)
xs.forEach(function (x) {
  var h = 1e-6;
  var fd = (m.costAt(coefs, x + h) - m.costAt(coefs, x - h)) / (2 * h);
  check(
    "marginalAt(" + x + ") ~ finite-diff",
    Math.abs(fd - m.marginalAt(coefs, x)) < 1e-4
  );
});

// 3a. drag scaling hits the target at ub
var ub = 10;
[5, 30, 100, 0.0].forEach(function (target) {
  var res = m.dragScale(coefs, ub, target);
  check("dragScale target=" + target + " scaled", res.scaled === true);
  var costUb = m.costAt(res.coefs, ub);
  check(
    "dragScale: cost(ub) = " + costUb + " ~ target " + target,
    Math.abs(costUb - target) < 1e-7
  );
  // shape preserved: ratio of cost at an interior point equals the endpoint
  // ratio (all coefs scaled by the same factor).
  if (target > 0) {
    var interior = m.costAt(res.coefs, 4) / m.costAt(coefs, 4);
    check(
      "dragScale target=" + target + " preserves shape (uniform ratio)",
      Math.abs(interior - res.ratio) < 1e-9
    );
  }
});

// 3b. negative target is clamped to 0
var negRes = m.dragScale(coefs, ub, -50);
check("dragScale negative target clamped", Math.abs(m.costAt(negRes.coefs, ub)) < 1e-7);

// 3c. zero curve cannot be scaled -> no-op
var zeroRes = m.dragScale([0, 0, 0, 0], ub, 42);
check("dragScale zero curve is a no-op", zeroRes.scaled === false);

// 3d. all-negative / non-positive-at-ub curve refuses to scale
var negCurve = m.dragScale([0, -1], ub, 10); // cost(ub) = -10 < 0
check("dragScale non-positive curve refuses", negCurve.scaled === false);

// 4. validate flags a decreasing curve and negative marginal
var good = m.validate([0, 1, 0.01], 0, 10);
check("validate: increasing curve is valid", good.nonDecreasing && good.positiveMarginal);
check("validate: increasing curve is non-negative total", good.nonNegativeTotal === true);
var bad = m.validate([0, 2, -1], 0, 10); // marginal 2 - 2x goes negative, then decreasing
check("validate: catches negative marginal", bad.positiveMarginal === false);
check("validate: catches decreasing total", bad.nonDecreasing === false);

// 5. (fix 2) validate flags a total cost that goes negative, matching the R
// server's is_positive_valid / negative_cost_warning. coefs [-5, 1] on [0,10]
// is increasing with positive marginal, but the total starts at -5 < 0.
var negTotal = m.validate([-5, 1], 0, 10);
check("validate: [-5,1] total flagged negative", negTotal.nonNegativeTotal === false);
check(
  "validate: [-5,1] is still non-decreasing / positive-marginal",
  negTotal.nonDecreasing === true && negTotal.positiveMarginal === true
);

// 6. (fix 1) dragging to the plot floor must not brick the handle. The browser
// clamps the drag target to a small positive floor (max(1e-6, |cost(ub)|*1e-4))
// so the coefficients never all collapse to exactly zero. Mirror that floor
// here and prove the resulting curve is (a) non-zero at ub and (b) still
// recoverable by a subsequent drag back up.
var floorUb = 10;
var floorStart = m.costAt(coefs, floorUb);
var targetFloor = Math.max(1e-6, Math.abs(floorStart) * 1e-4);
var floored = m.dragScale(coefs, floorUb, targetFloor);
check("dragScale to floor still scales", floored.scaled === true);
var floorCost = m.costAt(floored.coefs, floorUb);
check("dragScale to floor leaves cost(ub) > 0", floorCost > 0);
// recovery: from the tiny floored curve, a drag back up must succeed (the old
// code, which allowed target 0, left an all-zero curve that dragScale refused).
var recovered = m.dragScale(floored.coefs, floorUb, 100);
check("dragScale recovers from floored curve", recovered.scaled === true);
check(
  "dragScale recovery hits target",
  Math.abs(m.costAt(recovered.coefs, floorUb) - 100) < 1e-6
);
// contrast: an all-zero curve (what target 0 used to produce) is unrecoverable.
var brick = m.dragScale([0, 0, 0, 0], floorUb, 100);
check("all-zero curve is unrecoverable (why the floor matters)", brick.scaled === false);

// 7. (fix 3) the end-of-drag writeback must be guarded by a boolean set inside
// the drag handler, NOT by array identity. A fresh slice is never === the
// original, so the old `working !== cfg.coefs` guard was always true and fired
// on plain clicks / no-op drags. Prove the identity compare is dead here.
var sliceOfCoefs = coefs.slice();
check("fix 3: a fresh slice is never === the original (dead guard)", sliceOfCoefs !== coefs);

// 8. anchorXs: n evenly spaced points across [lb, ub], endpoints included.
var ax = m.anchorXs(1, 40, 5);
check("anchorXs returns n points", ax.length === 5);
check("anchorXs starts at lb", ax[0] === 1);
check("anchorXs ends exactly at ub", ax[4] === 40);
check("anchorXs is evenly spaced", Math.abs((ax[2] - ax[1]) - (ax[1] - ax[0])) < 1e-9);

// 9. solveVandermonde is the inverse of costAt: sampling a known polynomial at
// the anchors and solving must recover its coefficients (drag reshape relies on
// this). Use a realistic degree-4 total cost over [1, 40].
var poly = [1700, 1200, -30, 5, 0.02];
var axm = m.anchorXs(1, 40, poly.length);
var ym = axm.map(function (x) { return m.costAt(poly, x); });
var solved = m.solveVandermonde(axm, ym);
check("solveVandermonde recovers the coefficients", solved !== null);
for (var si = 0; si < poly.length; si++) {
  var rel = Math.abs(solved[si] - poly[si]) / (Math.abs(poly[si]) + 1e-9);
  check("solveVandermonde coef " + si + " round-trips (rel<1e-4)", rel < 1e-4);
}
// the reconstructed curve matches at arbitrary points, not just the anchors
[3.2, 17, 29.5].forEach(function (x) {
  check(
    "solveVandermonde curve matches at x=" + x,
    Math.abs(m.costAt(solved, x) - m.costAt(poly, x)) < 1e-3
  );
});

// 10. reshape semantics: move ONE anchor's y and re-solve; the new curve passes
// through the moved point and still through the untouched anchors.
var ys2 = axm.map(function (x) { return m.costAt(poly, x); });
ys2[2] = ys2[2] + 5000; // drag the middle anchor up
var reshaped = m.solveVandermonde(axm, ys2);
check("reshape hits the dragged anchor", Math.abs(m.costAt(reshaped, axm[2]) - ys2[2]) < 1e-3);
check("reshape keeps a fixed anchor", Math.abs(m.costAt(reshaped, axm[0]) - ys2[0]) < 1e-3);

// 11. solveVandermonde returns null on a singular system (duplicated x).
check("solveVandermonde null on duplicated x", m.solveVandermonde([1, 1, 2], [1, 2, 3]) === null);

// 12. preferExact reads a rounded slider value as the exact one within half a
// step, and a moved slider as moved.
var exact = { value: 5e-7, step: 1e-5 };
check("preferExact keeps the exact small value", m.preferExact(0, exact) === 5e-7);
check("preferExact reads a moved slider as moved", m.preferExact(2e-5, exact) === 2e-5);
check("preferExact without an exact value reads the slider", m.preferExact(0.3, undefined) === 0.3);
// after a range edit the slider still shows its old reading, which is not a move
var edited = { value: 1.851852e-5, step: 1e-6, stale: 2e-5 };
check("preferExact keeps the exact value at the stale reading", m.preferExact(2e-5, edited) === 1.851852e-5);
var moved = { value: 1.851852e-5, step: 1e-6, stale: 2e-5 };
check("a stale reading is kept while the slider does not move", m.readThroughExact(2e-5, moved) === 1.851852e-5);
check("a real move is read as moved", m.readThroughExact(5e-5, moved) === 5e-5);
check("a move back to the old reading is read as moved", m.readThroughExact(2e-5, moved) === 2e-5);
check("preferExact reads a real move after a range edit", m.preferExact(3e-5, edited) === 3e-5);
// The server sends the stale reading with 16 significant digits, so a reading
// that needs 17 differs from it in the last digit and is still not a move.
var reading17 = 0.1 + 0.2;
var stale16 = +reading17.toPrecision(16);
check("the 16 digit stale reading differs from the slider's", stale16 !== reading17);
check("preferExact keeps the exact value at a 16 digit stale reading",
  m.preferExact(reading17, { value: 0.25, step: 1e-3, stale: stale16 }) === 0.25);
var rounded = { value: 0.25, step: 1e-3, stale: stale16 };
check("a 16 digit stale reading is kept while the slider does not move",
  m.readThroughExact(reading17, rounded) === 0.25 && rounded.stale === stale16);
check("a reading 1e-9 away from the stale one is a move",
  m.readThroughExact(0.3 + 1e-9, rounded) === 0.3 + 1e-9 && rounded.stale === undefined);
check("sameReading needs a stale reading", m.sameReading(0.3, undefined) === false);
check("sameReading matches 0 with 0", m.sameReading(0, 0) === true);
check("sameReading tells a tiny value from 0", m.sameReading(1e-300, 0) === false);

// 13. a cubic drag still solves when the bounds are narrow near 0. Before, the
// raw powers of x fell below the fixed pivot threshold and every drag failed.
function designerCubic(lb, ub, uc) {
  var s = ub - lb, a4 = uc / (2 * s * s * s);
  return [a4 * Math.pow(lb, 4) + uc / 2 * lb, -4 * a4 * Math.pow(lb, 3) + uc / 2,
    6 * a4 * lb * lb, -4 * a4 * lb, a4];
}
[0.001, 0.0001].forEach(function (ubNarrow) {
  var cNarrow = designerCubic(0, ubNarrow, 1);
  var axNarrow = m.anchorXs(0, ubNarrow, 5);
  var ysNarrow = axNarrow.map(function (x) { return m.costAt(cNarrow, x); });
  ysNarrow[2] *= 1.1;
  var sNarrow = m.solveVandermonde(axNarrow, ysNarrow);
  check("cubic drag solves on [0, " + ubNarrow + "]", sNarrow !== null);
  var worst = Math.max.apply(null, axNarrow.map(function (x, i) {
    return Math.abs(m.costAt(sNarrow, x) - ysNarrow[i]);
  }));
  check("cubic drag on [0, " + ubNarrow + "] passes through the anchors", worst < 1e-9 * ubNarrow);
});
check("solveVandermonde still null on duplicated x in a narrow range",
  m.solveVandermonde([0, 0.0005, 0.0005, 0.001], [0, 1, 2, 3]) === null);

// 14. validate allows the round-off of a valid cubic whose narrow bounds sit
// far from 0, but still flags a truly decreasing curve there.
[[1000, 1001], [1e4, 1e4 + 1]].forEach(function (b) {
  var vFar = m.validate(designerCubic(b[0], b[1], 1), b[0], b[1], 500);
  check("valid cubic on [" + b + "] is not flagged",
    vFar.nonDecreasing && vFar.positiveMarginal && vFar.nonNegativeTotal);
});
var vDec = m.validate([2000, -1], 1000, 1001, 500);
check("decreasing line on [1000, 1001] is flagged", !vDec.nonDecreasing && !vDec.positiveMarginal);
var vNeg = m.validate([-5, 1], 0, 10, 500);
check("c(-5, 1) has a positive marginal but a negative total",
  vNeg.nonDecreasing && vNeg.positiveMarginal && !vNeg.nonNegativeTotal);

// 14b. shiftCoefs re-expands in t = (x - lb) / (ub - lb): 1 + 2x + 3x^2 at
// x = 1 + 2t is 6 + 16t + 12t^2
var sh = m.shiftCoefs([1, 2, 3], 1, 3);
check("shiftCoefs re-expands the cost", sh.d[0] === 6 && sh.d[1] === 16 && sh.d[2] === 12);
check("shiftCoefs bounds a small round-off", sh.err.every(function (e) { return e >= 0 && e < 1e-12; }));
// a cubic far from 0 that falls by 49 over its bounds is flagged, which the
// step by step check with its wide round-off allowance missed
[1000, 1e4, 1e5].forEach(function (lb) {
  var c = designerCubic(lb, lb + 1, 1);
  c[0] += 50 * lb;
  c[1] -= 50;
  var v = m.validate(c, lb, lb + 1, 500);
  check("falling cubic on [" + lb + ", " + (lb + 1) + "] is flagged",
    !v.nonDecreasing && !v.positiveMarginal);
});
check("designer cubic on [5000, 5010] is valid",
  m.validate(designerCubic(5000, 5010, 1), 5000, 5010, 500).nonDecreasing);
// a small cost that falls by 2% is flagged
var vSmall = m.validate([1e-6, -2e-8], 0, 1, 500);
check("a small falling cost is flagged", !vSmall.nonDecreasing && vSmall.nonNegativeTotal);

// 15. the unit cost label keeps significant digits for small values
check("formatSig(0.001) is 0.001", m.formatSig(0.001) === "0.001");
check("formatSig(2) is 2", m.formatSig(2) === "2");
check("formatSig(1.23456) is 1.235", m.formatSig(1.23456) === "1.235");
check("formatSig(NaN) is NaN", m.formatSig(NaN) === "NaN");

// 16. the designer tooltips round like the report: no e notation for an
// ordinary cost, and the same text as lago-report.js for every value.
var rep = require("../../inst/js/lago-report.js");
check("fmtCost(23928) is 23928", m.fmtCost(23928) === "23928");
check("fmtCost(12345.6) is 12346", m.fmtCost(12345.6) === "12346");
check("fmtValue(1000.37) is 1000.37", m.fmtValue(1000.37) === "1000.37");
check("fmtValue(5e6) is 5000000", m.fmtValue(5e6) === "5000000");
[23928, 1000.37, 1000.999, 12345.6, 1e-7, 0.000123456, 5e6, -0, -2.5, 0.05,
  1e21, NaN, Infinity, 2.30495, 3487.45, 0.10005, 12.3455, 60.5405,
  5784.9499999999998, 1e16 + 2, 404461827.45464969, -2555838594.7868457,
  1.0000000000000049].forEach(function (v) {
  check("fmtValue(" + v + ") matches the report", m.fmtValue(v) === rep.fmtValue(v));
  check("fmtCost(" + v + ") matches the report", m.fmtCost(v) === rep.fmtCost(v));
});

// 17. a curve fitted through dragged points on narrow bounds far from 0 misses
// its points by more than it rises, which validate flags as not accurate. The
// other checks pass there, so before only the miss went unflagged.
var farXs = m.anchorXs(1e4, 1e4 + 1, 5);
var farFit = m.solveVandermonde(farXs, [10, 11, 12.5, 14, 20]);
var farMiss = Math.max.apply(null, farXs.map(function (x, i) {
  return Math.abs(m.costAt(farFit, x) - [10, 11, 12.5, 14, 20][i]);
}));
check("the fit on [1e4, 1e4 + 1] misses its points", farMiss > 1);
var vFit = m.validate(farFit, 1e4, 1e4 + 1, 500);
check("the fit on [1e4, 1e4 + 1] is not accurate", vFit.accurate === false);
check("the designer cubic on [1000, 1001] is not accurate",
  m.validate(designerCubic(1000, 1001, 1), 1000, 1001, 500).accurate === false);
// ordinary curves are accurate
var nearFit = m.solveVandermonde(m.anchorXs(0, 100, 5), [10, 11, 12.5, 14, 20]);
check("a fit on [0, 100] is accurate", m.validate(nearFit, 0, 100, 500).accurate === true);
check("the designer cubic on [10, 20] is accurate",
  m.validate(designerCubic(10, 20, 1), 10, 20, 500).accurate === true);
check("a line on [1000, 1001] is accurate", m.validate([0, 1], 1000, 1001, 500).accurate === true);
check("a constant cost is accurate", m.validate([5, 0, 0, 0, 0], 0, 10, 500).accurate === true);
check("a zero cost is accurate", m.validate([0, 0], 0, 10, 500).accurate === true);

console.log("\nAll " + passed + " JS math assertions passed.");
