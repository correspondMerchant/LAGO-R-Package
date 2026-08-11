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
// The reference values below were produced in R (LAGO's calculate_cost /
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

console.log("\nAll " + passed + " JS math assertions passed.");
