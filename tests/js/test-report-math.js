// Pure-JS unit test of the cost math re-used by inst/js/lago-report.js.
//
// Run with:  node tests/js/test-report-math.js
//
// lago-report.js copies costAt / marginalAt / sampleCurve from cost-curves.js
// (so the report has no dependency on the Shiny file). This test proves that
// copy still matches R's calculate_cost / calculate_derivative, independently
// of D3 / the browser. The reference values below were produced in R for
// coefs = c(0.5, 1.2, 0.03, 0.004), the same coefficients cost-curves.js tests.

var assert = require("assert");
var m = require("../../inst/js/lago-report.js");

var coefs = [0.5, 1.2, 0.03, 0.004];
var xs = [0, 1, 2.5, 5, 7.3, 10];

// R reference (calculate_cost)
var R_COST = [0.5, 1.734, 3.75, 7.75, 12.414768, 19.5];
// R reference (calculate_derivative)
var R_DERIV = [1.2, 1.272, 1.425, 1.8, 2.27748, 3.0];

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

// 1. total cost matches R
xs.forEach(function (x, i) {
  var got = m.costAt(coefs, x);
  check(
    "costAt(" + x + ") = " + got + " ~ R " + R_COST[i],
    Math.abs(got - R_COST[i]) < TOL
  );
});

// 2. marginal cost matches R
xs.forEach(function (x, i) {
  var got = m.marginalAt(coefs, x);
  check(
    "marginalAt(" + x + ") = " + got + " ~ R " + R_DERIV[i],
    Math.abs(got - R_DERIV[i]) < TOL
  );
});

// 3. marginal is consistent with a finite-difference of cost (sanity)
xs.forEach(function (x) {
  var h = 1e-6;
  var fd = (m.costAt(coefs, x + h) - m.costAt(coefs, x - h)) / (2 * h);
  check(
    "marginalAt(" + x + ") ~ finite-diff",
    Math.abs(fd - m.marginalAt(coefs, x)) < 1e-4
  );
});

// 4. a constant (degree-0) curve has zero marginal everywhere
check("marginalAt of a constant is 0", m.marginalAt([3], 5) === 0);

// 5. sampleCurve: right count, exact endpoints, and values match valueAt
var n = 50;
var pts = m.sampleCurve(coefs, 1, 10, n, m.costAt);
check("sampleCurve returns n points", pts.length === n);
check("sampleCurve starts exactly at lb", pts[0].x === 1);
check("sampleCurve ends exactly at ub", pts[n - 1].x === 10);
check(
  "sampleCurve endpoint value = costAt(ub)",
  Math.abs(pts[n - 1].y - m.costAt(coefs, 10)) < TOL
);
check(
  "sampleCurve interior value matches costAt",
  Math.abs(pts[10].y - m.costAt(coefs, pts[10].x)) < TOL
);

console.log("\nAll " + passed + " JS report-math assertions passed.");
