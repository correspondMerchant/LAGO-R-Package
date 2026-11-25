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

// 6. sensFinitePoints: keep only points whose value AND cost are present and
// finite. A failed sweep run serializes cost as null; +null is 0 (finite), so
// the guard must reject null before the isFinite check (regression guard for
// the sensitivity chart's filter).
var sens = [
  { value: 30, cost: 12 },       // ok
  { value: 35, cost: null },     // failed run -> drop (must not plot at 0)
  { value: 40, cost: 5 },        // ok
  { value: null, cost: 7 },      // no swept value -> drop
  { value: 45, cost: NaN }       // non-finite cost -> drop
];
var kept = m.sensFinitePoints(sens);
check("sensFinitePoints keeps only the 2 finite points", kept.length === 2);
check("sensFinitePoints keeps the right rows", kept[0].value === 30 && kept[1].value === 40);
check("sensFinitePoints drops a null cost (not plotted at 0)",
  !kept.some(function (p) { return p.cost === null; }));
check("sensFinitePoints on [] is []", m.sensFinitePoints([]).length === 0);
check("sensFinitePoints on undefined is []", m.sensFinitePoints(undefined).length === 0);
check("sensFinitePoints keeps a legitimate zero cost",
  m.sensFinitePoints([{ value: 1, cost: 0 }]).length === 1);

// 7. budgetFinitePoints: keep only frontier points whose rec_int_cost AND
// est_outcome are present and finite (a null from R must be rejected before the
// finite check, since +null is 0). Regression guard for the budget chart.
var fr = [
  { rec_int_cost: 30, est_outcome: 0.6 },   // ok
  { rec_int_cost: null, est_outcome: 0.7 }, // failed run -> drop
  { rec_int_cost: 50, est_outcome: null },  // no outcome -> drop
  { rec_int_cost: 80, est_outcome: 0.9 },   // ok
  { rec_int_cost: NaN, est_outcome: 0.5 }   // non-finite -> drop
];
var bkept = m.budgetFinitePoints(fr);
check("budgetFinitePoints keeps only the 2 finite points", bkept.length === 2);
check("budgetFinitePoints keeps the right rows",
  bkept[0].rec_int_cost === 30 && bkept[1].rec_int_cost === 80);
check("budgetFinitePoints drops a null cost (not plotted at 0)",
  !bkept.some(function (p) { return p.rec_int_cost === null; }));
check("budgetFinitePoints on [] is []", m.budgetFinitePoints([]).length === 0);
check("budgetFinitePoints on undefined is []", m.budgetFinitePoints(undefined).length === 0);
check("budgetFinitePoints keeps a legitimate zero cost/outcome",
  m.budgetFinitePoints([{ rec_int_cost: 0, est_outcome: 0 }]).length === 1);

// 8. fmtValue / fmtCost: the hover read-outs round like the R text of the
// report (.lago_fmt_value and .lago_fmt_cost) and do not use e notation for
// ordinary numbers. The expected strings are what R writes for the same values.
check("fmtCost(23927.77) is 23927.77", m.fmtCost(23927.77) === "23927.77");
check("fmtCost(9999.77) is 9999.8", m.fmtCost(9999.77) === "9999.8");
check("fmtCost(63600) is 63600", m.fmtCost(63600) === "63600");
check("fmtCost(23927772138.456) is 23927772138.46", m.fmtCost(23927772138.456) === "23927772138.46");
check("fmtCost(2.392777e-05) is 0.000023928", m.fmtCost(2.392777e-05) === "0.000023928");
check("fmtValue(2.778472) is 2.7785", m.fmtValue(2.778472) === "2.7785");
check("fmtValue(13.62331) is 13.6233", m.fmtValue(13.62331) === "13.6233");
check("fmtValue(0.85) is 0.85", m.fmtValue(0.85) === "0.85");
check("fmtValue(0.0123456) keeps 4 significant digits", m.fmtValue(0.0123456) === "0.01235");
check("fmtValue(5e-7) is 0", m.fmtValue(5e-7) === "0");
check("fmtValue(-5e-7) is 0, not -0", m.fmtValue(-5e-7) === "0");
check("fmtValue(1234567) is 1234567", m.fmtValue(1234567) === "1234567");
check("fmtValue(NaN) is NaN", m.fmtValue(NaN) === "NaN");
check("fmtCost(Infinity) is Infinity", m.fmtCost(Infinity) === "Infinity");
// Round and small numbers, which R once wrote as 1e+05 and 1e-05. The R test
// test-bb3-present.R checks that .lago_num_text() gives the same strings.
check("fmtCost(100000) is 100000", m.fmtCost(100000) === "100000");
check("fmtCost(2e6) is 2000000", m.fmtCost(2e6) === "2000000");
check("fmtValue(1e-5) is 0.00001", m.fmtValue(1e-5) === "0.00001");
check("fmtCost(1e21) is 1e+21", m.fmtCost(1e21) === "1e+21");
check("fmtCost(1e-7) is 1e-7", m.fmtCost(1e-7) === "1e-7");

// 9. A cut digit of 5 rounds up, away from 0, from the decimal digits of the
// value, as R's .lago_round_dec() does. toFixed() and toPrecision() round by
// the binary value instead. The values and the expected strings are the same
// as bb4_ties in tests/testthat/test-bb4-present.R, so R and JS agree.
var ties = [
  [2.30495, "2.305", "2.305", "2.305"],
  [3487.45, "3487.45", "3487.5", "3487.45"],
  [0.10005, "0.1001", "0.10005", "0.1"],
  [12.3455, "12.3455", "12.346", "12.346"],
  [60.5405, "60.5405", "60.541", "60.541"],
  [5784.9499999999998, "5784.95", "5785", "5784.95"],
  [-2.30495, "-2.305", "-2.305", "-2.305"],
  [0.0123455, "0.01235", "0.012346", "0.01235"],
  [1.00005, "1.0001", "1.0001", "1"],
  [0.6789, "0.6789", "0.6789", "0.679"]
];
ties.forEach(function (t) {
  check("fmtValue(" + t[0] + ") is " + t[1], m.fmtValue(t[0]) === t[1]);
  check("fmtCost(" + t[0] + ") is " + t[2], m.fmtCost(t[0]) === t[2]);
  check("fmtInterval(" + t[0] + ") is " + t[3], m.fmtInterval(t[0]) === t[3]);
});
check("fmtInterval(Infinity) is Infinity", m.fmtInterval(Infinity) === "Infinity");
check("roundDec(0.005, 2) is 0.01", m.roundDec(0.005, 2, false) === 0.01);
check("roundDec(0.004, 2) is 0", m.roundDec(0.004, 2, false) === 0);
check("roundDec(0, 2) is 0", m.roundDec(0, 2, false) === 0);

// 10. numText writes at most 15 significant digits, as R's .lago_num_text()
// does, so a large number shows no floating point noise, and an exact tie in
// the 16th digit is settled to even, as jsonlite and R's sprintf() do.
var texts = [
  [1e16 + 2, "10000000000000000"],
  [123456789012.34567, "123456789012.346"],
  [1e25, "1e+25"],
  [1.5e-7, "1.5e-7"],
  [0.000001, "0.000001"],
  [-0, "0"],
  [1.5e21, "1.5e+21"],
  [-123456.5, "-123456.5"],
  [100000000000000.5, "100000000000000"],
  [100000000000001.5, "100000000000002"],
  [999999999999999.5, "1000000000000000"],
  [NaN, "NaN"]
];
texts.forEach(function (t) {
  check("numText(" + t[0] + ") is " + t[1], m.numText(t[0]) === t[1]);
});

// 11. roundDec rounds in one step from the 17 digits of a value, as R's
// .lago_round_dec() does, so a value is not rounded first to 15 digits and
// then again. A value whose 15 digits read back as the same double still
// rounds a cut digit of 5 up, as it is typed. The expected strings are the
// same as in tests/testthat/test-bb5-present.R.
check("fmtValue(404461827.45464969) is 404461827.4546",
  m.fmtValue(404461827.45464969) === "404461827.4546");
check("fmtValue(-2555838594.7868457) is -2555838594.7868",
  m.fmtValue(-2555838594.7868457) === "-2555838594.7868");
check("fmtInterval(404461827.45464969) is 404461827.455",
  m.fmtInterval(404461827.45464969) === "404461827.455");
check("roundDec(1.0000000000000049, 15) is 1", m.roundDec(1.0000000000000049, 15, true) === 1);
check("roundDec(0.12345678901234567, 20) keeps 15 digits",
  m.roundDec(0.12345678901234567, 20, true) === 0.123456789012346);
var dd = m.decDigits(2.30495);
check("decDigits(2.30495) cuts from its 15 digits", dd.cutDigits === "23049500000000000" && dd.cutExp === 0);
dd = m.decDigits(404461827.45464969);
check("decDigits(404461827.45464969) cuts from its 17 digits", dd.cutDigits === "40446182745464969" && dd.cutExp === 8);

// 12. A swept outcome goal and the estimated outcome are rounded like the
// estimate in R's print(), so a met goal does not look missed and a small
// goal does not show as 0. Other swept values keep fmtValue.
var tl = m.sensTooltipLines(["a"], "outcome_goal", { value: 5e-7, cost: 10, est: 0.850491, comps: { a: 0.85049 } });
check("a swept goal of 5e-7 shows as 5e-7", tl[0] === "outcome_goal: 5e-7");
check("the estimated outcome uses fmtInterval", tl[2] === "est. outcome: 0.85");
check("a component value uses fmtValue", tl[3] === "a: 0.8505");
tl = m.sensTooltipLines([], "cost_multiplier", { value: 5e-7, cost: 10, est: null });
check("another swept value keeps fmtValue", tl[0] === "cost_multiplier: 0" && tl.length === 2);

// 13. A cost of 1e4 or more shows 2 decimals, so the part that changes with
// the dose is not hidden under a large fixed cost, as in R's .lago_fmt_cost().
check("fmtCost(1000012.3456) is 1000012.35", m.fmtCost(1000012.3456) === "1000012.35");
check("fmtCost(-1000012.3456) is -1000012.35", m.fmtCost(-1000012.3456) === "-1000012.35");
check("fmtCost(1234.5678) keeps 5 significant digits", m.fmtCost(1234.5678) === "1234.6");
check("fmtCost(NaN) is NaN", m.fmtCost(NaN) === "NaN");

// 14. An estimate that reached its goal never shows on the missed side of it,
// as in R's .lago_fmt_estimate(): 0.81249998723151962 reached a goal of 0.8125
// to maximize, and shows as the goal does, 0.813, not as 0.812.
check("a reached goal to maximize shows the goal's text",
  m.fmtEstimate(0.81249998723151962, 0.8125, "maximize", true) === "0.813");
check("a goal that was not reached keeps the estimate",
  m.fmtEstimate(0.81249998723151962, 0.8125, "maximize", false) === "0.812");
check("a reached goal to minimize shows the goal's text",
  m.fmtEstimate(0.6125001, 0.6124999, "minimize", true) === "0.612");
check("an estimate above a goal to maximize stays as it is",
  m.fmtEstimate(0.9, 0.8125, "maximize", true) === "0.9");
check("an estimate farther from the goal than the tolerance stays as it is",
  m.fmtEstimate(0.8124, 0.8125, "maximize", true) === "0.812");
check("an estimate below a goal to minimize stays as it is",
  m.fmtEstimate(0.5, 0.6125, "minimize", true) === "0.5");
check("an estimate on the met side to minimize stays as it is",
  m.fmtEstimate(0.81249998723151962, 0.8125, "minimize", true) === "0.812");
check("without a goal the estimate stays as it is",
  m.fmtEstimate(0.81249998723151962, null, "maximize", true) === "0.812");
check("a missing estimate stays missing",
  m.fmtEstimate(NaN, 0.8125, "maximize", true) === "NaN");
tl = m.sensTooltipLines([], "outcome_goal", { value: 0.8125, cost: 10, est: 0.81249998723151962, shrunk: false }, "maximize");
check("a swept goal that was reached shows its estimate as the goal",
  tl[0] === "outcome_goal: 0.813" && tl[2] === "est. outcome: 0.813");
tl = m.sensTooltipLines([], "outcome_goal", { value: 0.8125, cost: 10, est: 0.81249998723151962, shrunk: true }, "maximize");
check("a swept goal that was shrunk keeps its estimate", tl[2] === "est. outcome: 0.812");
tl = m.sensTooltipLines([], "power_goal", { value: 0.8125, cost: 10, est: 0.81249998723151962, shrunk: false }, "maximize");
check("another sweep keeps fmtInterval for the estimate", tl[2] === "est. outcome: 0.812");

// 15. The budget hover never shows an affordable cost above the budget, as in
// R's .lago_fmt_cost_within(): it rounds such a cost down to the same digits.
check("a cost just under the budget rounds down", m.fmtCostWithin(1234.56, 1234.57) === "1234.5");
check("a large cost just under the budget rounds down", m.fmtCostWithin(12345.678, 12345.679) === "12345.67");
check("a cost that rounds under the budget is kept", m.fmtCostWithin(1234.54, 1234.57) === "1234.5");
check("a cost over the budget is kept", m.fmtCostWithin(1234.58, 1234.57) === "1234.6");
check("a cost without a budget is kept", m.fmtCostWithin(1234.56, null) === "1234.6");
check("a cost below 1 just under the budget rounds down", m.fmtCostWithin(0.123456, 0.1234565) === "0.12345");
check("a zero cost is kept", m.fmtCostWithin(0, 1) === "0");

// 16. The formatters shared with cost-curves.js are the same code.
var fs = require("fs");
var path = require("path");
function fnText(file, name) {
  var src = fs.readFileSync(path.join(__dirname, "..", "..", "inst", "js", file), "utf8");
  var at = src.indexOf("  function " + name + "(");
  return at < 0 ? null : src.slice(at, src.indexOf("\n  }\n", at) + 5);
}
["roundCost", "fmtCost", "fmtValue", "numText", "roundDec", "decDigits"].forEach(function (name) {
  var a = fnText("lago-report.js", name);
  check(name + " is the same in lago-report.js and cost-curves.js",
    a !== null && a === fnText("cost-curves.js", name));
});

console.log("\nAll " + passed + " JS report-math assertions passed.");
