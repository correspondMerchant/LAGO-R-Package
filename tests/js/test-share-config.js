// Pure-JS unit test of the playground "Share link" encode/decode in
// pkgdown/assets/share-config.js.
//
// Run with:  node tests/js/test-share-config.js
//
// It proves, independently of the browser/DOM:
//   1. buildShareQuery -> parseShareQuery round-trips the whole configuration
//      (dataset, outcome, otype, components, bounds, costs, goal, intent),
//   2. linearUnitCost / isDesignerCost classify cost vectors correctly, which is
//      what decides on restore whether a component gets a linear unit cost or a
//      designer coefficient vector,
//   3. a query carrying no configuration parses to null, and a truncated one
//      degrades to NaN entries rather than throwing.

var assert = require("assert");
var m = require("../../pkgdown/assets/share-config.js");

var pass = 0;
function ok(cond, msg) { assert.ok(cond, msg); pass++; }
function eq(a, b, msg) { assert.deepStrictEqual(a, b, msg); pass++; }

// ---- 1. round-trip: a mixed config (one linear cost, one designer vector) ----
var config = {
  dataset: "BB_data",
  outcome: "pre_post",
  otype: "binary",
  goal: 0.9,
  intent: "minimize",
  rows: [
    { name: "coaching_updt", lb: 0, ub: 40, costVec: [0, 222, 0.7, 0.03, 0.002] }, // designer
    { name: "datafeedback", lb: 0, ub: 3, costVec: [0, 1] },                        // linear c(0,1)
  ],
};
var r = m.parseShareQuery("?" + m.buildShareQuery(config));
ok(r !== null, "config query parses");
eq(r.dataset, "BB_data", "dataset round-trips");
eq(r.outcome, "pre_post", "outcome round-trips");
eq(r.otype, "binary", "otype round-trips");
eq(r.components, ["coaching_updt", "datafeedback"], "components round-trip in order");
eq(r.lower, [0, 0], "lower bounds round-trip");
eq(r.upper, [40, 3], "upper bounds round-trip");
eq(r.costs, [[0, 222, 0.7, 0.03, 0.002], [0, 1]], "cost vectors round-trip");
eq(Number(r.goal), 0.9, "goal round-trips");
eq(r.intent, "minimize", "intent round-trips");

// a component name containing a comma survives (repeated param, not comma-split)
var comma = m.parseShareQuery("?" + m.buildShareQuery({
  dataset: "d", outcome: "o", otype: "continuous", goal: 1, intent: "maximize",
  rows: [{ name: "a,b", lb: 1, ub: 2, costVec: [0, 5] }],
}));
eq(comma.components, ["a,b"], "component name with a comma round-trips");

// ---- 2. cost classification ----
eq(m.linearUnitCost([0, 5]), 5, "[0,x] is a linear unit cost x");
eq(m.linearUnitCost([0, 1]), 1, "[0,1] linear");
eq(m.linearUnitCost([3, 2]), null, "non-zero intercept is not linear");
eq(m.linearUnitCost([0, 1, 2]), null, "3-coef vector is not linear");
eq(m.linearUnitCost([0, NaN]), null, "non-finite slope is not linear");
eq(m.linearUnitCost("x"), null, "non-array is not linear");
ok(m.isDesignerCost([3, 2]), "2-coef non-zero-intercept vector is a designer cost");
ok(m.isDesignerCost([0, 1, 2, 3, 4]), "multi-coef vector is a designer cost");
ok(!m.isDesignerCost([]), "empty vector is not a designer cost");
ok(!m.isDesignerCost([1, NaN]), "vector with NaN is not a designer cost");
// the intentional fold: a linear designer cost [0,x] is classified linear (the
// restored optimization is identical either way, list(c(0, x)))
eq(m.linearUnitCost([0, 7]), 7, "designer [0,x] folds to the linear unit cost x");

// ---- 3. absent / truncated queries ----
eq(m.parseShareQuery(""), null, "empty query -> null (no restore)");
eq(m.parseShareQuery("?dataset=BB_data"), null, "dataset alone (no outcome/components) -> null");
var trunc = m.parseShareQuery("?components=a&components=b&lower=0");
ok(trunc !== null, "components-only query still parses");
eq(trunc.upper, [], "missing upper -> [] (so applyRestore skips each upper, no forced 0)");
ok(trunc.upper[0] === undefined, "absent upper index is undefined -> isFinite guard skips it");
eq(trunc.costs, [], "missing costs -> [] (each component keeps its default cost)");
eq(trunc.lower, [0], "partial lower kept as given");

console.log("share-config round-trip: " + pass + " assertions passed");
