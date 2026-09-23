// Pure-JS unit test of the playground "Share link" encode/decode in
// pkgdown/assets/share-config.js.
//
// Run with:  node tests/js/test-share-config.js
//
// It proves, independently of the browser/DOM:
//   1. buildShareQuery -> parseShareQuery round-trips the whole configuration
//      (dataset, outcome, otype, components, bounds, costs, goal, intent, plus
//      the optional center characteristics, interaction terms, budget, and
//      sweep settings),
//   2. linearUnitCost / isDesignerCost classify cost vectors correctly, which is
//      what decides on restore whether a component gets a linear unit cost or a
//      designer coefficient vector,
//   3. a query carrying no configuration parses to null; a truncated numeric
//      list (lower/upper/costs/ccval) degrades to a non-finite entry the caller
//      skips rather than a forced 0; and an omitted optional field (cc/ccval/
//      int/budget/sweep) degrades to []/null so the opener keeps its own default.

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

// ---- 4. center characteristics, budget, sweep round-trip ----
var full = {
  dataset: "BB_data", outcome: "pp3_oxytocin_mother", otype: "binary",
  goal: 0.85, intent: "maximize",
  rows: [{ name: "coaching_updt", lb: 0, ub: 40, costVec: [0, 1] }],
  centerChars: [{ name: "birth_volume_100", value: 1.75 }, { name: "urban", value: 0 }],
  interactions: ["coaching_updt:launch_duration", "coaching_updt:datafeedback"],
  budget: 120.5,
  sweep: { param: "cost_multiplier", from: 0.5, to: 1.5, steps: 7 },
};
var f = m.parseShareQuery("?" + m.buildShareQuery(full));
eq(f.centerChars, ["birth_volume_100", "urban"], "center-char names round-trip in order");
eq(f.centerCharValues, [1.75, 0], "center-char values round-trip (including 0)");
eq(f.interactions, ["coaching_updt:launch_duration", "coaching_updt:datafeedback"],
  "interaction terms round-trip in order");
eq(f.budget, 120.5, "budget round-trips");
eq(f.sweep.param, "cost_multiplier", "sweep param round-trips");
eq(f.sweep.from, 0.5, "sweep from round-trips");
eq(f.sweep.to, 1.5, "sweep to round-trips");
eq(f.sweep.steps, 7, "sweep steps round-trips");

// a center-char name with a comma survives (repeated param, not comma-split)
var ccComma = m.parseShareQuery("?" + m.buildShareQuery({
  dataset: "d", outcome: "o", otype: "continuous", goal: 1, intent: "maximize",
  rows: [{ name: "a", lb: 0, ub: 1, costVec: [0, 1] }],
  centerChars: [{ name: "x,y", value: 2 }],
}));
eq(ccComma.centerChars, ["x,y"], "center-char name with a comma round-trips");

// an interaction term containing a comma survives (repeated `int` param)
var intComma = m.parseShareQuery("?" + m.buildShareQuery({
  dataset: "d", outcome: "o", otype: "continuous", goal: 1, intent: "maximize",
  rows: [{ name: "a", lb: 0, ub: 1, costVec: [0, 1] }],
  interactions: ["a,x:b"],
}));
eq(intComma.interactions, ["a,x:b"], "interaction term with a comma round-trips");

// ---- 5. optional fields omitted: an older/leaner link keeps page defaults ----
var lean = m.parseShareQuery("?" + m.buildShareQuery({
  dataset: "BB_data", outcome: "pp3_oxytocin_mother", otype: "binary",
  goal: 0.85, intent: "maximize",
  rows: [{ name: "coaching_updt", lb: 0, ub: 40, costVec: [0, 1] }],
}));
eq(lean.centerChars, [], "no cc params -> [] (no characteristics restored)");
eq(lean.centerCharValues, [], "no ccval -> [] (values skipped)");
eq(lean.interactions, [], "no int params -> [] (no interactions restored)");
eq(lean.budget, null, "no budget param -> null (page auto-fills its default)");
eq(lean.sweep, { param: null, from: null, to: null, steps: null },
  "no sweep params -> nulls (page keeps its derived defaults)");
// an unset/zero budget is not encoded, so it never overwrites the auto-default
ok(m.buildShareQuery({
  dataset: "d", outcome: "o", otype: "binary", goal: 1, intent: "maximize",
  rows: [{ name: "a", lb: 0, ub: 1, costVec: [0, 1] }], budget: null,
}).indexOf("budget=") === -1, "a null budget is omitted from the query");

// ---- 6. truncated optional fields degrade rather than throw ----
var badCc = m.parseShareQuery("?outcome=o&cc=a&cc=b&ccval=1.5");
eq(badCc.centerChars, ["a", "b"], "both cc names parse");
ok(Number.isFinite(badCc.centerCharValues[0]) && badCc.centerCharValues[1] === undefined,
  "missing second ccval is undefined -> isFinite guard skips it (no forced 0)");
// a trailing-comma / truncated ccval segment is non-finite, NOT a forced 0, so
// applyRestore's isFinite guard leaves that characteristic at its default
var truncCc = m.parseShareQuery("?outcome=o&cc=a&cc=b&ccval=1.5,");
eq(truncCc.centerChars, ["a", "b"], "both cc names parse (trailing comma)");
ok(truncCc.centerCharValues[0] === 1.5 && Number.isNaN(truncCc.centerCharValues[1]),
  "empty ccval segment -> NaN, not a forced 0");
eq(m.parseShareQuery("?outcome=o&budget=abc").budget, null,
  "non-numeric budget -> null (no NaN forced into the field)");
eq(m.parseShareQuery("?outcome=o&sfrom=xyz").sweep.from, null,
  "non-numeric sweep bound -> null (range left at its default)");
// a truncated costs segment degrades to a non-finite vector, so neither
// linearUnitCost nor isDesignerCost accepts it and applyRestore keeps the
// component's default cost instead of silently setting it free (0)
var truncCost = m.parseShareQuery("?components=a&components=b&costs=0,;0,1");
ok(Number.isNaN(truncCost.costs[0][1]), "empty costs coefficient -> NaN, not 0");
eq(m.linearUnitCost(truncCost.costs[0]), null, "a NaN cost vector is not a linear unit cost");
ok(!m.isDesignerCost(truncCost.costs[0]), "a NaN cost vector is not a designer cost (kept default)");
eq(m.linearUnitCost(truncCost.costs[1]), 1, "an intact costs segment still round-trips");
// a cleared sweep/budget field ("") is omitted from the query (Number("")===0
// must not encode a blank field as an explicit 0 that overrides the default)
var blankSweep = m.buildShareQuery({
  dataset: "d", outcome: "o", otype: "binary", goal: 1, intent: "maximize",
  rows: [{ name: "a", lb: 0, ub: 1, costVec: [0, 1] }],
  sweep: { param: "outcome_goal", from: "", to: "", steps: "" }, budget: "",
});
ok(blankSweep.indexOf("sfrom=") === -1 && blankSweep.indexOf("sto=") === -1 &&
  blankSweep.indexOf("ssteps=") === -1, "blank sweep fields are omitted, not encoded as 0");
ok(blankSweep.indexOf("sparam=outcome_goal") !== -1, "a set sweep param still encodes");
ok(blankSweep.indexOf("budget=") === -1, "a blank budget is omitted, not encoded as 0");
// a legitimate sweep value of 0 (a valid continuous goal) still round-trips
eq(m.parseShareQuery("?" + m.buildShareQuery({
  dataset: "d", outcome: "o", otype: "continuous", goal: 0, intent: "minimize",
  rows: [{ name: "a", lb: 0, ub: 1, costVec: [0, 1] }],
  sweep: { param: "outcome_goal", from: 0, to: 2, steps: 5 },
})).sweep.from, 0, "a sweep 'from' of 0 round-trips (not dropped as blank)");

console.log("share-config round-trip: " + pass + " assertions passed");
