// Pure encode/decode for the playground's "Share link" feature: it turns the
// current configuration into URL query params and back, so a link restores the
// whole setup on load. Kept free of the DOM (playground.html reads/writes the
// controls; this only shapes the query string and classifies cost vectors) so
// the round-trip is unit-testable under node (tests/js/test-share-config.js).
//
// Encoding: dataset, outcome, otype, one repeated `components` per component,
// `lower`/`upper` comma-joined, `costs` a ";"-separated list of per-component
// coefficient vectors (comma within), plus goal and intent. A component's cost
// is either a linear unit cost, encoded as the vector c(0, x), or a designer
// cost, encoded as its full coefficient vector, so both round-trip.
(function (global) {
  "use strict";

  // The linear-cost representation is exactly the two-coefficient vector
  // [0, unitCost]. Return that unit cost when `vec` is one, else null. Any
  // vector with a non-zero intercept or a different length is a designer cost.
  function linearUnitCost(vec) {
    return Array.isArray(vec) && vec.length === 2 && vec[0] === 0 && Number.isFinite(vec[1])
      ? vec[1]
      : null;
  }

  // A usable designer cost: a non-empty, all-finite coefficient vector. (A
  // linear [0, x] also satisfies this, but callers check linearUnitCost first.)
  function isDesignerCost(vec) {
    return Array.isArray(vec) && vec.length > 0 && vec.every(function (x) { return Number.isFinite(x); });
  }

  // Parse a page's location.search into a restore descriptor, or null when the
  // query carries no configuration. A missing numeric field becomes an empty
  // array (rather than [0]), so its per-component entries are undefined and the
  // caller's isFinite guard skips them: a truncated or hand-edited link degrades
  // rather than forcing a bound or cost to zero or throwing.
  function parseShareQuery(search) {
    var q = new URLSearchParams(search || "");
    if (!q.has("outcome") && !q.has("components")) return null;
    var toNums = function (s) { return s ? s.split(",").map(Number) : []; };
    var costs = q.get("costs");
    return {
      dataset: q.get("dataset"),
      outcome: q.get("outcome"),
      otype: q.get("otype"),
      components: q.getAll("components"),
      lower: toNums(q.get("lower")),
      upper: toNums(q.get("upper")),
      costs: costs ? costs.split(";").map(function (s) { return s.split(",").map(Number); }) : [],
      goal: q.get("goal"),
      intent: q.get("intent"),
    };
  }

  // Build the query string (without the leading "?") from a configuration:
  // { dataset, outcome, otype, rows: [{ name, lb, ub, costVec }], goal, intent }.
  // costVec is the component's full coefficient vector ([0, unitCost] for a
  // linear cost). Inverse of parseShareQuery.
  function buildShareQuery(config) {
    var p = new URLSearchParams();
    p.set("dataset", config.dataset);
    p.set("outcome", config.outcome);
    p.set("otype", config.otype);
    config.rows.forEach(function (r) { p.append("components", r.name); });
    p.set("lower", config.rows.map(function (r) { return Number(r.lb); }).join(","));
    p.set("upper", config.rows.map(function (r) { return Number(r.ub); }).join(","));
    p.set("costs", config.rows.map(function (r) { return r.costVec.map(Number).join(","); }).join(";"));
    p.set("goal", Number(config.goal));
    p.set("intent", config.intent);
    return p.toString();
  }

  var api = { linearUnitCost: linearUnitCost, isDesignerCost: isDesignerCost,
    parseShareQuery: parseShareQuery, buildShareQuery: buildShareQuery };

  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else global.ShareConfig = api;
})(typeof window !== "undefined" ? window : this);
