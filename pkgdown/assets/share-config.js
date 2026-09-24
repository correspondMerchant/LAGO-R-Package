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
// cost, encoded as its full coefficient vector, so both round-trip. The center
// characteristics (one repeated `cc` name each, values comma-joined in `ccval`),
// the interaction terms (one repeated `int` per "a:b" term), the additional
// covariates (one repeated `cov` per column), a user-set `budget`, and the
// sweep settings (`sparam`, `sfrom`, `sto`, `ssteps`) ride along too, so a link
// restores the whole page and not just the core optimization. Each of these is
// optional: an older or truncated link that omits them simply keeps the page's
// own defaults for that control. (Fixed time effects are not carried, since
// they need an uploaded CSV and sharing is offered only for bundled datasets.)
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
    // An empty comma segment becomes NaN, not 0: Number("") is 0, so a truncated
    // or hand-trimmed list ("1.5," -> ["1.5",""]) would otherwise force that
    // position to 0. NaN makes the caller's Number.isFinite guard (bounds/ccval)
    // or its linearUnitCost/isDesignerCost check (costs) skip it, so a missing
    // value keeps the control's default rather than pinning it to 0 (as the
    // "degrades rather than forcing to zero" contract below intends).
    var toNum = function (x) { return x === "" ? NaN : Number(x); };
    var toNums = function (s) { return s ? s.split(",").map(toNum) : []; };
    var costs = q.get("costs");
    // A finite number from a query value, else null: keeps a missing or
    // hand-mangled optional field from forcing a control to NaN or 0.
    var numOrNull = function (s) {
      return s !== null && s !== "" && Number.isFinite(Number(s)) ? Number(s) : null;
    };
    return {
      dataset: q.get("dataset"),
      outcome: q.get("outcome"),
      otype: q.get("otype"),
      components: q.getAll("components"),
      lower: toNums(q.get("lower")),
      upper: toNums(q.get("upper")),
      costs: costs ? costs.split(";").map(function (s) { return s.split(",").map(toNum); }) : [],
      goal: q.get("goal"),
      intent: q.get("intent"),
      centerChars: q.getAll("cc"),
      centerCharValues: toNums(q.get("ccval")),
      interactions: q.getAll("int"),
      covariates: q.getAll("cov"),
      budget: numOrNull(q.get("budget")),
      sweep: {
        param: q.get("sparam"),
        from: numOrNull(q.get("sfrom")),
        to: numOrNull(q.get("sto")),
        steps: numOrNull(q.get("ssteps")),
      },
    };
  }

  // Build the query string (without the leading "?") from a configuration:
  // { dataset, outcome, otype, rows: [{ name, lb, ub, costVec }], goal, intent,
  //   centerChars: [{ name, value }], interactions: ["a:b"], covariates: [name],
  //   budget, sweep: { param, from, to, steps } }.
  // costVec is the component's full coefficient vector ([0, unitCost] for a
  // linear cost). centerChars, interactions, covariates, budget and sweep are
  // optional (omitted params just restore defaults). Inverse of parseShareQuery.
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
    // A finite number, else null. Treats "" (a cleared input) as absent, since
    // Number("") is 0 and would otherwise encode a blank field as an explicit 0
    // that overrides the opener's own default instead of being omitted.
    var numField = function (v) {
      return v !== "" && v != null && Number.isFinite(Number(v)) ? Number(v) : null;
    };
    // center characteristics: a repeated `cc` per name, values comma-joined in
    // `ccval` (same shape as components + lower/upper), only when any are set.
    var ccs = config.centerChars || [];
    if (ccs.length) {
      ccs.forEach(function (c) { p.append("cc", c.name); });
      p.set("ccval", ccs.map(function (c) { return Number(c.value); }).join(","));
    }
    // interaction terms: a repeated `int` per "a:b" term, only when any are set.
    (config.interactions || []).forEach(function (t) { p.append("int", t); });
    // additional covariates: a repeated `cov` per column name.
    (config.covariates || []).forEach(function (name) { p.append("cov", name); });
    // budget only when the user set one; otherwise the page auto-fills its own
    // default from the current costs, which a stale encoded value would defeat.
    if (numField(config.budget) !== null) p.set("budget", numField(config.budget));
    var s = config.sweep;
    if (s) {
      if (s.param) p.set("sparam", s.param);
      if (numField(s.from) !== null) p.set("sfrom", numField(s.from));
      if (numField(s.to) !== null) p.set("sto", numField(s.to));
      if (numField(s.steps) !== null) p.set("ssteps", numField(s.steps));
    }
    return p.toString();
  }

  var api = { linearUnitCost: linearUnitCost, isDesignerCost: isDesignerCost,
    parseShareQuery: parseShareQuery, buildShareQuery: buildShareQuery };

  if (typeof module !== "undefined" && module.exports) module.exports = api;
  else global.ShareConfig = api;
})(typeof window !== "undefined" ? window : this);
