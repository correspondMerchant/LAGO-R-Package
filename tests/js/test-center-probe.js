// Checks how the playground decides which centers glm() would leave out
// (centerProbeCode(), probeColumns() and centerLeftOut() in
// pkgdown/assets/playground.html).
//
// Run with:  node tests/js/test-center-probe.js
//
// The package turns a numeric 'period' column into a factor when it is the time
// factor, which keeps NaN as a level and its row in the fit. As a numeric
// component or covariate, a NaN period is dropped by glm(), as is an NA period
// in either role. The R part needs Rscript with jsonlite. It is skipped when
// they are missing, except on CI, where that is a failure.

var assert = require("assert");
var fs = require("fs");
var os = require("os");
var path = require("path");
var execFileSync = require("child_process").execFileSync;

var html = fs.readFileSync(
  path.join(__dirname, "..", "..", "pkgdown", "assets", "playground.html"),
  "utf8"
);
var pass = 0;
function ok(cond, msg) { assert.ok(cond, msg); pass++; console.log("ok  - " + msg); }

// the page's own helpers, taken from its source
function extract(name, pattern) {
  var match = html.match(pattern);
  ok(!!match, "the page defines " + name + "()");
  return match;
}
var rstr = eval(extract("rstr", /const rstr = (\(s\) => [^\n]+);/)[1]);
var centerProbeCode = eval("(" + extract("centerProbeCode", /function centerProbeCode\(src\) \{[\s\S]*?\n    \}/)[0] + ")");
var probeColumns = eval("(" + extract("probeColumns", /function probeColumns\(modelColumns, timeEffects\) \{[\s\S]*?\n    \}/)[0] + ")");
var centerLeftOut = eval("(" + extract("centerLeftOut", /function centerLeftOut\(patterns, used\) \{[\s\S]*?\n    \}/)[0] + ")");

// the columns that count, for period as the time factor and as a component
var asTime = probeColumns(["y", "dose"], true);
var asComponent = probeColumns(["y", "dose", "period"], false);
ok(asTime.has("period") && !asTime.has("period:NaN"), "as the time factor, only an NA period counts");
ok(asComponent.has("period") && asComponent.has("period:NaN"), "as a component, a NaN period counts too");
ok(!probeColumns(["y", "dose"], false).has("period"), "an unused period does not count");
ok(!centerLeftOut([], asComponent), "a center with no patterns is never left out");

var rscript = null;
try {
  execFileSync("Rscript", ["--no-init-file", "-e", "stopifnot(requireNamespace('jsonlite', quietly = TRUE))"], { stdio: "ignore" });
  rscript = "Rscript";
} catch (e) {
  if (process.env.CI) throw new Error("Rscript with jsonlite is needed on CI to run the R probe");
  console.log("skip - Rscript with jsonlite not available, R probe not run");
}

if (rscript) {
  // Center a has one row with a NaN dose. Center b has only a NaN period row.
  // Center c is complete. Center d has only an NA period row. Center e has only
  // a NaN dose row.
  var setup = "PG_DATA <- data.frame(" +
    "center = c('a', 'a', 'b', 'c', 'd', 'e'), " +
    "period = c(1, 2, NaN, 2, NA, 1), " +
    "dose = c(1, NaN, 2, 3, 4, NaN), y = c(0, 1, 0, 1, 1, 0));";
  var file = path.join(fs.mkdtempSync(path.join(os.tmpdir(), "probe-")), "probe.R");
  fs.writeFileSync(file, setup + "cat(" + centerProbeCode("center") + ")");
  var out = JSON.parse(execFileSync(rscript, ["--no-init-file", file], { encoding: "utf8" }));
  assert.deepStrictEqual(out.names, ["a", "b", "c", "d", "e"]);
  pass++;
  var p = out.patterns;
  ok(!centerLeftOut(p[0], asTime), "a center with one complete row is kept");
  ok(!centerLeftOut(p[1], asTime), "a NaN period is kept when period is the time factor");
  ok(centerLeftOut(p[1], asComponent), "a NaN period is dropped when period is a component");
  ok(!centerLeftOut(p[2], asComponent), "a complete center is kept");
  ok(centerLeftOut(p[3], asTime) && centerLeftOut(p[3], asComponent), "an NA period is dropped in either role");
  ok(centerLeftOut(p[4], asTime) && centerLeftOut(p[4], asComponent), "a NaN in another numeric column is dropped");
}

console.log("\n" + pass + " checks passed");
