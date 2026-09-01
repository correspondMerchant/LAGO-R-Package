// Guards the live webR demo wiring (pkgdown/assets/live-demo.html) against the
// wasm-repo build workflow (.github/workflows/webr-repo.yaml).
//
// Run with:  node tests/js/test-webr-demo.js
//
// A wasm package binary built against one webR ABI will not load in a different
// webR runtime, so the webR version the page loads from the CDN MUST equal the
// webR Docker image the binary is built with. This test also checks that the
// package repository URL the page installs from matches the folder the workflow
// deploys to, and that the page actually installs LAGOtrials. These are the
// wiring mistakes that would silently break the demo without any R/JS error at
// build time.

var assert = require("assert");
var fs = require("fs");
var path = require("path");

var root = path.join(__dirname, "..", "..");
var workflow = fs.readFileSync(
  path.join(root, ".github/workflows/webr-repo.yaml"),
  "utf8"
);
var passed = 0;
function check(name, cond) {
  if (!cond) {
    console.error("FAIL: " + name);
    process.exit(1);
  }
  passed++;
  console.log("ok  - " + name);
}

// webR version the wasm binary is built with (ghcr.io/r-wasm/webr:vX.Y.Z).
var buildMatch = workflow.match(/ghcr\.io\/r-wasm\/webr:v(\d+\.\d+\.\d+)/);
check("workflow pins a webr-image version", !!buildMatch);
var buildVersion = buildMatch && buildMatch[1];

// The repo folder the workflow deploys to (target-folder).
var targetMatch = workflow.match(/target-folder:\s*([^\s#]+)/);
check("workflow declares a deploy target-folder", !!targetMatch);
var targetFolder = targetMatch && targetMatch[1].trim();

// Both webR pages must agree with the build on the ABI-critical version and
// install LAGOtrials from the deployed repo.
["live-demo.html", "playground.html"].forEach(function (name) {
  var page = fs.readFileSync(path.join(root, "pkgdown/assets", name), "utf8");

  var constMatch = page.match(/WEBR_VERSION\s*=\s*"(\d+\.\d+\.\d+)"/);
  check(name + ": declares a WEBR_VERSION", !!constMatch);
  var pageVersion = constMatch && constMatch[1];

  // The CDN import URL is built from WEBR_VERSION, so pinning is single-sourced.
  check(
    name + ": imports webr.mjs pinned to WEBR_VERSION",
    page.indexOf("webr.r-wasm.org/v${WEBR_VERSION}/webr.mjs") !== -1
  );

  // The ABI lock: build image version == page runtime version.
  check(
    name + ": webR version (" + pageVersion + ") == build (" + buildVersion + ")",
    buildVersion === pageVersion
  );

  check(name + ": installs LAGOtrials", /webr::install\("LAGOtrials"/.test(page));

  check(
    name + ": install URL points at the deployed /" + targetFolder + " repo",
    page.indexOf("/" + targetFolder) !== -1 &&
      /correspondmerchant\.github\.io\/LAGO-R-Package\//.test(page)
  );
});

console.log("\nAll " + passed + " webR demo wiring assertions passed.");
