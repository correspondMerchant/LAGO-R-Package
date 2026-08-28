#!/usr/bin/env Rscript
# Build a WebAssembly binary of LAGOtrials and assemble a small CRAN-like
# repository that webR can install from. Run inside the pinned webR toolchain
# container (ghcr.io/r-wasm/webr:v0.6.0) by .github/workflows/webr-repo.yaml.
#
# Why not rwasm::add_pkg("local::.") (the usual one-liner)?
#   add_pkg() and build() both call rwasm's internal prefer_remotes(), which
#   reconciles the resolved packages against a CRAN index. For a package that is
#   NOT on CRAN (LAGOtrials), that one-row lookup gets zero rows and aborts with
#   "`nrow(out)` must equal `1`" before anything is compiled. So we drive the
#   same internal steps add_pkg would run -- resolve, make the source tarball,
#   compile it to Wasm, write the PACKAGES index -- but skip prefer_remotes,
#   which is only needed to pick between competing remote/CRAN versions and is
#   irrelevant for a single local package.
#
# Only LAGOtrials itself is built here. Its dependencies are standard CRAN
# packages that webR already ships as prebuilt Wasm binaries, so the demo page
# installs them from the webR CRAN mirror (https://repo.r-wasm.org) at runtime.
#
# Inputs (bind-mounted by the workflow):
#   /work  a clean checkout of the package source (no .git, no renv)
#   /out   the directory the repository is written into (=> /out/webr-repo)

setwd("/work")
options(warn = 1)

ns <- getNamespace("rwasm")
repo_dir <- "/out/webr-repo"

# webR's Wasm binaries live under bin/emscripten/contrib/<R major.minor>.
rver <- R_system_version(getOption("rwasm.webr_version"))
contrib_bin <- file.path(
  repo_dir, "bin", "emscripten", "contrib",
  paste0(rver$major, ".", rver$minor)
)
contrib_src <- file.path(repo_dir, "src", "contrib")
dir.create(contrib_bin, recursive = TRUE, showWarnings = FALSE)
dir.create(contrib_src, recursive = TRUE, showWarnings = FALSE)

# Resolve the local package (LAGOtrials only; its dependencies come from the
# webR CRAN mirror at install time). This is exactly what rwasm::add_pkg does
# up to, but not including, the failing prefer_remotes() step.
config <- ns$ppm_config
config$dependencies <- FALSE
proposal <- pkgdepends::new_pkg_download_proposal("local::.", config = config)
proposal <- proposal$resolve()
resolution <- proposal$get_resolution()
resolution <- resolution[grepl("^source$", resolution$platform), , drop = FALSE]

row <- resolution[resolution$package == "LAGOtrials", , drop = FALSE]
if (nrow(row) < 1) {
  stop("Could not resolve LAGOtrials from the local source at /work.")
}
row <- row[1, ]

# Make the source tarball, compile it to Wasm, and write the repository index.
source_tarball <- file.path(tempdir(), basename(row$target))
ns$make_remote_tarball(row$package, row$sources[[1]][[1]], source_tarball)
if (!file.exists(source_tarball)) {
  stop("Failed to create the LAGOtrials source tarball.")
}
ns$wasm_build("LAGOtrials", source_tarball, contrib_bin, TRUE)
ns$write_packages(repo_dir)

built <- list.files(contrib_bin, pattern = "^LAGOtrials_.*\\.tgz$")
if (length(built) < 1 || !file.exists(file.path(contrib_bin, "PACKAGES"))) {
  stop("Wasm build did not produce a LAGOtrials binary and PACKAGES index.")
}
cat("Built webR repository:\n")
print(list.files(repo_dir, recursive = TRUE))
