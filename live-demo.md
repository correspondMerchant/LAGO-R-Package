This runs the real `LAGOtrials` R package entirely in your browser, with
no install and no server, using
[webR](https://docs.r-wasm.org/webr/latest/) (R compiled to
WebAssembly). Edit the R code below and press **Run**. The first run
downloads R and the package, so give it a moment.

Starting webR…

R code

Run

Reset example

## Output

``` out
```

webR runs a WebAssembly build of R (version 4.6.0) fully client-side.
The interactive Shiny app behind `visualize_cost()` is not available
here; everything else works as it does locally. Package binaries are
served from this site's `/webr-repo` repository. See the [package
documentation](https://correspondmerchant.github.io/LAGO-R-Package/index.md)
for the full API.
