# lago-python

A Python wrapper for the [LAGO](https://github.com/correspondMerchant/LAGO-R-Package) R package.

## Honest note (read this first)

**This wrapper EMBEDS R via [rpy2](https://rpy2.github.io/). R and the installed
`LAGO` R package are REQUIRED at runtime.** It does not reimplement any LAGO
math. Every function calls the corresponding exported R function in `LAGO` and
converts inputs/outputs between Python-native types and R objects.

In other words: this gives Python users LAGO's API in Python syntax. It is
**not** an R-free install of LAGO. You must have:

- a working R installation (rpy2 must be able to reach it, i.e. `R_HOME` set),
- the `LAGO` R package installed in that R,
- `rpy2` and `pandas` installed for Python.

`visualize_cost()` **opens a browser and BLOCKS** until you close the app (it
launches the R Shiny + D3 cost-function visualizer, unchanged, R-side).

## Install

```bash
pip install rpy2 pandas
pip install -e python/   # from the repo root, or `pip install .` inside python/
```

Point rpy2 at your R (example for a conda R):

```bash
export R_HOME="$(R RHOME)"
```

## Usage

```python
import pandas as pd
import lago

# `data` is a pandas DataFrame; it is converted to an R data.frame.
result = lago.optimize(
    data=df,
    outcome_name="case",
    outcome_type="binary",
    glm_family="binomial",
    intervention_components=["age", "parity"],
    intervention_lower_bounds=[0, 0],
    intervention_upper_bounds=[50, 10],
    cost_list=[[0, 4], [0, 1]],     # list-of-lists -> R list of numeric vectors
    outcome_goal=0.5,
    outcome_goal_intention="maximize",
    confidence_set_grid_step_size=[1, 1],
    quiet=True,
)

result["rec_int"]           # list[float], the recommended intervention
result["est_outcome_goal"]  # float
result["rec_int_cost"]      # float
result["cs"]                # pandas.DataFrame (the confidence set) or None
result["est_outcome_ci"]    # {"lower": .., "upper": ..} or None
result["model"]             # raw rpy2 glm object (does NOT convert cleanly)
```

Any argument of the R `lago_optimization()` can be passed through as a keyword
argument (e.g. `center_characteristics`, `power_goal`, `link`, `unit_costs`,
`include_confidence_set`).

### Confidence set

```python
cs = lago.get_confidence_set(
    predictors_data=df[["coaching_updt", "launch_duration", "birth_volume_100"]],
    intervention_components=["coaching_updt", "launch_duration"],
    outcome_data=list(df["pp3_oxytocin_mother"]),
    fitted_model=result["model"],      # the raw rpy2 glm object from optimize()
    link="logit",
    outcome_goal=0.85,
    outcome_type="binary",
    intervention_lower_bounds=[1, 1],
    intervention_upper_bounds=[40, 5],
    confidence_set_grid_step_size=[1, 1],
    cost_list=[[0, 1.7], [0, 8]],
    rec_int=result["rec_int"],
)
cs["confidence_set_size_percentage"]  # float
cs["rec_int_ci"]                      # {"lower": .., "upper": ..} or None
cs["cs"]                              # pandas.DataFrame or None
```

### HTML report

```python
path = lago.lago_report(result, output_file="report.html", title="My report")
# -> path to the rendered HTML file (str)
```

`lago_report()` accepts the dict returned by `optimize()` (it reuses the
underlying R object stored under `_r_object`), so the optimization is not
re-run.

### Cost-function visualizer (interactive, blocking)

```python
cost_list = lago.visualize_cost(
    component_names=["Component 1", "Component 2"],
    unit_costs=[0.5, 1.0],
    default_cost_fxn_type="linear",
    intervention_lower_bounds=[0, 0],
    intervention_upper_bounds=[10, 10],
)
# Opens a browser and BLOCKS. Close the app with its
# "Return list to R & close" button to get the cost list back as a
# python list-of-lists (suitable to pass as cost_list= to optimize()).
# Closing the browser tab instead returns nothing.
```

## What does not convert cleanly

- The fitted outcome `model` (an R `glm` object) is returned as the raw rpy2
  object under `result["model"]`, not a Python-native structure. You can pass
  it straight into `get_confidence_set(fitted_model=...)`.
- The whole R result is also kept under `result["_r_object"]` so it can be fed
  to `lago_report()` without re-running the optimization.

## Testing

```bash
cd python && pytest
```

The tests embed R and require the `LAGO` R package to be installed; if R/rpy2/
LAGO cannot be reached the suite skips itself. `visualize_cost()` is **not**
auto-tested because it launches a blocking browser app; only its R-call
construction is checked. Test it interactively by hand.

### Environment note (conda R)

rpy2 loads R's shared library at import. With a conda R you must point rpy2 at
it and, on older host systems, put the conda libs first so R's newer C++
runtime (`libstdc++`, `libicu`) is found before the system one:

```bash
export R_HOME="$(R RHOME)"
export R_LIBS="$R_HOME/library"
export LD_LIBRARY_PATH="$(dirname "$R_HOME")/lib:$LD_LIBRARY_PATH"
```

Under `pytest` this `LD_LIBRARY_PATH` ordering matters: pytest's plugins load
C extensions before rpy2, which can otherwise pin an older system `libstdc++`
and fail with `GLIBCXX_... not found` when R's library is dlopen'd.
```
