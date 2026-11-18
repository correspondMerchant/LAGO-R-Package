# lago-python

A Python wrapper for the [LAGOtrials](https://github.com/correspondMerchant/LAGO-R-Package) R package.

## Honest note (read this first)

**This wrapper EMBEDS R via [rpy2](https://rpy2.github.io/). R and the installed
`LAGOtrials` R package are REQUIRED at runtime.** It does not reimplement any
LAGOtrials math. Every function calls the corresponding exported R function in
`LAGOtrials` and converts inputs/outputs between Python-native types and R
objects.

In other words: this gives Python users LAGOtrials's API in Python syntax. It is
**not** an R-free install of LAGOtrials. You must have:

- a working R installation (rpy2 must be able to reach it, i.e. `R_HOME` set),
- the `LAGOtrials` R package installed in that R, from
  [CRAN](https://CRAN.R-project.org/package=LAGOtrials) with
  `install.packages("LAGOtrials")` or the development version from GitHub,
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

### Budget

```python
best = lago.budget(
    data=df,
    outcome_name="pp3_oxytocin_mother",
    outcome_type="binary",
    intervention_components=["coaching_updt", "launch_duration"],
    intervention_lower_bounds=[1, 1],
    intervention_upper_bounds=[40, 5],
    cost_list=[[0, 1700], [0, 8000]],
    budget=30000,
)
best["feasible"], best["rec_int"], best["est_outcome"]
best["frontier"]   # pandas.DataFrame of the outcome goals searched
```

### Warnings of the R package

Each function issues the warnings of the R package as a `lago.LagoRWarning`, so
they can be read like any Python warning:

```python
import warnings

with warnings.catch_warnings(record=True) as caught:
    warnings.simplefilter("always")
    result = lago.optimize(data=df, ...)
messages = [str(w.message) for w in caught if issubclass(w.category, lago.LagoRWarning)]
```

Every function works from any thread or asyncio task. rpy2 keeps its conversion
rules per context, so each function sets them for its own call.

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

## MCP server

An optional [Model Context Protocol](https://modelcontextprotocol.io/) server
(`lago.mcp_server`) exposes LAGOtrials as callable tools so any MCP-aware agent (Claude
Desktop, Claude Code, ...) can run optimizations. It is a thin front end over the
`lago` wrapper, so it adds ZERO impact to the R package and does no LAGOtrials math of
its own.

**Honest caveat:** because it reuses the `lago` wrapper, the MCP server EMBEDS R
via rpy2. A working R installation and the `LAGOtrials` R package are REQUIRED at
runtime, exactly as for the wrapper. It is **not** an R-free install.

### Tools exposed

- **`describe_data`** — lists the columns of a dataset with their type, missing
  values, number of distinct values, whether they are 0/1, and the range of
  each numeric column. An agent should call it first, to choose the components,
  their bounds, and any center characteristic value from the data.
- **`optimize`** — runs one LAGO optimization. Returns `rec_int`, `rec_int_cost`,
  `est_outcome_goal`, `effective_outcome_goal`, `shrinking_method_used`, and,
  when a confidence set is computed, `est_outcome_ci`, `confidence_level`,
  `confidence_set_size_percentage` and `confidence_set`.
- **`sensitivity`** — sweeps one input (`parameter` + `values`) and returns
  `{"runs": [...], "warnings": [...]}`, with one record in `runs` per swept value
  (`value`, the recommended value per component, `rec_int_cost`,
  `est_outcome_goal`, `status`). `parameter` is a scalar numeric optimization
  argument (e.g. `"outcome_goal"`) or the special `"cost_multiplier"`.
- **`budget`** — finds the best outcome a `budget` can buy, with `n_grid` outcome
  goals to try (default 25). Returns `feasible`, `binding` (whether a better
  outcome would cost more), `rec_int`, `rec_int_cost`, `est_outcome`,
  `outcome_goal`, `frontier` (the goals searched) and `run_error`.

The three model tools take the same arguments, which cover every argument of
`lago_optimization()` a request can carry:

- the data, as EXACTLY ONE of `data_csv` (a path to a CSV file) or
  `data_records` (a list of row dicts);
- the costs, as EXACTLY ONE of `cost_list_of_vectors` or `unit_costs` (with
  `default_cost_fxn_type` and `unit_cost_doses`);
- the goals, the power goal settings, the model (`glm_family`, `link`,
  `center_characteristics`, `additional_covariates`, center and time effects,
  center weights, interactions, `input_data_structure`), the optimizer, the
  confidence level and set, and the previous stage.

The package reads some columns by a fixed name. The tools build them from
columns the caller names, as `lago_app()` does, so an agent does not have to
prepare the data:

| Argument | Builds |
|---|---|
| `group_column` with `treatment_value` | `group` (`"treatment"` or `"control"`), for a power goal |
| `center_column` | `center`, for center effects |
| `period_column` | `period`, for time effects |
| `successes_column` or `proportion_column`, with `total_column` | `proportion` and `center_sample_size`, for center level data |
| `weights_column` | the `weights` vector |

Every tool result has a `warnings` list with the warnings of the R package, each
once, such as a recommendation for a center outside the centers in the data. An
agent should pass them on with the numbers. A tool that fails returns
`{"error": "..."}` with the package's own message, and `warnings` when there
were any.

The server also sends the agent short instructions on how to use the tools:
describe the data first, report every warning, and read an interval from 0 to
1 or `shrinking_method_used: true` as the data not supporting the goal.

`visualize_cost` and `lago_app` (each opens a blocking browser app) and
`lago_report` (writes an HTML file) are intentionally **not** exposed: none of them
maps cleanly onto a request/response tool call.

### Install

```bash
# base install + the mcp extra (from the repo root)
pip install -e "python[mcp]"
# or, inside python/: pip install -e ".[mcp]"
```

R and the `LAGOtrials` R package must be installed (rpy2 embeds R). Point rpy2 at
your R as in the environment note above (`R_HOME`, `R_LIBS`, `LD_LIBRARY_PATH`).

### Run

```bash
python -m lago.mcp_server      # or the console script: lago-mcp
```

The server speaks the stdio transport (FastMCP's default), so an MCP client
launches it as a subprocess and talks JSON-RPC over stdin/stdout.

### Client config

Add a stanza like this to your MCP client's config (for example Claude Desktop's
`claude_desktop_config.json`, or a Claude Code `.mcp.json`). Set the `env` so
rpy2 can find your R (adjust the paths to your R install):

```json
{
  "mcpServers": {
    "lago": {
      "command": "python",
      "args": ["-m", "lago.mcp_server"],
      "env": {
        "R_HOME": "/path/to/R/lib/R",
        "R_LIBS": "/path/to/R/lib/R/library",
        "LD_LIBRARY_PATH": "/path/to/R/lib"
      }
    }
  }
}
```

Point `command` at the Python interpreter that has `lago-python[mcp]` installed
(use its absolute path, e.g. a venv/conda `python`, if it is not on the client's
`PATH`).

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

The tests embed R and require the `LAGOtrials` R package to be installed; if
R/rpy2/LAGOtrials cannot be reached the suite skips itself. `visualize_cost()` is **not**
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
