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
# A list argument can also be a numpy array, a pandas Series or a pandas
# Index, such as df.columns[1:3], and its elements numpy numbers.

result["rec_int"]           # list[float], the recommended intervention
result["est_outcome_goal"]  # float
result["rec_int_cost"]      # float
result["cs"]                # pandas.DataFrame (the confidence set), or None
# The index of cs holds the number of each row in the grid of interventions R
# searched, as text, such as "11". Use cs.reset_index(drop=True) for the index 0, 1, 2 and so on.
result["est_outcome_ci"]    # {"lower": .., "upper": ..}, or None
# Both are None when no confidence set is computed, which also happens when the
# goal cannot be reached and result["shrinking_method_used"] is True.
result["model"]             # raw rpy2 glm object (does NOT convert cleanly)
```

Any argument of the R `lago_optimization()` can be passed through as a keyword
argument (e.g. `center_characteristics`, `power_goal`, `link`, `unit_costs`,
`include_confidence_set`).

### Confidence set

`optimize()` already returns the confidence set. To build one on its own, give
`get_confidence_set()` the model of a fit on the same data, here `bb` with the
BetterBirth example, and every model predictor in `predictors_data`:

```python
fit = lago.optimize(
    data=bb,
    outcome_name="pp3_oxytocin_mother",
    outcome_type="binary",
    intervention_components=["coaching_updt", "launch_duration"],
    intervention_lower_bounds=[1, 1],
    intervention_upper_bounds=[40, 5],
    center_characteristics=["birth_volume_100"],
    center_characteristics_optimization_values=[3],
    cost_list=[[0, 1.7], [0, 8]],
    outcome_goal=0.85,
    include_confidence_set=False,
)
cs = lago.get_confidence_set(
    predictors_data=bb[["coaching_updt", "launch_duration", "birth_volume_100"]],
    intervention_components=["coaching_updt", "launch_duration"],
    center_characteristics=["birth_volume_100"],
    center_characteristics_optimization_values=[3],
    outcome_data=list(bb["pp3_oxytocin_mother"]),
    fitted_model=fit["model"],         # the raw rpy2 glm object from optimize()
    link="logit",
    outcome_goal=0.85,
    outcome_type="binary",
    intervention_lower_bounds=[1, 1],
    intervention_upper_bounds=[40, 5],
    confidence_set_grid_step_size=[1, 1],
    cost_list=[[0, 1.7], [0, 8]],
    rec_int=fit["rec_int"],
)
cs["confidence_set_size_percentage"]  # float
cs["rec_int_ci"]                      # {"lower": .., "upper": ..} or None
cs["cs"]                              # pandas.DataFrame or None
```

`cost_list` (or `cost_list_of_vectors`) and `rec_int` are required.

### HTML report

```python
path = lago.lago_report(result, output_file="report.html", title="My report")
# -> path to the rendered HTML file (str)
```

`lago_report()` accepts the dict returned by `optimize()` (it reuses the
underlying R object stored under `_r_object`), so the optimization is not
re-run.

### Sensitivity

```python
sweep = lago.sensitivity(
    data=df,
    outcome_name="pp3_oxytocin_mother",
    outcome_type="binary",
    intervention_components=["coaching_updt", "launch_duration"],
    intervention_lower_bounds=[1, 1],
    intervention_upper_bounds=[40, 5],
    cost_list=[[0, 1700], [0, 8000]],
    parameter="outcome_goal",
    values=[0.8, 0.85, 0.9],
)
sweep  # pandas.DataFrame: value, one column per component, rec_int_cost,
       # est_outcome_goal, status, shrinking_method_used, effective_outcome_goal,
       # error_message
```

With `parameter="cost_multiplier"`, each run multiplies every coefficient of
`cost_list` by one of the `values`, or, when the costs are given as
`unit_costs`, multiplies the unit costs, which scales the cost built from them
in the same way. The values must be above 0.

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

When an R function fails, the function first issues the warnings R gave before
the error, and then raises the error.

Arguments can be plain Python values, numpy values, or pandas values. A pandas
Series, Index or array (such as the result of `Series.unique()`, or an `Int64`
array) becomes a list. A pandas Series of numbers keeps its index as names,
unless the index is a `RangeIndex` (the default positions), so bounds such as
`df[["gear", "qsec"]].max()` and center weights such as
`pd.Series({"north": 0.6, "south": 0.4})` are matched to the components or
the centers by name. A dict of numbers, such as `{"gear": 5, "qsec": 20}`, is
a named vector too, and a dict of lists, such as the cost list
`{"gear": [0, 4], "qsec": [0, 6]}`, is a named list, which R also matches to
the components by name. A name that is a number, such as a center 1.0 in the
index of `df.groupby("center").size()`, is written as R writes the level of
that center, so 1.0 is `"1"` and 100000.0 is `"1e+05"`. When R refuses the
names, give the values as a plain list instead. A missing value (`None`, `pd.NA` or `NaT`) in a list
becomes the R `NA` of the type of the other elements. A categorical value
becomes its text, and a date or a time becomes the text R gives for it, such as
`"2020-01-01"` or `"2020-01-01 10:30:00.5"` (a fraction of a second is kept
to the microsecond), so it names the same period as a date column of the data.
A pandas `Period` becomes its label, such as `"2020Q1"`. When the `period`
column of the data holds times with a time zone, a time with a time zone given
as a period is first moved to the zone of that column, so the same instant
names the same period. A 0-d numpy array is the value it holds, and a time
span (`timedelta`, `pd.Timedelta` or `np.timedelta64`) is its number of days,
as in a data frame column. A
float NaN in a list of text, or of true and false, is a missing value too. A
missing value on its own becomes R's `NA`, and a `Decimal` becomes a number. A
value that cannot be converted, such as a set or a dict of text, raises a
`TypeError` that names its type, and for a list the types of its elements. In
a data frame that comes back from R, a missing text value is `None`. A named R
vector or list comes back as a dict, unless a name is empty, `NA` or used
twice. Then it comes back as a list of `(name, value)` pairs, so no element is
lost, with `""` for an empty name and `None` for an `NA` name.

Each data frame passed to R is prepared first, without changing your copy. A
column of dates or times (`datetime.date`, `datetime.datetime` or numpy
datetimes) becomes the same text, so a date names its period. A time with a time
zone is written in the zone of its column, and in a column of times in more
than one zone, in the zone of the first. A whole number column with a value
too large for R's integers becomes a float column, as R stores such numbers as
doubles. An unsigned `uint32` or `uint64` column becomes an integer column
when its values fit R's integers. A column of Python objects that are all
numbers, such as `[1, None, 3]` or numpy numbers after `astype(object)`,
becomes a float column with R's `NA` where a value is missing. A missing value
(NaN) in a float column becomes R's `NA`, not `NaN`. A categorical column whose
categories are numbers or dates becomes a factor whose levels are the text of
the categories, in the same order, with dates written as above. A text column with missing values, written as
`None`, NaN or `pd.NA`, becomes text with R's `NA`. A column of true and false
values with missing values, as `pd.read_csv()` gives it, becomes a logical
column with R's `NA` where a value is missing. A column of `Decimal` or
`Fraction` numbers becomes a float column too, and a sparse column is made
dense. A
column of time spans (`timedelta`) becomes the number of days as a float, so
36 hours is 1.5. A column of pandas `Period` values becomes the label of each
period.

The data frames that `lago.sensitivity()` and the `frontier` of `lago.budget()`
return have the default index 0, 1, 2 and so on. In a sensitivity result, the
`error_message` of each failed run is the error of that run, and a run that
did not fail has `None`. `n_grid` of `lago.budget()` takes a whole number, such
as 25, 25.0 or `Decimal("25")`.

The `object` argument of R's `lago_sensitivity()` and `lago_budget()`, which
reuses the arguments of an earlier `lago_optimization()` result, is not
supported. Pass the optimization arguments to `lago.sensitivity()` and
`lago.budget()` directly.

Every function works from any thread or asyncio task. rpy2 keeps its conversion
rules per context, so each function sets them for its own call, and a lock lets
one call into R at a time.

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

- **`describe_data`** lists the columns of a dataset with their type, missing
  values, number of distinct values, whether they are 0/1, and the range of
  each numeric column. An agent should call it first, to choose the components,
  their bounds, and any center characteristic value from the data.
- **`optimize`** runs one LAGO optimization. It returns `rec_int`, `rec_int_cost`,
  `est_outcome_goal`, `shrinking_method_used`, `effective_outcome_goal` with a
  power goal, and,
  when a confidence set is computed, `est_outcome_ci`, `confidence_level`,
  `confidence_set_size_percentage` and `confidence_set`.
- **`sensitivity`** sweeps one input (`parameter` and `values`) and returns
  `{"runs": [...], "warnings": [...]}`, with one record in `runs` per swept value
  (`value`, the recommended value per component, `rec_int_cost`,
  `est_outcome_goal`, `status`, `shrinking_method_used`, `effective_outcome_goal`
  and `error_message`). `parameter` is a scalar numeric optimization argument,
  such as `"outcome_goal"`, or the special `"cost_multiplier"`.
- **`budget`** finds the best outcome a `budget` can buy, with `n_grid` outcome
  goals to try (default 25). It returns `feasible`, `binding` (whether a better
  outcome would cost more), `rec_int`, `rec_int_cost`, `est_outcome`,
  `outcome_goal`, `frontier` (the goals searched) and `run_error`.

The model tools take the arguments of `lago_optimization()`:

- the data, as EXACTLY ONE of `data_csv` (a path to a CSV file) or
  `data_records` (a list of row dicts).
- the costs, as EXACTLY ONE of `cost_list_of_vectors` or `unit_costs` (with
  `default_cost_fxn_type` and `unit_cost_doses`).
- the goals, the power goal settings, the model (`glm_family`, `link`,
  `center_characteristics`, `additional_covariates`, center and time effects,
  center weights, interactions, `input_data_structure`), the optimizer, the
  confidence level and set, and the previous stage.

`optimize` takes all of them. `sensitivity` takes all but the confidence set
arguments, as it does not compute one. `budget` takes all but the goals, which
it sweeps, the power goal, the confidence set and the previous stage.

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
`{"error": "...", "warnings": [...]}` with the package's own message, as a tool
result marked as an error (`isError` true), so a client can tell it from a
result. Its `warnings` holds the warnings R gave before the error. An argument
that does not fit the tool gives the same kind of error result: one of the
wrong type, a missing required one, or one the tool does not take (the schema
of each tool says `additionalProperties: false`). So does a call to a tool the
server does not have. A `sensitivity` sweep where every run failed is an error
result too, with the message of its first run and its `runs`.

`time_effect_optimization_value` names one period by its number or by its
label, such as `"2024-Q2"`, and `center_effects_optimization_values` names one
center by its label or by its number, which the server passes on as text, as
R writes it. Neither takes `true` or `false`.
`center_weights_for_outcome_goal` takes an object of one weight per center,
such as `{"north": 0.6, "south": 0.4}`, which is matched to the centers by
name. It also takes a list, which is matched to the centers in the order R
sorts them: numbers from small to large, and text in the sort order of R's
locale, which can differ from a plain sort of the characters. A weight, and a
value of the `weights_column`, must be a number, not `true` or `false`.
`confidence_level` is `1 - confidence_set_alpha`, rounded to 12
significant digits, so an alpha of 0.07 gives 0.93.

JSON has no NaN or Infinity. The MCP SDK turns a NaN or an Infinity into null
before a tool sees it, so the server cannot tell it from null. For an optional
argument, such as `confidence_set_alpha`, null means the argument was not
given, and the default is used. For a required number, or a number in a list,
null is an error, and its message says that a NaN or an Infinity would have
become null, in case the client sent one. So does the error R gives when
neither goal is given, when the call sent `outcome_goal` or `power_goal` as
null, and so does the `error_message` of each sensitivity run that failed for
that reason.

The server also sends the agent short instructions on how to use the tools.
They say to describe the data first, to report every warning, and to read an
interval from 0 to 1, or `shrinking_method_used: true`, as the data not
supporting the goal.

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
your R as in the environment note below (`R_HOME`, `R_LIBS`, `LD_LIBRARY_PATH`).

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
