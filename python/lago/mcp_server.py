"""MCP (Model Context Protocol) server exposing LAGOtrials as callable tools.

This server lets any MCP-aware agent run LAGO optimizations without writing any
code. It is a thin front end over the existing :mod:`lago` Python wrapper (which
itself bridges to the ``LAGOtrials`` R package via rpy2), so it performs NO LAGO
math of its own and adds ZERO impact to the R package: everything lives under
``python/``.

Because it reuses :mod:`lago`, this server EMBEDS R via rpy2. A working R
installation and the ``LAGOtrials`` R package are REQUIRED at runtime. It is not
an R-free install. See ``python/README.md``.

Tools exposed
-------------
``describe_data`` -> the columns of a dataset, to choose the arguments from
``optimize``      -> :func:`lago.optimize`   (a single LAGO optimization)
``sensitivity``   -> :func:`lago.sensitivity` (sweep one input, report the move)
``budget``        -> :func:`lago.budget`      (the best outcome a budget buys)

The model tools take the model arguments of ``lago_optimization()``:
``optimize`` takes all of them, ``sensitivity`` all but the confidence set ones,
which it does not compute, and ``budget`` all but the goals, which it sweeps,
the confidence set and the previous stage. The columns the
package reads by a fixed name (``group``, ``center``, ``period``, and for
center level data ``proportion`` and ``center_sample_size``) can be built from
columns the caller names, as ``lago_app()`` does.

Every result has a ``warnings`` list with the warnings of the R package, such as
a recommendation for a center outside the centers in the data, so an agent can
pass them on instead of reporting a number without its caveats. A tool that
fails returns ``{"error": "...", "warnings": [...]}`` as a tool result marked
as an error (``isError`` true).

Deliberately NOT exposed
------------------------
``visualize_cost`` and ``lago_app`` open interactive browser apps and BLOCK
until the user closes them, which is a poor fit for an autonomous tool call.
``lago_report`` writes an HTML file to disk as a side effect. None of them maps
cleanly onto a request/response tool, so they are intentionally left out.

Run it
------
``python -m lago.mcp_server`` (or the ``lago-mcp`` console script). FastMCP's
``run()`` uses the stdio transport by default, so an MCP client launches this as
a subprocess and speaks JSON-RPC over stdin/stdout.
"""

from __future__ import annotations

import functools
import json
import math
from typing import Any

import anyio

from mcp.server.fastmcp import FastMCP

import lago
from lago._bridge import collect_warnings

INSTRUCTIONS = """\
Tools for LAGO (Learn-As-you-GO) adaptive trials, from the LAGOtrials R package.
They fit an outcome model to the data of the stages so far and recommend the
cheapest intervention package for the next stage that is expected to reach an
outcome goal or a power goal.

How to use them well:
1. Call describe_data first. Choose the outcome, the components and their
   bounds, and any center characteristic value, from the columns and ranges it
   reports. A characteristic value outside the data is an extrapolation.
2. Call optimize for the recommendation. Use sensitivity to show how it moves
   when one input changes, and budget to find the best outcome a budget buys.
3. Report every entry of `warnings` together with the numbers. They are the
   package's own caveats, such as a model component with no clear effect or a
   recommendation outside the data. Do not report a number without them.
4. On the logit link, a binary outcome's estimated outcome and interval are
   probabilities, and the interval is cut off at 0 and 1. An interval from 0
   to 1 means the data cannot pin the outcome down there. On the identity
   link the estimate can leave 0 to 1, and the warnings say so.
5. `shrinking_method_used: true` means the goal cannot be reached within the
   bounds, and the recommendation falls short of it. Say so.
"""

mcp = FastMCP("lago", instructions=INSTRUCTIONS)


# --------------------------------------------------------------------------
# Shared helpers
# --------------------------------------------------------------------------
def _resolve_data(data_csv, data_records):
    """Return a pandas DataFrame from EXACTLY ONE of the two data inputs.

    Exactly one of ``data_csv`` (a path to a CSV file) or ``data_records`` (a
    list of row dicts) must be supplied; giving both or neither raises a clear
    ``ValueError`` naming the problem.
    """
    import pandas as pd

    given = [
        name
        for name, val in (("data_csv", data_csv), ("data_records", data_records))
        if val is not None
    ]
    if not given:
        raise ValueError(
            "provide exactly one of `data_csv` (a path to a CSV file) or "
            "`data_records` (a list of row dicts); neither was given."
        )
    if len(given) == 2:
        raise ValueError(
            "provide exactly one of `data_csv` or `data_records`, not both; "
            "both were given."
        )
    if data_csv is not None:
        return pd.read_csv(data_csv)
    if not isinstance(data_records, list) or not data_records:
        raise ValueError("`data_records` must be a non-empty list of row dicts.")
    return pd.DataFrame.from_records(data_records)


def _column(df, name, role):
    if name not in df.columns:
        raise ValueError(f"the {role} column {name!r} is not a column of the data.")
    return df[name]


def _arm(arm, name, treatment_value):
    """The group column the package reads: "treatment" or "control" per row.

    A number and a text value are compared as the column holds them, so a
    treatment value of 1 matches a column of 1s whether it arrives as 1, 1.0 or
    "1". A row with no arm is refused, as R refuses it.
    """
    import numpy as np
    import pandas as pd

    if treatment_value is None:
        raise ValueError("`group_column` needs `treatment_value`, the value that marks treated rows.")
    missing = int(arm.isna().sum())
    if missing:
        raise ValueError(f"the group column {name!r} has {missing} rows with no value. Give every row an arm, or leave those rows out.")
    if pd.api.types.is_bool_dtype(arm):
        # JSON true arrives as 1.0 through the schema, and a CSV gives TRUE
        text = str(treatment_value).strip().lower()
        if text not in ("true", "false", "1", "0", "1.0", "0.0"):
            raise ValueError(f"`treatment_value` {treatment_value!r} is not true or false, while the group column {name!r} holds true and false.")
        treated = arm == (text in ("true", "1", "1.0"))
    elif pd.api.types.is_numeric_dtype(arm):
        try:
            treated = arm.astype(float) == float(treatment_value)
        except (TypeError, ValueError):
            raise ValueError(f"`treatment_value` {treatment_value!r} is not a number, while the group column {name!r} holds numbers.") from None
    else:
        wanted = {str(treatment_value)}
        if isinstance(treatment_value, float) and treatment_value.is_integer():
            wanted.add(str(int(treatment_value)))
        treated = arm.astype(str).isin(wanted)
    values = sorted({str(v) for v in arm.unique()})
    if not treated.any():
        raise ValueError(f"`treatment_value` {treatment_value!r} is not a value of the group column {name!r}, whose values are {', '.join(values[:10])}.")
    if treated.all():
        raise ValueError(f"every row of the group column {name!r} has the treatment value, so there is no control arm.")
    return pd.Series(np.where(treated, "treatment", "control"), index=arm.index)


def _prepare_data(df, p):
    """Build the columns the package reads by a fixed name, from named columns.

    Returns the data and the outcome name to use. A column the package reads is
    only replaced when the caller names a different column for it.
    """
    import pandas as pd

    df = df.copy()
    outcome_name = p["outcome_name"]
    if p.get("group_column"):
        df["group"] = _arm(_column(df, p["group_column"], "group"), p["group_column"], p.get("treatment_value"))
    # copied as they are, so R orders numbered centers and periods as numbers,
    # as it does for a column already named center or period. A pandas
    # Categorical would reach R as text, ordering 10 before 2.
    if p.get("center_column") and p["center_column"] != "center":
        df["center"] = _column(df, p["center_column"], "center").to_numpy()
    if p.get("period_column") and p["period_column"] != "period":
        df["period"] = _column(df, p["period_column"], "period").to_numpy()
    # an interaction term "a:b" is the product of its two components
    for term in p.get("intervention_components") or []:
        parts = term.split(":")
        if len(parts) > 1 and term not in df.columns and all(c in df.columns for c in parts):
            product = df[parts[0]].astype(float)
            for c in parts[1:]:
                product = product * df[c].astype(float)
            df[term] = product
    center_level_columns = [n for n in ("successes_column", "proportion_column", "total_column") if p.get(n)]
    if center_level_columns and p.get("input_data_structure") != "center_level":
        raise ValueError(
            f"{', '.join('`' + n + '`' for n in center_level_columns)} describe center level data. "
            'Also give `input_data_structure="center_level"`.'
        )
    if p.get("input_data_structure") == "center_level":
        total = p.get("total_column")
        if p.get("successes_column"):
            if not total:
                raise ValueError("`successes_column` needs `total_column`, the number of patients of each row.")
            df["proportion"] = _column(df, p["successes_column"], "successes") / _column(df, total, "total")
        else:
            # without either column, the outcome named is the proportion
            share = p.get("proportion_column") or outcome_name
            if share != "proportion":
                df["proportion"] = _column(df, share, "proportion")
        if total and total != "center_sample_size":
            df["center_sample_size"] = _column(df, total, "total")
        outcome_name = "proportion"
    return df, outcome_name


def _model_kwargs(p, df):
    """The lago_optimization() arguments of a tool call, from its parameters.

    An argument left at None keeps the package default. Exactly one of the cost
    list and the unit costs must be given.
    """
    if (p.get("cost_list_of_vectors") is None) == (p.get("unit_costs") is None):
        raise ValueError("give exactly one of `cost_list_of_vectors` or `unit_costs`.")
    # a JSON NaN or Infinity would otherwise reach R as an unclear error
    for name in ("intervention_lower_bounds", "intervention_upper_bounds", "unit_costs", "cost_list_of_vectors",
                 "outcome_goal", "power_goal", "budget"):
        value = p.get(name)
        flat = [x for v in value for x in (v if isinstance(v, (list, tuple)) else [v])] if isinstance(value, (list, tuple)) else [value]
        if any(isinstance(x, float) and not math.isfinite(x) for x in flat if x is not None):
            raise ValueError(f"`{name}` must hold finite numbers, with no NaN or Infinity.")
    names = [
        "outcome_goal_intention", "input_data_structure", "power_goal", "num_centers_in_next_stage",
        "patients_per_center_in_next_stage", "power_goal_approach", "icc", "power_goal_cluster_id",
        "unit_costs", "default_cost_fxn_type", "unit_cost_doses", "center_characteristics",
        "center_characteristics_optimization_values", "additional_covariates", "include_center_effects",
        "center_effects_optimization_values", "center_weights_for_outcome_goal", "include_time_effects",
        "time_effect_optimization_value", "include_interaction_terms", "main_components",
        "optimization_method", "optimization_grid_search_step_size", "prev_recommended_interventions",
    ]
    extra = {n: p[n] for n in names if p.get(n) is not None}
    for n in ("glm_family", "link"):
        if p.get(n) not in (None, "default"):
            extra[n] = p[n]
    if p.get("weights_column"):
        extra["weights"] = [float(v) for v in _column(df, p["weights_column"], "weights")]
    return extra


def _error(exc: Exception, warned=None) -> dict:
    """A clean one-line error payload (never a raw rpy2 traceback dump)."""
    lines = [ln.strip() for ln in str(exc).strip().splitlines() if ln.strip()]
    # An R error starts with a line naming the call, such as "Error in
    # (function (data, ...) :", and gives its message on the lines after it.
    # Keep the message, joined into one line, so the agent can act on it.
    if len(lines) > 1 and lines[0].startswith("Error in ") and lines[0].endswith(":"):
        lines = lines[1:]
    message = " ".join(lines) if lines else str(exc)
    # every result has a warnings list, an error before R is reached too
    return {"error": "{}: {}".format(type(exc).__name__, message), "warnings": list(warned or [])}


def _finite(x):
    """Coerce to a JSON-safe float: NaN/Inf become None.

    Strict JSON (RFC 8259) has no NaN/Infinity tokens, and the MCP serializer
    would emit invalid JSON for them. The DataFrame paths below already map
    non-finite values to null via _records. This keeps the scalar fields
    consistent so a pathological R result can never produce invalid JSON.
    """
    if x is None:
        return None
    f = float(x)
    return f if math.isfinite(f) else None


def _json_value(v):
    """A table cell as a JSON-safe Python value, numbers at full precision."""
    import numpy as np
    import pandas as pd

    if isinstance(v, np.generic):
        v = v.item()
    if isinstance(v, float):
        return _finite(v)
    if v is None or isinstance(v, (bool, int, str)):
        return v
    # pandas.NA or NaT for a missing cell, and text for anything else
    return None if pd.isna(v) else str(v)


def _records(df):
    """A pandas DataFrame as a list of row dicts of plain Python values.

    ``DataFrame.to_json()`` rounds every number to 10 decimal places by
    default, so a cost in small units became 0. This keeps every digit, as
    the fields of ``optimize`` do.
    """
    return [{str(k): _json_value(v) for k, v in row.items()} for row in df.to_dict(orient="records")]


# The model arguments every model tool takes, explained once for the agent.
MODEL_ARGS_DOC = """
    Data: EXACTLY ONE of `data_csv` (a path to a CSV file the server reads) or
    `data_records` (a list of row dicts, one per row).

    Costs: EXACTLY ONE of `cost_list_of_vectors` (one list of polynomial
    coefficients per component, [c0, c1, ...] for c0 + c1 x + ...) or
    `unit_costs` (one cost per unit per component) with
    `default_cost_fxn_type` ("linear" or "cubic", the package default) and,
    for a cubic cost, optionally `unit_cost_doses`.

    Columns the package reads by name can be built from your columns:
    `group_column` with `treatment_value` (for a power goal), `center_column`
    (for center effects), `period_column` (for time effects), and for center
    level data (`input_data_structure="center_level"`, one row per center and
    period, binary outcome) `successes_column` or `proportion_column` with
    `total_column`. Without either, `outcome_name` is the proportion column. `weights_column` names a column of glm() weights.

    Model: `glm_family` and `link` ("logit" or "identity"),
    `center_characteristics` with `center_characteristics_optimization_values`,
    `additional_covariates`, `include_center_effects` with either
    `center_effects_optimization_values` (one center to plan for) or
    `center_weights_for_outcome_goal` (one weight per center, in sorted order,
    summing to 1), `include_time_effects` with
    `time_effect_optimization_value`, and `include_interaction_terms` with the
    products listed in `intervention_components` as "a:b" and the mains in
    `main_components`. The server builds the column of a product "a:b" from
    the columns a and b when the data has none. Optimizer: `optimization_method` ("numerical" or
    "grid_search") with `optimization_grid_search_step_size`. Shrinking
    method: `prev_recommended_interventions`.
"""


def _with_model_doc(fn):
    """Put the shared model arguments into a tool's description.

    It runs before ``mcp.tool()``, which reads the description from the
    docstring when it registers the tool.
    """
    fn.__doc__ = fn.__doc__.format(model=MODEL_ARGS_DOC)
    return fn


# --------------------------------------------------------------------------
# Tool 0: describe_data
# --------------------------------------------------------------------------
def describe_data(data_csv: str | None = None, data_records: list[dict] | None = None) -> Any:
    """Describe the columns of a dataset, to choose the arguments of the other tools.

    Provide EXACTLY ONE of `data_csv` (a path to a CSV file) or `data_records`
    (a list of row dicts). Returns `n_rows` and one entry per column with its
    `type` ("numeric", "logical" for true and false, or "text"), `n_missing`,
    `n_distinct`, whether it is `binary` (only 0 and 1, or true and false), and
    for a numeric column its `min` and `max`, and an empty `warnings` list.
    Use the ranges for the bounds of the components and for any center
    characteristic value. On a bad input it returns
    `{"error": "...", "warnings": []}` as an error result.
    """
    try:
        import pandas as pd

        df = _resolve_data(data_csv, data_records)
        cols = []
        for name in df.columns:
            col = df[name]
            logical = pd.api.types.is_bool_dtype(col) or (
                col.dtype == object and len(col.dropna()) > 0 and all(isinstance(v, bool) for v in col.dropna())
            )
            numeric = pd.api.types.is_numeric_dtype(col) and not logical
            values = col.dropna()
            entry = {
                "name": str(name),
                "type": "logical" if logical else "numeric" if numeric else "text",
                "n_missing": int(col.isna().sum()),
                "n_distinct": int(values.nunique()),
                # a true/false column works as a binary outcome too
                "binary": bool(logical or (numeric and len(values) > 0 and set(values.unique()) <= {0, 1})),
            }
            if numeric and len(values):
                entry["min"] = _finite(values.min())
                entry["max"] = _finite(values.max())
            cols.append(entry)
        return {"n_rows": int(len(df)), "columns": cols, "warnings": []}
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc)


# --------------------------------------------------------------------------
# Tool 1: optimize
# --------------------------------------------------------------------------
@_with_model_doc
def optimize(
    outcome_name: str,
    outcome_type: str,
    intervention_components: list[str],
    intervention_lower_bounds: list[float],
    intervention_upper_bounds: list[float],
    cost_list_of_vectors: list[list[float]] | None = None,
    data_csv: str | None = None,
    data_records: list[dict] | None = None,
    outcome_goal: float | None = None,
    power_goal: float | None = None,
    outcome_goal_intention: str = "maximize",
    glm_family: str = "default",
    link: str = "default",
    center_characteristics: list[str] | None = None,
    center_characteristics_optimization_values: list[float] | None = None,
    num_centers_in_next_stage: int | None = None,
    patients_per_center_in_next_stage: int | None = None,
    power_goal_approach: str | None = None,
    icc: list[float] | float | None = None,
    power_goal_cluster_id: str | None = None,
    confidence_set_grid_step_size: list[float] | None = None,
    include_confidence_set: bool = True,
    confidence_set_alpha: float | None = None,
    unit_costs: list[float] | None = None,
    default_cost_fxn_type: str | None = None,
    unit_cost_doses: list[float] | None = None,
    input_data_structure: str | None = None,
    group_column: str | None = None,
    treatment_value: str | float | None = None,
    center_column: str | None = None,
    period_column: str | None = None,
    successes_column: str | None = None,
    proportion_column: str | None = None,
    total_column: str | None = None,
    weights_column: str | None = None,
    additional_covariates: list[str] | None = None,
    include_center_effects: bool | None = None,
    center_effects_optimization_values: str | None = None,
    center_weights_for_outcome_goal: list[float] | None = None,
    include_time_effects: bool | None = None,
    time_effect_optimization_value: float | None = None,
    include_interaction_terms: bool | None = None,
    main_components: list[str] | None = None,
    optimization_method: str | None = None,
    optimization_grid_search_step_size: list[float] | None = None,
    prev_recommended_interventions: list[float] | None = None,
) -> dict:
    """Run one LAGO optimization and return the recommended intervention.

    Supply the outcome column, the intervention component columns with their
    lower and upper bounds, the costs, and at least one of `outcome_goal` or
    `power_goal`. A `power_goal` needs a binary outcome, a group column (see
    `group_column`), `num_centers_in_next_stage` and
    `patients_per_center_in_next_stage`, and can take `power_goal_approach`
    ("unconditional" or "conditional"), `icc` (one value, or control and
    treatment values) and `power_goal_cluster_id`. `confidence_set_alpha` sets
    the level of the interval and the confidence set (0.05 gives 95%).
    {model}
    Returns `rec_int` (the recommended value per component), `rec_int_cost`,
    `est_outcome_goal` (the estimated outcome there), `effective_outcome_goal`
    (the goal used, which a power goal can raise), `shrinking_method_used`
    (True when the goal cannot be reached within the bounds, so no confidence
    set is computed), and with a confidence set `est_outcome_ci`
    (lower/upper), `confidence_level`, `confidence_set_size_percentage` and
    `confidence_set`. `warnings` lists the warnings of the R package. On a bad
    input or optimization error it returns `{{"error": "...", "warnings": [...]}}`
    as an error result.
    """
    p = dict(locals())
    try:
        data, outcome = _prepare_data(_resolve_data(data_csv, data_records), p)
        extra = _model_kwargs(p, data)
        extra["include_confidence_set"] = include_confidence_set
        if confidence_set_grid_step_size is not None:
            extra["confidence_set_grid_step_size"] = confidence_set_grid_step_size
        if confidence_set_alpha is not None:
            extra["confidence_set_alpha"] = confidence_set_alpha
        with collect_warnings() as warned:
            res = lago.optimize(
                data=data,
                outcome_name=outcome,
                outcome_type=outcome_type,
                intervention_components=intervention_components,
                intervention_lower_bounds=intervention_lower_bounds,
                intervention_upper_bounds=intervention_upper_bounds,
                outcome_goal=outcome_goal,
                cost_list=cost_list_of_vectors,
                quiet=True,
                **extra,
            )

        out: dict[str, Any] = {
            "rec_int": [_finite(v) for v in res["rec_int"]],
            "rec_int_cost": _finite(res["rec_int_cost"]),
            "est_outcome_goal": _finite(res["est_outcome_goal"]),
            # True when the goal could not be reached, so no confidence set
            "shrinking_method_used": bool(res.get("shrinking_method_used", False)),
        }
        if res.get("effective_outcome_goal") is not None:
            out["effective_outcome_goal"] = _finite(res["effective_outcome_goal"])
        ci = res.get("est_outcome_ci")
        if isinstance(ci, dict):
            out["est_outcome_ci"] = {k: _finite(v) for k, v in ci.items()}
            out["confidence_level"] = 1 - (confidence_set_alpha if confidence_set_alpha is not None else 0.05)
        csp = res.get("confidence_set_size_percentage")
        if csp is not None:
            out["confidence_set_size_percentage"] = _finite(csp)
        cs = res.get("cs")
        if cs is not None and hasattr(cs, "to_dict"):
            out["confidence_set"] = _records(cs)
        out["warnings"] = warned
        return out
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc, locals().get("warned"))


# --------------------------------------------------------------------------
# Tool 2: sensitivity
# --------------------------------------------------------------------------
@_with_model_doc
def sensitivity(
    outcome_name: str,
    outcome_type: str,
    intervention_components: list[str],
    intervention_lower_bounds: list[float],
    intervention_upper_bounds: list[float],
    parameter: str,
    values: list[float],
    cost_list_of_vectors: list[list[float]] | None = None,
    data_csv: str | None = None,
    data_records: list[dict] | None = None,
    outcome_goal: float | None = None,
    power_goal: float | None = None,
    outcome_goal_intention: str = "maximize",
    glm_family: str = "default",
    link: str = "default",
    center_characteristics: list[str] | None = None,
    center_characteristics_optimization_values: list[float] | None = None,
    num_centers_in_next_stage: int | None = None,
    patients_per_center_in_next_stage: int | None = None,
    power_goal_approach: str | None = None,
    icc: list[float] | float | None = None,
    power_goal_cluster_id: str | None = None,
    unit_costs: list[float] | None = None,
    default_cost_fxn_type: str | None = None,
    unit_cost_doses: list[float] | None = None,
    input_data_structure: str | None = None,
    group_column: str | None = None,
    treatment_value: str | float | None = None,
    center_column: str | None = None,
    period_column: str | None = None,
    successes_column: str | None = None,
    proportion_column: str | None = None,
    total_column: str | None = None,
    weights_column: str | None = None,
    additional_covariates: list[str] | None = None,
    include_center_effects: bool | None = None,
    center_effects_optimization_values: str | None = None,
    center_weights_for_outcome_goal: list[float] | None = None,
    include_time_effects: bool | None = None,
    time_effect_optimization_value: float | None = None,
    include_interaction_terms: bool | None = None,
    main_components: list[str] | None = None,
    optimization_method: str | None = None,
    optimization_grid_search_step_size: list[float] | None = None,
    prev_recommended_interventions: list[float] | None = None,
) -> Any:
    """Sweep one LAGO input and report how the recommendation moves.

    Takes the same arguments as `optimize`, plus `parameter` and `values`.
    `parameter` is either the name of a scalar numeric optimization argument
    that affects the recommendation (for example `"outcome_goal"`,
    `"power_goal"`, `"icc"`, `"num_centers_in_next_stage"` or
    `"time_effect_optimization_value"`) or the special string
    `"cost_multiplier"` (each run scales every cost coefficient by one element
    of `values`. All values must be positive, and it needs
    `cost_list_of_vectors`.) `values` is a non-empty list of finite numbers,
    one run per element. The confidence set is not computed.
    {model}
    Returns `{{"runs": [...], "warnings": [...]}}`. `runs` has one record per
    swept value, each with `value` (the swept value), one entry per
    intervention component (its recommended value that run), `rec_int_cost`,
    `est_outcome_goal`, and `status` (`"ok"` or `"error"`). `warnings` lists
    the warnings of the R package, each once. On a bad input or setup error it
    returns `{{"error": "...", "warnings": [...]}}` as an error result.
    """
    p = dict(locals())
    try:
        data, outcome = _prepare_data(_resolve_data(data_csv, data_records), p)
        extra = _model_kwargs(p, data)
        with collect_warnings() as warned:
            df = lago.sensitivity(
                data=data,
                outcome_name=outcome,
                outcome_type=outcome_type,
                intervention_components=intervention_components,
                intervention_lower_bounds=intervention_lower_bounds,
                intervention_upper_bounds=intervention_upper_bounds,
                parameter=parameter,
                values=values,
                outcome_goal=outcome_goal,
                cost_list=cost_list_of_vectors,
                quiet=True,
                **extra,
            )
        return {"runs": _records(df), "warnings": warned}
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc, locals().get("warned"))


# --------------------------------------------------------------------------
# Tool 3: budget
# --------------------------------------------------------------------------
@_with_model_doc
def budget(
    outcome_name: str,
    outcome_type: str,
    intervention_components: list[str],
    intervention_lower_bounds: list[float],
    intervention_upper_bounds: list[float],
    budget: float,
    cost_list_of_vectors: list[list[float]] | None = None,
    n_grid: int = 25,
    data_csv: str | None = None,
    data_records: list[dict] | None = None,
    outcome_goal_intention: str = "maximize",
    glm_family: str = "default",
    link: str = "default",
    center_characteristics: list[str] | None = None,
    center_characteristics_optimization_values: list[float] | None = None,
    unit_costs: list[float] | None = None,
    default_cost_fxn_type: str | None = None,
    unit_cost_doses: list[float] | None = None,
    input_data_structure: str | None = None,
    center_column: str | None = None,
    period_column: str | None = None,
    successes_column: str | None = None,
    proportion_column: str | None = None,
    total_column: str | None = None,
    weights_column: str | None = None,
    additional_covariates: list[str] | None = None,
    include_center_effects: bool | None = None,
    center_effects_optimization_values: str | None = None,
    center_weights_for_outcome_goal: list[float] | None = None,
    include_time_effects: bool | None = None,
    time_effect_optimization_value: float | None = None,
    include_interaction_terms: bool | None = None,
    main_components: list[str] | None = None,
    optimization_method: str | None = None,
    optimization_grid_search_step_size: list[float] | None = None,
) -> Any:
    """Find the best outcome a budget can buy, and the intervention that buys it.

    Takes the same data and model arguments as `optimize`, without a goal,
    which the search sweeps. `budget` is the most the intervention may cost,
    in the units of the costs. `n_grid` is how many outcome goals the first
    pass tries (a second pass tries as many again near the budget). Fewer goals
    are faster, and more give an answer closer to the best.
    {model}
    Returns `feasible` (whether any reachable intervention fits the budget),
    `binding` (whether a better outcome would cost more than the budget),
    `rec_int` (one level per component), `rec_int_cost`, `est_outcome`,
    `outcome_goal`, `frontier` (the goals searched, with their cost and
    outcome), `run_error` (set when every run failed) and `warnings` (the
    warnings of the R package). On a bad input or setup error it returns
    `{{"error": "...", "warnings": [...]}}` as an error result.
    """
    p = dict(locals())
    try:
        data, outcome = _prepare_data(_resolve_data(data_csv, data_records), p)
        extra = _model_kwargs(p, data)
        with collect_warnings() as warned:
            res = lago.budget(
                data=data,
                outcome_name=outcome,
                outcome_type=outcome_type,
                intervention_components=intervention_components,
                intervention_lower_bounds=intervention_lower_bounds,
                intervention_upper_bounds=intervention_upper_bounds,
                budget=budget,
                n_grid=n_grid,
                cost_list=cost_list_of_vectors,
                quiet=True,
                **extra,
            )
        frontier = res.get("frontier")
        return {
            "budget": _finite(res["budget"]),
            "feasible": bool(res["feasible"]),
            "binding": bool(res["binding"]),
            "rec_int": [_finite(v) for v in res["rec_int"]],
            "rec_int_cost": _finite(res["rec_int_cost"]),
            "est_outcome": _finite(res["est_outcome"]),
            "outcome_goal": _finite(res["outcome_goal"]),
            "frontier": _records(frontier) if hasattr(frontier, "to_dict") else [],
            "run_error": res.get("run_error"),
            "warnings": warned,
        }
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc, locals().get("warned"))


# --------------------------------------------------------------------------
# The R worker: the process that runs the tools for the server
# --------------------------------------------------------------------------
def _watch_parent(parent_pid, interval=1.0):
    """Exit the worker when the server that started it is gone.

    Stopping the server with SIGKILL, or any way that skips its cleanup, would
    otherwise leave a running R call going on its own until it ends. R gives
    up Python's lock while it computes, so this thread runs during a call.
    """
    import os
    import time

    while True:
        time.sleep(interval)
        if os.getppid() != parent_pid:
            os._exit(1)


def _worker_main(conn, parent_pid=None):
    """Run tool calls sent over ``conn`` until the pipe closes."""
    if parent_pid is not None:
        import threading

        threading.Thread(target=_watch_parent, args=(parent_pid,), daemon=True).start()
    tools = {fn.__name__: fn for fn in (describe_data, optimize, sensitivity, budget)}
    while True:
        try:
            name, kwargs = conn.recv()
        except (EOFError, OSError):
            return
        try:
            result = tools[name](**kwargs)
        except Exception as exc:  # noqa: BLE001 - the tools return errors, but never crash the worker
            result = _error(exc)
        conn.send(result)


class _Call:
    """One tool call, which the client may cancel before or while it runs."""

    def __init__(self):
        self.cancelled = False


class _RWorker:
    """One worker process that runs the R work of the tools, one call at a time.

    A running R call cannot be interrupted from another thread, so the server
    runs the tools in a process of its own. When a client cancels the call
    that is running, the process is stopped at once, and the next call starts
    a new one. A cancelled call that is still waiting for its turn just never
    runs, so it does not stop the call ahead of it. The server stays free to
    answer pings and cancellations while a call runs.
    """

    def __init__(self):
        import threading

        self._proc = None
        self._conn = None
        # one call at a time. cancel() does not take it, so a cancel never
        # waits for the call it cancels: stopping the process ends that call.
        self._calls = threading.Lock()
        # guards the process and the running call, which cancel() reads from
        # another thread
        self._state = threading.RLock()
        self._running = None

    def start(self):
        with self._state:
            if self._proc is not None and self._proc.is_alive():
                return
            import multiprocessing
            import os

            ctx = multiprocessing.get_context("spawn")
            parent, child = ctx.Pipe()
            self._proc = ctx.Process(target=_worker_main, args=(child, os.getpid()), daemon=True)
            self._proc.start()
            child.close()
            self._conn = parent

    def stop(self):
        with self._state:
            if self._proc is not None:
                self._proc.kill()
                self._proc.join(5)
                self._conn.close()
            self._proc = None
            self._conn = None

    def kill(self):
        """Kill the process now, without waiting for a lock (for a signal handler)."""
        proc = self._proc
        if proc is not None:
            proc.kill()

    def cancel(self, call):
        """Cancel one call: stop the worker only if that call is the one running."""
        with self._state:
            call.cancelled = True
            if self._running is call:
                self.stop()

    def call(self, name, kwargs, call=None):
        """Run one tool in the worker and wait for its result (blocking).

        Returns None, without running, for a call cancelled while it waited.
        """
        call = call or _Call()
        with self._calls:
            with self._state:
                if call.cancelled:
                    return None
                self.start()
                self._running = call
                conn = self._conn
            try:
                conn.send((name, kwargs))
                return conn.recv()
            except (EOFError, OSError):
                # the process died. Clear it up now, while no other call can
                # have started a new one.
                if not call.cancelled:
                    self.stop()
                raise
            finally:
                with self._state:
                    self._running = None


_WORKER = _RWorker()


def _error_result(out):
    """An error payload as a tool result marked as an error, with the JSON as its text."""
    from mcp.types import CallToolResult, TextContent

    return CallToolResult(content=[TextContent(type="text", text=json.dumps(out, indent=2))], isError=True)


def _run_in_worker(fn):
    """The tool as FastMCP runs it: the same function, in the R worker."""

    @functools.wraps(fn)
    async def tool(**kwargs):
        call = _Call()
        try:
            out = await anyio.to_thread.run_sync(_WORKER.call, fn.__name__, kwargs, call, abandon_on_cancel=True)
        except anyio.get_cancelled_exc_class():
            # the client cancelled: stop the R work now if it is running, and
            # never start it if it is still waiting
            _WORKER.cancel(call)
            raise
        except (EOFError, OSError) as exc:
            out = _error(RuntimeError(f"the R worker stopped during the call ({exc!r}). Try the call again."))
        if isinstance(out, dict) and "error" in out:
            return _error_result(out)
        return out

    return tool


for _fn in (describe_data, optimize, sensitivity, budget):
    mcp.tool(name=_fn.__name__)(_run_in_worker(_fn))


def _on_sigterm(signum, frame):
    """Stop the R worker, then end the server as SIGTERM would have."""
    import os
    import signal

    _WORKER.kill()
    signal.signal(signum, signal.SIG_DFL)
    os.kill(os.getpid(), signum)


def main():
    """Console entry point: run the stdio MCP server."""
    import signal

    # SIGTERM ends the process without running the finally below
    signal.signal(signal.SIGTERM, _on_sigterm)
    # start the R worker now, so the first call does not wait for R to load
    _WORKER.start()
    try:
        mcp.run()
    finally:
        _WORKER.stop()


if __name__ == "__main__":
    main()
