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

The three model tools take the same model arguments, which cover every
argument of ``lago_optimization()`` that a request can carry. The columns the
package reads by a fixed name (``group``, ``center``, ``period``, and for
center level data ``proportion`` and ``center_sample_size``) can be built from
columns the caller names, as ``lago_app()`` does.

Every result has a ``warnings`` list with the warnings of the R package, such as
a recommendation for a center outside the centers in the data, so an agent can
pass them on instead of reporting a number without its caveats.

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

import json
import math
from typing import Any

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
4. A binary outcome's estimated outcome and interval are probabilities. An
   interval from 0 to 1 means the data cannot pin the outcome down there.
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


def _prepare_data(df, p):
    """Build the columns the package reads by a fixed name, from named columns.

    Returns the data and the outcome name to use. A column the package reads is
    only replaced when the caller names a different column for it.
    """
    import pandas as pd

    df = df.copy()
    outcome_name = p["outcome_name"]
    if p.get("group_column"):
        arm = _column(df, p["group_column"], "group").astype(str)
        if p.get("treatment_value") is None:
            raise ValueError("`group_column` needs `treatment_value`, the value that marks treated rows.")
        df["group"] = (arm == str(p["treatment_value"])).map({True: "treatment", False: "control"})
    if p.get("center_column") and p["center_column"] != "center":
        df["center"] = pd.Categorical(_column(df, p["center_column"], "center"))
    if p.get("period_column") and p["period_column"] != "period":
        df["period"] = pd.Categorical(_column(df, p["period_column"], "period"))
    if p.get("input_data_structure") == "center_level":
        total = p.get("total_column")
        if p.get("successes_column"):
            if not total:
                raise ValueError("`successes_column` needs `total_column`, the number of patients of each row.")
            df["proportion"] = _column(df, p["successes_column"], "successes") / _column(df, total, "total")
        elif p.get("proportion_column") and p["proportion_column"] != "proportion":
            df["proportion"] = _column(df, p["proportion_column"], "proportion")
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
    out = {"error": "{}: {}".format(type(exc).__name__, message)}
    if warned:
        out["warnings"] = list(warned)
    return out


def _finite(x):
    """Coerce to a JSON-safe float: NaN/Inf become None.

    Strict JSON (RFC 8259) has no NaN/Infinity tokens, and the MCP serializer
    would emit invalid JSON for them. The DataFrame paths below already map
    non-finite values to null via to_json; this keeps the scalar fields
    consistent so a pathological R result can never produce invalid JSON.
    """
    if x is None:
        return None
    f = float(x)
    return f if math.isfinite(f) else None


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
    `total_column`. `weights_column` names a column of glm() weights.

    Model: `glm_family` and `link` ("logit" or "identity"),
    `center_characteristics` with `center_characteristics_optimization_values`,
    `additional_covariates`, `include_center_effects` with either
    `center_effects_optimization_values` (one center to plan for) or
    `center_weights_for_outcome_goal` (one weight per center, in sorted order,
    summing to 1), `include_time_effects` with
    `time_effect_optimization_value`, and `include_interaction_terms` with the
    products listed in `intervention_components` as "a:b" and the mains in
    `main_components`. Optimizer: `optimization_method` ("numerical" or
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
@mcp.tool()
def describe_data(data_csv: str | None = None, data_records: list[dict] | None = None) -> Any:
    """Describe the columns of a dataset, to choose the arguments of the other tools.

    Provide EXACTLY ONE of `data_csv` (a path to a CSV file) or `data_records`
    (a list of row dicts). Returns `n_rows` and one entry per column with its
    `type`, `n_missing`, `n_distinct`, whether it is `binary` (only 0 and 1),
    and for a numeric column its `min` and `max`. Use the ranges for the bounds
    of the components and for any center characteristic value.
    """
    try:
        import pandas as pd

        df = _resolve_data(data_csv, data_records)
        cols = []
        for name in df.columns:
            col = df[name]
            numeric = pd.api.types.is_numeric_dtype(col) and not pd.api.types.is_bool_dtype(col)
            values = col.dropna()
            entry = {
                "name": str(name),
                "type": "numeric" if numeric else "text",
                "n_missing": int(col.isna().sum()),
                "n_distinct": int(values.nunique()),
                "binary": bool(numeric and len(values) > 0 and set(values.unique()) <= {0, 1}),
            }
            if numeric and len(values):
                entry["min"] = _finite(values.min())
                entry["max"] = _finite(values.max())
            cols.append(entry)
        return {"n_rows": int(len(df)), "columns": cols}
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc)


# --------------------------------------------------------------------------
# Tool 1: optimize
# --------------------------------------------------------------------------
@mcp.tool()
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
    input or optimization error it returns `{{"error": "..."}}`.
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
        if cs is not None and hasattr(cs, "to_json"):
            # to_json -> json.loads yields native Python/JSON types (no numpy
            # scalars that the MCP serializer would choke on).
            out["confidence_set"] = json.loads(cs.to_json(orient="records"))
        out["warnings"] = warned
        return out
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc, locals().get("warned"))


# --------------------------------------------------------------------------
# Tool 2: sensitivity
# --------------------------------------------------------------------------
@mcp.tool()
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
    of `values`; all values must be positive, and it needs
    `cost_list_of_vectors`). `values` is a non-empty list of finite numbers,
    one run per element. The confidence set is not computed.
    {model}
    Returns `{{"runs": [...], "warnings": [...]}}`. `runs` has one record per
    swept value, each with `value` (the swept value), one entry per
    intervention component (its recommended value that run), `rec_int_cost`,
    `est_outcome_goal`, and `status` (`"ok"` or `"error"`). `warnings` lists
    the warnings of the R package, each once. On a bad input or setup error it
    returns `{{"error": "..."}}`.
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
        # to_json -> json.loads yields native Python/JSON types (records with
        # no numpy scalars).
        return {"runs": json.loads(df.to_json(orient="records")), "warnings": warned}
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc, locals().get("warned"))


# --------------------------------------------------------------------------
# Tool 3: budget
# --------------------------------------------------------------------------
@mcp.tool()
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
    `{{"error": "..."}}`.
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
            "frontier": json.loads(frontier.to_json(orient="records")) if hasattr(frontier, "to_json") else [],
            "run_error": res.get("run_error"),
            "warnings": warned,
        }
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc, locals().get("warned"))


def main():
    """Console entry point: run the stdio MCP server."""
    mcp.run()


if __name__ == "__main__":
    main()
