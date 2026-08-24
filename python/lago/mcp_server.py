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
``optimize``      -> :func:`lago.optimize`   (a single LAGO optimization)
``sensitivity``   -> :func:`lago.sensitivity` (sweep one input, report the move)

Deliberately NOT exposed
------------------------
``visualize_cost`` opens an interactive Shiny browser app and BLOCKS until the
user closes it, which is a poor fit for an autonomous tool call. ``lago_report``
writes an HTML file to disk as a side effect. Neither maps cleanly onto a
request/response tool, so they are intentionally left out of the MCP surface.

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

mcp = FastMCP("lago")


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


def _baseline_kwargs(
    outcome_goal,
    power_goal,
    outcome_goal_intention,
    glm_family,
    link,
    center_characteristics,
    center_characteristics_optimization_values,
):
    """Build the shared optional baseline kwargs for optimize/sensitivity.

    ``glm_family`` / ``link`` use the sentinel ``"default"`` to mean "let R
    decide", so they are only forwarded when the caller overrides them.
    ``power_goal`` and the center-characteristics arguments are forwarded only
    when supplied.
    """
    extra: dict[str, Any] = {"outcome_goal_intention": outcome_goal_intention}
    if power_goal is not None:
        extra["power_goal"] = power_goal
    if glm_family != "default":
        extra["glm_family"] = glm_family
    if link != "default":
        extra["link"] = link
    if center_characteristics is not None:
        extra["center_characteristics"] = center_characteristics
    if center_characteristics_optimization_values is not None:
        extra["center_characteristics_optimization_values"] = (
            center_characteristics_optimization_values
        )
    return extra


def _error(exc: Exception) -> dict:
    """A clean one-line error payload (never a raw rpy2 traceback dump)."""
    msg = str(exc).strip()
    # Collapse to the first meaningful line so an rpy2/R error message does not
    # dump a multi-line traceback into the tool result.
    first_line = next((ln for ln in msg.splitlines() if ln.strip()), msg)
    return {"error": "{}: {}".format(type(exc).__name__, first_line)}


def _finite(x):
    """Coerce to a JSON-safe float: NaN/Inf become None.

    Strict JSON (RFC 8259) has no NaN/Infinity tokens, and the MCP serializer
    would emit invalid JSON for them. The DataFrame paths below already map
    non-finite values to null via to_json; this keeps the scalar fields
    consistent so a pathological R result can never produce invalid JSON.
    """
    f = float(x)
    return f if math.isfinite(f) else None


# --------------------------------------------------------------------------
# Tool 1: optimize
# --------------------------------------------------------------------------
@mcp.tool()
def optimize(
    outcome_name: str,
    outcome_type: str,
    intervention_components: list[str],
    intervention_lower_bounds: list[float],
    intervention_upper_bounds: list[float],
    cost_list_of_vectors: list[list[float]],
    data_csv: str | None = None,
    data_records: list[dict] | None = None,
    outcome_goal: float | None = None,
    power_goal: float | None = None,
    outcome_goal_intention: str = "maximize",
    glm_family: str = "default",
    link: str = "default",
    center_characteristics: list[str] | None = None,
    center_characteristics_optimization_values: list[float] | None = None,
    confidence_set_grid_step_size: list[float] | None = None,
    include_confidence_set: bool = True,
) -> dict:
    """Run one LAGO optimization and return the recommended intervention.

    Provide the trial data as EXACTLY ONE of `data_csv` (a path to a CSV file
    the server reads) or `data_records` (a list of row dicts, one per
    observation). Supply the outcome column, the intervention component
    columns and their per-component lower/upper bounds, and the cost functions
    as `cost_list_of_vectors` (one list of polynomial coefficients per
    component). At least one of `outcome_goal` or `power_goal` must be given.

    Returns a JSON dict with `rec_int` (the recommended value per component),
    `rec_int_cost` (its cost), `est_outcome_goal` (the estimated outcome at the
    recommendation), and, when a confidence set is computed, `est_outcome_ci`
    (lower/upper), `confidence_set_size_percentage`, and `confidence_set` (the
    confidence-set rows). On a bad input or optimization error it returns
    `{"error": "..."}` with a short message instead of raising.
    """
    try:
        data = _resolve_data(data_csv, data_records)
        extra = _baseline_kwargs(
            outcome_goal,
            power_goal,
            outcome_goal_intention,
            glm_family,
            link,
            center_characteristics,
            center_characteristics_optimization_values,
        )
        extra["include_confidence_set"] = include_confidence_set
        if confidence_set_grid_step_size is not None:
            extra["confidence_set_grid_step_size"] = confidence_set_grid_step_size

        res = lago.optimize(
            data=data,
            outcome_name=outcome_name,
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
        }
        ci = res.get("est_outcome_ci")
        if isinstance(ci, dict):
            out["est_outcome_ci"] = {k: _finite(v) for k, v in ci.items()}
        csp = res.get("confidence_set_size_percentage")
        if csp is not None:
            out["confidence_set_size_percentage"] = _finite(csp)
        cs = res.get("cs")
        if cs is not None and hasattr(cs, "to_json"):
            # to_json -> json.loads yields native Python/JSON types (no numpy
            # scalars that the MCP serializer would choke on).
            out["confidence_set"] = json.loads(cs.to_json(orient="records"))
        return out
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc)


# --------------------------------------------------------------------------
# Tool 2: sensitivity
# --------------------------------------------------------------------------
@mcp.tool()
def sensitivity(
    outcome_name: str,
    outcome_type: str,
    intervention_components: list[str],
    intervention_lower_bounds: list[float],
    intervention_upper_bounds: list[float],
    cost_list_of_vectors: list[list[float]],
    parameter: str,
    values: list[float],
    data_csv: str | None = None,
    data_records: list[dict] | None = None,
    outcome_goal: float | None = None,
    power_goal: float | None = None,
    outcome_goal_intention: str = "maximize",
    glm_family: str = "default",
    link: str = "default",
    center_characteristics: list[str] | None = None,
    center_characteristics_optimization_values: list[float] | None = None,
) -> Any:
    """Sweep one LAGO input and report how the recommendation moves.

    Takes the same data and optimization arguments as `optimize` (provide data
    as EXACTLY ONE of `data_csv` or `data_records`), plus `parameter` and
    `values`. `parameter` is either the name of a scalar numeric optimization
    argument that affects the recommendation (for example `"outcome_goal"`,
    `"power_goal"`, or `"shrinkage_threshold"`) or the special string
    `"cost_multiplier"` (each run scales every cost coefficient by one element
    of `values`; all values must be positive). `values` is a non-empty list of
    finite numbers, one run per element.

    Returns a list of record dicts, one per swept value, each with `value`
    (the swept value), one entry per intervention component (its recommended
    value that run), `rec_int_cost`, `est_outcome_goal`, and `status` (`"ok"`
    or `"error"`). On a bad input or setup error it returns `{"error": "..."}`
    with a short message instead of raising.
    """
    try:
        data = _resolve_data(data_csv, data_records)
        extra = _baseline_kwargs(
            outcome_goal,
            power_goal,
            outcome_goal_intention,
            glm_family,
            link,
            center_characteristics,
            center_characteristics_optimization_values,
        )
        df = lago.sensitivity(
            data=data,
            outcome_name=outcome_name,
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
        return json.loads(df.to_json(orient="records"))
    except Exception as exc:  # noqa: BLE001 - surface a clean message, not a crash
        return _error(exc)


def main():
    """Console entry point: run the stdio MCP server."""
    mcp.run()


if __name__ == "__main__":
    main()
