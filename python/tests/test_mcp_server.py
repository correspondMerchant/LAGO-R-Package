"""Tests for the LAGO MCP server (lago.mcp_server).

These test the tool FUNCTIONS at the function/registration level. The blocking
stdio server (mcp.run()) is intentionally NOT launched: it would block on
stdin/stdout forever. Instead we call the registered tool callables directly
(FastMCP's ``@mcp.tool()`` returns the original function unchanged) and inspect
the FastMCP tool registry for the input schemas.

Like the wrapper tests, the tool-execution tests drive the REAL LAGO R
functions through rpy2 and reuse the shared ``ro`` fixture (see conftest.py), so
they skip cleanly when R / rpy2 / LAGOtrials cannot be reached. The whole module
skips if the ``mcp`` SDK is not installed.
"""
import asyncio

import numpy as np
import pytest

# Skip the entire module if the optional mcp SDK is not installed.
pytest.importorskip(
    "mcp.server.fastmcp",
    reason="mcp SDK not installed; install with `pip install -e python[mcp]`",
)

from lago import mcp_server as S  # noqa: E402


# --------------------------------------------------------------------------
# Fixtures / shared args
# --------------------------------------------------------------------------
@pytest.fixture(scope="module")
def mtcars_records(ro):
    """R's built-in mtcars (mpg/gear/qsec) as a list of row dicts."""
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    mt_r = ro.r("mtcars")
    with localconverter(ro.default_converter + pandas2ri.converter):
        mt = ro.conversion.get_conversion().rpy2py(mt_r)
    return mt[["mpg", "gear", "qsec"]].to_dict(orient="records")


def _common_args():
    """Baseline optimization args shared by the optimize/sensitivity tests
    (the mtcars continuous-outcome example from the R docs)."""
    return dict(
        outcome_name="mpg",
        outcome_type="continuous",
        intervention_components=["gear", "qsec"],
        intervention_lower_bounds=[0.0, 0.0],
        intervention_upper_bounds=[10.0, 350.0],
        cost_list_of_vectors=[[0.0, 4.0], [4.0, 6.0]],
        glm_family="gaussian",
        link="identity",
        outcome_goal_intention="maximize",
    )


# --------------------------------------------------------------------------
# 1. optimize tool: real output on a small dataset (data_records)
# --------------------------------------------------------------------------
def test_optimize_tool_records(mtcars_records):
    out = S.optimize(
        data_records=mtcars_records,
        outcome_goal=30.0,
        include_confidence_set=False,
        **_common_args(),
    )
    assert isinstance(out, dict)
    assert "error" not in out, out
    # rec_int is the right length (one per component) and all floats
    assert isinstance(out["rec_int"], list)
    assert len(out["rec_int"]) == 2
    assert all(isinstance(v, float) for v in out["rec_int"])
    assert isinstance(out["rec_int_cost"], float)
    assert isinstance(out["est_outcome_goal"], float)
    # non-vacuous: the estimate reaches the goal and the cost is positive
    assert out["rec_int_cost"] > 0
    assert np.isclose(out["est_outcome_goal"], 30.0, atol=1e-3)


def test_optimize_tool_confidence_set_fields(mtcars_records):
    """With the confidence set on, the extra fields are present and JSON-safe."""
    out = S.optimize(
        data_records=mtcars_records,
        outcome_goal=30.0,
        include_confidence_set=True,
        confidence_set_grid_step_size=[1.0, 5.0],
        **_common_args(),
    )
    assert "error" not in out, out
    assert isinstance(out["confidence_set_size_percentage"], float)
    assert isinstance(out["est_outcome_ci"], dict)
    assert set(out["est_outcome_ci"].keys()) == {"lower", "upper"}
    if "confidence_set" in out:
        assert isinstance(out["confidence_set"], list)


# --------------------------------------------------------------------------
# 2. data input handling: records vs csv equivalence; both / neither error
# --------------------------------------------------------------------------
def test_data_records_and_csv_agree(mtcars_records, tmp_path):
    import pandas as pd

    csv_path = tmp_path / "mtcars.csv"
    pd.DataFrame.from_records(mtcars_records).to_csv(csv_path, index=False)

    from_records = S.optimize(
        data_records=mtcars_records,
        outcome_goal=30.0,
        include_confidence_set=False,
        **_common_args(),
    )
    from_csv = S.optimize(
        data_csv=str(csv_path),
        outcome_goal=30.0,
        include_confidence_set=False,
        **_common_args(),
    )
    assert "error" not in from_records and "error" not in from_csv
    assert np.allclose(from_records["rec_int"], from_csv["rec_int"])
    assert np.isclose(from_records["rec_int_cost"], from_csv["rec_int_cost"])
    assert np.isclose(
        from_records["est_outcome_goal"], from_csv["est_outcome_goal"]
    )


def test_optimize_both_data_inputs_errors(mtcars_records, tmp_path):
    import pandas as pd

    csv_path = tmp_path / "mtcars.csv"
    pd.DataFrame.from_records(mtcars_records).to_csv(csv_path, index=False)
    out = S.optimize(
        data_records=mtcars_records,
        data_csv=str(csv_path),
        outcome_goal=30.0,
        **_common_args(),
    )
    assert isinstance(out, dict) and "error" in out
    assert "exactly one" in out["error"]


def test_optimize_neither_data_input_errors():
    out = S.optimize(outcome_goal=30.0, **_common_args())
    assert isinstance(out, dict) and "error" in out
    assert "exactly one" in out["error"]


def test_sensitivity_neither_data_input_errors():
    out = S.sensitivity(
        parameter="outcome_goal",
        values=[30.0, 35.0],
        outcome_goal=30.0,
        **_common_args(),
    )
    assert isinstance(out, dict) and "error" in out
    assert "exactly one" in out["error"]


# --------------------------------------------------------------------------
# 3. sensitivity tool: records that vary; cost_multiplier argmin-invariance
# --------------------------------------------------------------------------
def test_sensitivity_tool_outcome_goal(mtcars_records):
    rows = S.sensitivity(
        data_records=mtcars_records,
        parameter="outcome_goal",
        values=[30.0, 35.0, 40.0],
        **_common_args(),
    )
    assert isinstance(rows, list) and len(rows) == 3
    for r in rows:
        assert set(("value", "rec_int_cost", "est_outcome_goal", "status")) <= set(r)
        assert r["status"] == "ok"
        assert isinstance(r["rec_int_cost"], float)
    # non-vacuous: a stricter outcome goal costs strictly more here
    costs = [r["rec_int_cost"] for r in rows]
    assert costs[0] < costs[1] < costs[2]
    # the estimated outcome tracks the swept goal
    assert np.allclose([r["est_outcome_goal"] for r in rows], [30, 35, 40], atol=1e-3)


def test_sensitivity_cost_multiplier_argmin_invariant(mtcars_records):
    mult = [0.8, 1.0, 1.2]
    rows = S.sensitivity(
        data_records=mtcars_records,
        parameter="cost_multiplier",
        values=mult,
        outcome_goal=30.0,
        **_common_args(),
    )
    assert isinstance(rows, list) and len(rows) == 3
    assert all(r["status"] == "ok" for r in rows)
    # a uniform cost rescaling never changes which intervention is cheapest:
    # the recommended value per component is invariant across multipliers.
    comps = ["gear", "qsec"]
    recs = [[r[c] for c in comps] for r in rows]
    assert np.allclose(recs[0], recs[1], atol=1e-4)
    assert np.allclose(recs[1], recs[2], atol=1e-4)
    # and the cost scales linearly with the multiplier off the baseline (1.0).
    base = rows[1]["rec_int_cost"]
    assert np.isclose(rows[0]["rec_int_cost"], base * 0.8, rtol=1e-4)
    assert np.isclose(rows[2]["rec_int_cost"], base * 1.2, rtol=1e-4)


def test_sensitivity_bad_parameter_returns_error(mtcars_records):
    """A non-sweepable parameter surfaces a clean error, not a crash."""
    out = S.sensitivity(
        data_records=mtcars_records,
        parameter="intervention_lower_bounds",  # vector-valued -> rejected
        values=[1.0, 2.0],
        outcome_goal=30.0,
        **_common_args(),
    )
    assert isinstance(out, dict) and "error" in out


# --------------------------------------------------------------------------
# 4. registration / schema (no R needed)
# --------------------------------------------------------------------------
def test_tools_registered_with_expected_schemas():
    tools = asyncio.run(S.mcp.list_tools())
    by_name = {t.name: t for t in tools}
    assert set(by_name) == {"optimize", "sensitivity"}

    core = {
        "outcome_name",
        "outcome_type",
        "intervention_components",
        "intervention_lower_bounds",
        "intervention_upper_bounds",
        "cost_list_of_vectors",
        "data_csv",
        "data_records",
        "outcome_goal",
    }
    opt_props = set(by_name["optimize"].inputSchema["properties"])
    assert core <= opt_props
    # a non-empty description (the agent-facing tool doc)
    assert by_name["optimize"].description

    sens_props = set(by_name["sensitivity"].inputSchema["properties"])
    assert core <= sens_props
    # sensitivity adds the sweep controls, and they are required
    assert {"parameter", "values"} <= sens_props
    sens_required = set(by_name["sensitivity"].inputSchema.get("required", []))
    assert {"parameter", "values"} <= sens_required

    # the vector params carry an array schema (FastMCP derives it from the hints)
    comp_schema = by_name["optimize"].inputSchema["properties"][
        "intervention_components"
    ]
    assert comp_schema.get("type") == "array"


# --------------------------------------------------------------------------
# 5. call_tool serialization boundary: the JSON-RPC path an MCP client uses
# --------------------------------------------------------------------------
def test_call_tool_optimize_round_trips_json(mtcars_records):
    # The execution tests above call the tool callables directly. This exercises
    # the FastMCP call_tool boundary an actual MCP client goes through, so a
    # future non-JSON-serializable return would be caught here rather than only
    # in a live agent call.
    args = dict(
        data_records=mtcars_records,
        outcome_goal=30.0,
        include_confidence_set=True,
        confidence_set_grid_step_size=[1.0, 1.0],
        **_common_args(),
    )
    result = asyncio.run(S.mcp.call_tool("optimize", args))

    # FastMCP returns the tool content; find the JSON text and parse it. The
    # shape varies by SDK version (a content list, or a (content, structured)
    # tuple), so pull text out of whatever came back.
    content = result[0] if isinstance(result, tuple) else result
    text = None
    for item in content:
        t = getattr(item, "text", None)
        if t is not None:
            text = t
            break
    assert text is not None, result
    import json as _json

    payload = _json.loads(text)  # must be valid JSON (no NaN/Infinity tokens)
    assert "error" not in payload, payload
    assert isinstance(payload["rec_int"], list) and len(payload["rec_int"]) == 2
    assert isinstance(payload["rec_int_cost"], float)
    assert "confidence_set" in payload
