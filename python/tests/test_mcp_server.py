"""Tests for the LAGOtrials MCP server (lago.mcp_server).

These test the tool FUNCTIONS at the function/registration level. The blocking
stdio server (mcp.run()) is intentionally NOT launched: it would block on
stdin/stdout forever. Instead we call the registered tool callables directly
(FastMCP's ``@mcp.tool()`` returns the original function unchanged) and inspect
the FastMCP tool registry for the input schemas.

Like the wrapper tests, the tool-execution tests drive the REAL LAGOtrials R
functions through rpy2 and reuse the shared ``ro`` fixture (see conftest.py), so
they skip cleanly when R / rpy2 / LAGOtrials cannot be reached. The whole module
skips if the ``mcp`` SDK is not installed.
"""
import asyncio
import json
import os

import numpy as np
import pytest

# Skip the entire module if the optional mcp SDK is not installed, except in
# CI (LAGO_REQUIRE_R=1), where it must be there.
if os.environ.get("LAGO_REQUIRE_R") != "1":
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


@pytest.fixture(scope="module")
def mtcars_wt_records(ro):
    """mtcars with wt too, to use as a center characteristic."""
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    mt_r = ro.r("mtcars")
    with localconverter(ro.default_converter + pandas2ri.converter):
        mt = ro.conversion.get_conversion().rpy2py(mt_r)
    return mt[["mpg", "gear", "qsec", "wt"]].to_dict(orient="records")


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
    )["runs"]
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
    )["runs"]
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
    assert set(by_name) == {"describe_data", "optimize", "sensitivity", "budget"}
    # each model tool explains the shared model arguments, and the server
    # tells the agent how to use the tools
    for name in ("optimize", "sensitivity", "budget"):
        assert "Costs: EXACTLY ONE of" in by_name[name].description
        assert "{model}" not in by_name[name].description
    assert "Report every entry of `warnings`" in S.mcp.instructions

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


# --------------------------------------------------------------------------
# 6. every call works in any thread or task, not only the first one
# --------------------------------------------------------------------------
def _in_new_thread(fn):
    """Run fn in a fresh thread, which starts with an empty context."""
    import threading

    out = {}

    def target():
        try:
            out["value"] = fn()
        except Exception as exc:  # pragma: no cover - reported by the assert
            out["value"] = {"error": repr(exc)}

    t = threading.Thread(target=target)
    t.start()
    t.join()
    return out["value"]


def test_tools_work_in_fresh_threads_and_tasks(mtcars_records):
    # rpy2 keeps its conversion rules in a context variable, so a thread or a
    # task that did not load rpy2 has none. The MCP server runs each call in a
    # context of its own, and used to answer only its first call.
    for _ in range(2):
        res = _in_new_thread(lambda: S.optimize(data_records=mtcars_records, outcome_goal=30.0, **_common_args()))
        assert "error" not in res, res
        assert len(res["rec_int"]) == 2
    res = _in_new_thread(lambda: asyncio.run(S.mcp.call_tool("sensitivity", dict(
        data_records=mtcars_records, parameter="outcome_goal", values=[30.0, 35.0], **_common_args()
    ))))
    content = res[0] if isinstance(res, tuple) else res
    payload = json.loads(next(getattr(c, "text") for c in content if getattr(c, "text", None)))
    assert "error" not in payload, payload
    assert len(payload["runs"]) == 2


# --------------------------------------------------------------------------
# 7. the warnings of the R package reach the tool result
# --------------------------------------------------------------------------
def test_tool_results_carry_the_r_warnings(mtcars_wt_records):
    mtcars_records = mtcars_wt_records
    args = _common_args()
    res = S.optimize(
        data_records=mtcars_records, outcome_goal=30.0,
        center_characteristics=["wt"], center_characteristics_optimization_values=[20.0], **args
    )
    assert "error" not in res, res
    assert any("outside the centers in the data" in w for w in res["warnings"])
    # each warning once, and none when there is nothing to warn about
    assert len(res["warnings"]) == len(set(res["warnings"]))
    quiet = S.optimize(data_records=mtcars_records, outcome_goal=30.0, **args)
    assert not any("outside the centers" in w for w in quiet["warnings"])
    sens = S.sensitivity(
        data_records=mtcars_records, parameter="outcome_goal", values=[30.0, 35.0],
        center_characteristics=["wt"], center_characteristics_optimization_values=[20.0], **args
    )
    assert sum("outside the centers" in w for w in sens["warnings"]) == 1


# --------------------------------------------------------------------------
# 8. budget tool
# --------------------------------------------------------------------------
def test_budget_tool(mtcars_records):
    opt = S.optimize(data_records=mtcars_records, outcome_goal=30.0, **_common_args())
    res = S.budget(data_records=mtcars_records, budget=opt["rec_int_cost"], n_grid=6, **_common_args())
    assert "error" not in res, res
    assert res["feasible"] is True
    assert isinstance(res["binding"], bool)
    assert len(res["rec_int"]) == 2
    assert res["rec_int_cost"] <= opt["rec_int_cost"] + 1e-6
    assert res["frontier"] and {"outcome_goal", "rec_int_cost", "est_outcome", "affordable"} <= set(res["frontier"][0])
    assert isinstance(res["warnings"], list)
    json.dumps(res)  # strict JSON
    # a budget below every reachable cost is not feasible, and says so in JSON
    tiny = S.budget(data_records=mtcars_records, budget=1e-6, n_grid=4, **_common_args())
    assert "error" not in tiny, tiny
    assert tiny["feasible"] is False
    assert tiny["rec_int_cost"] is None
    json.dumps(tiny, allow_nan=False)
    bad = S.budget(data_records=mtcars_records, budget=-1.0, **_common_args())
    assert "error" in bad and "budget" in bad["error"]


# --------------------------------------------------------------------------
# 9. describe_data, and the model arguments of every lago_optimization() kind
# --------------------------------------------------------------------------
@pytest.fixture(scope="module")
def bb_csv(ro, tmp_path_factory):
    path = tmp_path_factory.mktemp("bb") / "bb.csv"
    ro.r(f'write.csv(LAGOtrials::BB_data, "{path}", row.names = FALSE)')
    return str(path)


@pytest.fixture(scope="module")
def pulesa_csv(ro, tmp_path_factory):
    path = tmp_path_factory.mktemp("pulesa") / "pulesa.csv"
    ro.r(f'write.csv(LAGOtrials::main_pulesa_data, "{path}", row.names = FALSE)')
    return str(path)


def _r_rec(ro, call):
    """rec_int of a lago_optimization() call written in R, for comparison."""
    return list(ro.r(f"suppressWarnings(suppressMessages({call}))$rec_int"))


BB_ARGS = dict(
    outcome_name="pp3_oxytocin_mother", outcome_type="binary",
    intervention_components=["coaching_updt", "launch_duration"],
    intervention_lower_bounds=[1.0, 1.0], intervention_upper_bounds=[40.0, 5.0],
)


def test_describe_data(bb_csv):
    res = S.describe_data(data_csv=bb_csv)
    assert res["n_rows"] == 6124
    by = {c["name"]: c for c in res["columns"]}
    assert by["pp3_oxytocin_mother"]["binary"] is True
    assert by["birth_volume_100"]["min"] == pytest.approx(0.6025)
    assert by["birth_volume_100"]["max"] == pytest.approx(7.5)
    assert by["site_name"]["type"] == "text"
    json.dumps(res, allow_nan=False)
    assert "error" in S.describe_data()


def test_unit_costs_and_the_cost_choice(ro, bb_csv):
    res = S.optimize(
        data_csv=bb_csv, unit_costs=[1700.0, 8000.0], default_cost_fxn_type="linear",
        outcome_goal=0.85, include_confidence_set=False, **BB_ARGS
    )
    assert "error" not in res, res
    want = _r_rec(ro, 'LAGOtrials::lago_optimization(data = as.data.frame(LAGOtrials::BB_data), outcome_name = "pp3_oxytocin_mother", outcome_type = "binary", intervention_components = c("coaching_updt", "launch_duration"), intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5), unit_costs = c(1700, 8000), default_cost_fxn_type = "linear", outcome_goal = 0.85, include_confidence_set = FALSE, quiet = TRUE)')
    assert np.allclose(res["rec_int"], want, atol=1e-5)
    both = S.optimize(data_csv=bb_csv, unit_costs=[1.0, 1.0], cost_list_of_vectors=[[0, 1], [0, 1]], outcome_goal=0.85, **BB_ARGS)
    assert "exactly one of `cost_list_of_vectors` or `unit_costs`" in both["error"]
    neither = S.optimize(data_csv=bb_csv, outcome_goal=0.85, **BB_ARGS)
    assert "exactly one of" in neither["error"]


def test_named_columns_build_what_the_package_reads(ro, bb_csv):
    # center effects for one center, from a column the caller names
    res = S.optimize(
        data_csv=bb_csv, cost_list_of_vectors=[[0, 1700], [0, 8000]], outcome_goal=0.85,
        center_column="site_name", include_center_effects=True, center_effects_optimization_values="Auras",
        include_confidence_set=False, **BB_ARGS
    )
    assert "error" not in res, res
    want = _r_rec(ro, 'local({ d <- as.data.frame(LAGOtrials::BB_data); d$center <- factor(d$site_name); LAGOtrials::lago_optimization(data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary", intervention_components = c("coaching_updt", "launch_duration"), intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5), cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), outcome_goal = 0.85, include_center_effects = TRUE, center_effects_optimization_values = "Auras", include_confidence_set = FALSE, quiet = TRUE) })')
    assert np.allclose(res["rec_int"], want, atol=1e-5)
    bad = S.optimize(data_csv=bb_csv, cost_list_of_vectors=[[0, 1], [0, 1]], outcome_goal=0.85, center_column="nope", **BB_ARGS)
    assert "the center column 'nope' is not a column of the data" in bad["error"]


def test_a_power_goal_from_a_named_arm_column(ro, bb_csv):
    res = S.optimize(
        data_csv=bb_csv, cost_list_of_vectors=[[0, 1700], [0, 8000]], power_goal=0.8,
        group_column="pre_post", treatment_value=1, num_centers_in_next_stage=10,
        patients_per_center_in_next_stage=30, include_confidence_set=False, **BB_ARGS
    )
    assert "error" not in res, res
    want = _r_rec(ro, 'local({ d <- as.data.frame(LAGOtrials::BB_data); d$group <- ifelse(as.character(d$pre_post) == "1", "treatment", "control"); LAGOtrials::lago_optimization(data = d, outcome_name = "pp3_oxytocin_mother", outcome_type = "binary", intervention_components = c("coaching_updt", "launch_duration"), intervention_lower_bounds = c(1, 1), intervention_upper_bounds = c(40, 5), cost_list_of_vectors = list(c(0, 1700), c(0, 8000)), power_goal = 0.8, num_centers_in_next_stage = 10, patients_per_center_in_next_stage = 30, include_confidence_set = FALSE, quiet = TRUE) })')
    assert np.allclose(res["rec_int"], want, atol=1e-5)
    assert "effective_outcome_goal" in res
    missing = S.optimize(data_csv=bb_csv, cost_list_of_vectors=[[0, 1], [0, 1]], power_goal=0.8, group_column="pre_post", **BB_ARGS)
    assert "`group_column` needs `treatment_value`" in missing["error"]


def test_center_level_data_from_successes_and_totals(ro, pulesa_csv):
    common = dict(
        data_csv=pulesa_csv, outcome_name="Success", outcome_type="binary",
        intervention_components=["AccessMedicines", "DeliveryA"], intervention_lower_bounds=[0.0, 0.0],
        intervention_upper_bounds=[4.0, 1.0], unit_costs=[1.0, 1.0], outcome_goal=0.6,
        input_data_structure="center_level", center_column="Clinic", total_column="Total_visit",
        period_column="Period", include_time_effects=True, time_effect_optimization_value=13,
    )
    res = S.optimize(successes_column="Success", include_confidence_set=False, **common)
    assert "error" not in res, res
    want = _r_rec(ro, 'local({ d <- as.data.frame(LAGOtrials::main_pulesa_data); d$proportion <- d$Success / d$Total_visit; d$center_sample_size <- d$Total_visit; d$center <- factor(d$Clinic); d$period <- factor(d$Period); LAGOtrials::lago_optimization(data = d, input_data_structure = "center_level", outcome_name = "proportion", outcome_type = "binary", intervention_components = c("AccessMedicines", "DeliveryA"), intervention_lower_bounds = c(0, 0), intervention_upper_bounds = c(4, 1), unit_costs = c(1, 1), outcome_goal = 0.6, include_time_effects = TRUE, time_effect_optimization_value = 13, include_confidence_set = FALSE, quiet = TRUE) })')
    assert np.allclose(res["rec_int"], want, atol=1e-5)
    # the proportion column gives the same model
    prop = S.optimize(proportion_column="Proportions", include_confidence_set=False, **common)
    assert np.allclose(prop["rec_int"], want, atol=1e-4)
    bad = S.optimize(successes_column="Success", **{**common, "total_column": None})
    assert "`successes_column` needs `total_column`" in bad["error"]


def test_interactions_grid_search_level_and_weights(ro, mtcars_wt_records):
    records = [dict(r, **{"gear:qsec": r["gear"] * r["qsec"]}) for r in mtcars_wt_records]
    res = S.optimize(
        data_records=records, outcome_name="mpg", outcome_type="continuous",
        intervention_components=["gear", "qsec", "gear:qsec"], main_components=["gear", "qsec"],
        include_interaction_terms=True, intervention_lower_bounds=[3.0, 14.0], intervention_upper_bounds=[5.0, 20.0],
        unit_costs=[2.0, 1.0], default_cost_fxn_type="linear", outcome_goal=25.0,
        optimization_method="grid_search", optimization_grid_search_step_size=[0.5, 1.0],
        weights_column="wt", confidence_set_alpha=0.1, confidence_set_grid_step_size=[0.5, 1.0],
    )
    assert "error" not in res, res
    assert res["confidence_level"] == pytest.approx(0.9)
    want = _r_rec(ro, 'local({ d <- mtcars; d[["gear:qsec"]] <- d$gear * d$qsec; LAGOtrials::lago_optimization(data = d, outcome_name = "mpg", outcome_type = "continuous", intervention_components = c("gear", "qsec", "gear:qsec"), main_components = c("gear", "qsec"), include_interaction_terms = TRUE, intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 20), unit_costs = c(2, 1), default_cost_fxn_type = "linear", outcome_goal = 25, optimization_method = "grid_search", optimization_grid_search_step_size = c(0.5, 1), weights = d$wt, confidence_set_alpha = 0.1, confidence_set_grid_step_size = c(0.5, 1), quiet = TRUE) })')
    assert np.allclose(res["rec_int"], want, atol=1e-5)


def test_budget_and_sensitivity_take_the_model_arguments(bb_csv):
    common = dict(data_csv=bb_csv, unit_costs=[1700.0, 8000.0], default_cost_fxn_type="linear",
                  center_column="site_name", include_center_effects=True, **BB_ARGS)
    b = S.budget(budget=30000.0, n_grid=4, **common)
    assert "error" not in b, b
    assert b["feasible"] is True
    sw = S.sensitivity(parameter="outcome_goal", values=[0.8, 0.85], **common)
    assert "error" not in sw, sw
    assert [r["status"] for r in sw["runs"]] == ["ok", "ok"]


def test_an_r_error_gives_its_message_not_the_call(bb_csv):
    res = S.optimize(
        data_csv=bb_csv, cost_list_of_vectors=[[0, 1700], [0, 8000]], outcome_goal=0.85,
        center_column="site_name", include_center_effects=True, center_effects_optimization_values="Nowhere",
        include_confidence_set=False, **BB_ARGS
    )
    assert "Nowhere" in res["error"]
    assert "must be one of the centers" in res["error"]
    assert "Error in (function" not in res["error"]


# --------------------------------------------------------------------------
# 10. review fixes: the arm column, sweeps that miss their goal, threads,
#     true/false columns and center level columns
# --------------------------------------------------------------------------
def test_a_treatment_value_matches_however_it_is_given(bb_csv):
    common = dict(data_csv=bb_csv, cost_list_of_vectors=[[0, 1700], [0, 8000]], power_goal=0.8,
                  group_column="pre_post", num_centers_in_next_stage=10, patients_per_center_in_next_stage=30,
                  include_confidence_set=False, **BB_ARGS)
    costs = []
    for value in (1, 1.0, "1"):
        res = S.optimize(treatment_value=value, **common)
        assert "error" not in res, (value, res)
        costs.append(res["rec_int_cost"])
    assert costs[0] == costs[1] == costs[2]
    wrong = S.optimize(treatment_value=7, **common)
    assert "`treatment_value` 7 is not a value of the group column 'pre_post', whose values are 0, 1" in wrong["error"]
    text = S.optimize(treatment_value="yes", **common)
    assert "is not a number, while the group column 'pre_post' holds numbers" in text["error"]


def test_a_group_column_with_missing_values_is_refused(ro, tmp_path):
    path = tmp_path / "bb_na.csv"
    ro.r(f'local({{ d <- LAGOtrials::BB_data; d$pre_post[1:500] <- NA; write.csv(d, "{path}", row.names = FALSE) }})')
    res = S.optimize(
        data_csv=str(path), cost_list_of_vectors=[[0, 1700], [0, 8000]], power_goal=0.8, group_column="pre_post",
        treatment_value=1, num_centers_in_next_stage=10, patients_per_center_in_next_stage=30, **BB_ARGS
    )
    assert "the group column 'pre_post' has 500 rows with no value" in res["error"]


def test_each_sweep_run_says_whether_it_missed_its_goal_or_failed(bb_csv):
    common = dict(data_csv=bb_csv, cost_list_of_vectors=[[0, 1700], [0, 8000]], **BB_ARGS)
    res = S.sensitivity(parameter="outcome_goal", values=[0.8, 0.999], **common)
    assert "error" not in res, res
    runs = res["runs"]
    assert [r["shrinking_method_used"] for r in runs] == [False, True]
    # with no power goal there is no other goal to record
    assert runs[0]["effective_outcome_goal"] is None
    assert all(r["error_message"] is None for r in runs)
    failed = S.sensitivity(parameter="power_goal", values=[0.8, 1.5], group_column="pre_post", treatment_value=1,
                           num_centers_in_next_stage=10, patients_per_center_in_next_stage=30, **common)
    assert "error" not in failed, failed
    bad = [r for r in failed["runs"] if r["status"] == "error"]
    assert len(bad) == 1 and bad[0]["value"] == 1.5 and bad[0]["error_message"]
    good = [r for r in failed["runs"] if r["status"] == "ok"]
    assert isinstance(good[0]["effective_outcome_goal"], float)
    json.dumps(failed, allow_nan=False)


def test_calls_from_many_threads_at_once_are_safe(mtcars_records):
    import sys
    import threading

    broken = []
    old_hook = sys.unraisablehook
    sys.unraisablehook = lambda info: broken.append(repr(info.exc_value))
    try:
        results = [None] * 6

        def call(i):
            results[i] = S.optimize(data_records=mtcars_records, outcome_goal=30.0, **_common_args())

        threads = [threading.Thread(target=call, args=(i,)) for i in range(6)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()
        import gc

        gc.collect()
    finally:
        sys.unraisablehook = old_hook
    assert all("error" not in r for r in results), results
    assert len({round(r["rec_int_cost"], 8) for r in results}) == 1
    # rpy2's table of the R objects it keeps alive stayed whole
    assert broken == []


def test_describe_data_sees_a_true_false_column(mtcars_records):
    records = [dict(r, good=r["mpg"] > 20) for r in mtcars_records]
    cols = {c["name"]: c for c in S.describe_data(data_records=records)["columns"]}
    assert cols["good"]["type"] == "logical"
    assert cols["good"]["binary"] is True


def test_center_level_columns_need_the_structure(pulesa_csv):
    res = S.optimize(
        data_csv=pulesa_csv, outcome_name="Success", outcome_type="binary", intervention_components=["DeliveryA"],
        intervention_lower_bounds=[0.0], intervention_upper_bounds=[1.0], unit_costs=[1.0], outcome_goal=0.6,
        successes_column="Success", total_column="Total_visit",
    )
    assert '`successes_column`, `total_column` describe center level data. Also give `input_data_structure="center_level"`' in res["error"]



# --------------------------------------------------------------------------
# 11. bug bash: centers and periods by number, true/false arms, center level
#     outcomes, built products, non-finite inputs, and a responsive server
# --------------------------------------------------------------------------
def _numbered_centers(bb_csv, tmp_path, as_float=False):
    import pandas as pd

    d = pd.read_csv(bb_csv)
    ids = {name: i + 1 for i, name in enumerate(sorted(d["site_name"].unique()))}
    d["site_id"] = d["site_name"].map(ids).astype(float if as_float else int)
    d["center"] = d["site_id"]
    path = tmp_path / "numbered.csv"
    d.to_csv(path, index=False)
    return str(path), len(ids)


def test_center_weights_reach_the_same_centers_whatever_the_column_is_named(bb_csv, tmp_path):
    path, n = _numbered_centers(bb_csv, tmp_path)
    weights = [0.0] * n
    weights[1] = 1.0  # all weight on the second center, center 2
    common = dict(data_csv=path, cost_list_of_vectors=[[0, 1700], [0, 8000]], outcome_goal=0.85,
                  include_center_effects=True, center_weights_for_outcome_goal=weights,
                  include_confidence_set=False, **BB_ARGS)
    named_center = S.optimize(center_column="center", **common)
    named_other = S.optimize(center_column="site_id", **common)
    only_two = S.optimize(data_csv=path, cost_list_of_vectors=[[0, 1700], [0, 8000]], outcome_goal=0.85,
                          center_column="site_id", include_center_effects=True, center_effects_optimization_values="2",
                          include_confidence_set=False, **BB_ARGS)
    for res in (named_center, named_other, only_two):
        assert "error" not in res, res
    assert named_center["rec_int_cost"] == pytest.approx(named_other["rec_int_cost"])
    assert named_other["rec_int_cost"] == pytest.approx(only_two["rec_int_cost"])


def test_float_centers_and_periods_match_their_numbers(bb_csv, tmp_path):
    path, _ = _numbered_centers(bb_csv, tmp_path, as_float=True)
    res = S.optimize(data_csv=path, cost_list_of_vectors=[[0, 1700], [0, 8000]], outcome_goal=0.85,
                     center_column="site_id", include_center_effects=True, center_effects_optimization_values="2",
                     include_confidence_set=False, **BB_ARGS)
    assert "error" not in res, res
    import pandas as pd

    p = pd.read_csv(path)
    p["phase"] = p["pilot_phase"].astype(float)
    p.to_csv(path, index=False)
    res = S.optimize(data_csv=path, cost_list_of_vectors=[[0, 1700], [0, 8000]], outcome_goal=0.85,
                     period_column="phase", include_time_effects=True, time_effect_optimization_value=2,
                     include_confidence_set=False, **BB_ARGS)
    assert "error" not in res, res


def test_a_true_false_arm_column_takes_true_however_it_arrives(bb_csv, tmp_path):
    import pandas as pd

    d = pd.read_csv(bb_csv)
    d["treated"] = d["pre_post"] == 1
    path = tmp_path / "bool_arm.csv"
    d.to_csv(path, index=False)
    common = dict(data_csv=str(path), cost_list_of_vectors=[[0, 1700], [0, 8000]], power_goal=0.8,
                  group_column="treated", num_centers_in_next_stage=10, patients_per_center_in_next_stage=30,
                  include_confidence_set=False, **BB_ARGS)
    costs = {str(v): S.optimize(treatment_value=v, **common).get("rec_int_cost") for v in (1.0, "true", "TRUE", "True")}
    assert None not in costs.values(), costs
    assert len(set(costs.values())) == 1
    assert "is not true or false" in S.optimize(treatment_value="maybe", **common)["error"]


def test_center_level_data_uses_the_outcome_it_is_given(ro, tmp_path):
    path = tmp_path / "cl.csv"
    ro.r(f'local({{ d <- LAGOtrials::main_pulesa_data; d$proportion <- 0.5; d$share <- d$Success / d$Total_visit; write.csv(d, "{path}", row.names = FALSE) }})')
    common = dict(data_csv=str(path), outcome_type="binary", intervention_components=["DeliveryA"],
                  intervention_lower_bounds=[0.0], intervention_upper_bounds=[1.0], unit_costs=[1.0], outcome_goal=0.6,
                  input_data_structure="center_level", total_column="Total_visit", center_column="Clinic",
                  include_confidence_set=False)
    by_name = S.optimize(outcome_name="share", **common)
    by_column = S.optimize(outcome_name="anything", proportion_column="share", **common)
    assert "error" not in by_name, by_name
    # the existing proportion column of 0.5 is not what was fitted
    assert by_name["rec_int_cost"] == pytest.approx(by_column["rec_int_cost"])


def test_an_interaction_product_is_built_when_the_data_has_none(ro, mtcars_records):
    res = S.optimize(
        data_records=mtcars_records, outcome_name="mpg", outcome_type="continuous",
        intervention_components=["gear", "qsec", "gear:qsec"], main_components=["gear", "qsec"],
        include_interaction_terms=True, intervention_lower_bounds=[3.0, 14.0], intervention_upper_bounds=[5.0, 20.0],
        unit_costs=[2.0, 1.0], default_cost_fxn_type="linear", outcome_goal=25.0, include_confidence_set=False,
    )
    assert "error" not in res, res
    want = _r_rec(ro, 'local({ d <- mtcars; d[["gear:qsec"]] <- d$gear * d$qsec; LAGOtrials::lago_optimization(data = d, outcome_name = "mpg", outcome_type = "continuous", intervention_components = c("gear", "qsec", "gear:qsec"), main_components = c("gear", "qsec"), include_interaction_terms = TRUE, intervention_lower_bounds = c(3, 14), intervention_upper_bounds = c(5, 20), unit_costs = c(2, 1), default_cost_fxn_type = "linear", outcome_goal = 25, include_confidence_set = FALSE, quiet = TRUE) })')
    assert np.allclose(res["rec_int"], want, atol=1e-5)


def test_non_finite_inputs_get_a_clear_message(mtcars_records):
    args = _common_args()
    args["intervention_upper_bounds"] = [10.0, float("nan")]
    assert "`intervention_upper_bounds` must hold finite numbers" in S.optimize(data_records=mtcars_records, outcome_goal=30.0, **args)["error"]
    args = _common_args()
    args["cost_list_of_vectors"] = [[0.0, float("inf")], [4.0, 6.0]]
    assert "`cost_list_of_vectors` must hold finite numbers" in S.optimize(data_records=mtcars_records, outcome_goal=30.0, **args)["error"]


def test_the_server_answers_while_a_tool_runs(mtcars_records):
    import time

    async def scenario():
        started = time.monotonic()
        call = asyncio.create_task(S.mcp.call_tool("budget", dict(data_records=mtcars_records, budget=200.0, n_grid=8, **{k: v for k, v in _common_args().items()})))
        await asyncio.sleep(0.05)
        listed = await S.mcp.list_tools()
        answered_after = time.monotonic() - started
        await call
        return answered_after, time.monotonic() - started, len(listed)

    answered, finished, n_tools = asyncio.run(scenario())
    assert n_tools == 4
    # the list came back while the budget search was still running
    assert answered < finished


# --------------------------------------------------------------------------
# 12. a cancelled call stops its R work, and the server carries on
# --------------------------------------------------------------------------
def test_a_cancelled_call_stops_and_the_next_call_works(mtcars_records):
    import time

    args = dict(data_records=mtcars_records, budget=200.0, n_grid=25, **_common_args())

    async def scenario():
        call = asyncio.create_task(S.mcp.call_tool("budget", args))
        await asyncio.sleep(1.0)
        proc = S._WORKER._proc
        started = time.monotonic()
        call.cancel()
        try:
            await call
        except asyncio.CancelledError:
            pass
        cancelled_in = time.monotonic() - started
        after = await S.mcp.call_tool("optimize", dict(data_records=mtcars_records, outcome_goal=30.0, **_common_args()))
        return proc, cancelled_in, after

    proc, cancelled_in, after = asyncio.run(scenario())
    assert proc is not None and not proc.is_alive()
    assert cancelled_in < 5
    content = after[0] if isinstance(after, tuple) else after
    payload = json.loads(next(getattr(c, "text") for c in content if getattr(c, "text", None)))
    assert "error" not in payload, payload
    S._WORKER.stop()
