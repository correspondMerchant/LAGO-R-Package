"""End-to-end tests for the lago-python wrapper.

Every test drives the REAL LAGO R functions through rpy2. There is no mocked
LAGO math: the wrapper's job is only to convert inputs/outputs, so the tests
prove the round trip and that the wrapper does not corrupt the call.

visualize_cost() is intentionally NOT launched here: it opens a blocking Shiny
browser app. Instead we test that the wrapper builds the correct R call. See
test_visualize_cost_builds_r_call and the README note.
"""
import numpy as np
import pandas as pd
import pytest

import lago
from lago import _bridge


# ---- shared optimization kwargs (BB_data, non-empty confidence set) --------

def _bb_kwargs():
    return dict(
        outcome_name="pp3_oxytocin_mother",
        outcome_type="binary",
        glm_family="binomial",
        intervention_components=["coaching_updt", "launch_duration"],
        center_characteristics=["birth_volume_100"],
        center_characteristics_optimization_values=1.75,
        intervention_lower_bounds=[1, 1],
        intervention_upper_bounds=[40, 5],
        cost_list=[[0, 1.7], [0, 8]],
        outcome_goal=0.85,
        outcome_goal_intention="maximize",
        confidence_set_grid_step_size=[1, 1],
        quiet=True,
    )


# --------------------------------------------------------------------------
# 1. Core round-trip proof
# --------------------------------------------------------------------------
def test_optimize_end_to_end(infert):
    res = lago.optimize(
        data=infert,
        outcome_name="case",
        outcome_type="binary",
        glm_family="binomial",
        intervention_components=["age", "parity"],
        intervention_lower_bounds=[0, 0],
        intervention_upper_bounds=[50, 10],
        cost_list=[[0, 4], [0, 1]],
        outcome_goal=0.5,
        outcome_goal_intention="maximize",
        confidence_set_grid_step_size=[1, 1],
        quiet=True,
    )
    assert isinstance(res, dict)
    rec = res["rec_int"]
    assert isinstance(rec, list)
    assert len(rec) == 2
    assert all(isinstance(v, float) for v in rec)
    assert isinstance(res["est_outcome_goal"], float)
    assert isinstance(res["rec_int_cost"], float)


# --------------------------------------------------------------------------
# 2. Wrapper does not corrupt the call: matches direct R
# --------------------------------------------------------------------------
def test_result_matches_R(ro, infert):
    res = lago.optimize(
        data=infert,
        outcome_name="case",
        outcome_type="binary",
        glm_family="binomial",
        intervention_components=["age", "parity"],
        intervention_lower_bounds=[0, 0],
        intervention_upper_bounds=[50, 10],
        cost_list=[[0, 4], [0, 1]],
        outcome_goal=0.5,
        outcome_goal_intention="maximize",
        confidence_set_grid_step_size=[1, 1],
        quiet=True,
    )
    ro.r(
        """
        res_r <- LAGOtrials::lago_optimization(
          data = infert,
          outcome_name = "case",
          outcome_type = "binary",
          glm_family = "binomial",
          intervention_components = c("age", "parity"),
          intervention_lower_bounds = c(0, 0),
          intervention_upper_bounds = c(50, 10),
          cost_list_of_vectors = list(c(0, 4), c(0, 1)),
          outcome_goal = 0.5,
          outcome_goal_intention = "maximize",
          confidence_set_grid_step_size = c(1, 1),
          quiet = TRUE
        )
        """
    )
    rec_int_r = list(ro.r("res_r$rec_int"))
    est_r = list(ro.r("res_r$est_outcome_goal"))[0]

    assert np.allclose(
        np.array(res["rec_int"], dtype=float),
        np.array(rec_int_r, dtype=float),
    )
    assert np.isclose(res["est_outcome_goal"], est_r)


# --------------------------------------------------------------------------
# 3. cost_list (python list-of-lists) reaches R correctly
# --------------------------------------------------------------------------
def test_cost_list_roundtrip(bb_data):
    cost_list = [[0, 1.7], [0, 8]]
    res = lago.optimize(data=bb_data, cost_list=cost_list, **{
        k: v for k, v in _bb_kwargs().items() if k != "cost_list"
    })
    # The run succeeded with the supplied cost_list, and LAGO echoes it back on
    # the result as cost_list_of_vectors (converted to a python list-of-lists).
    echoed = res["cost_list_of_vectors"]
    assert isinstance(echoed, list)
    assert len(echoed) == 2
    assert np.allclose(echoed[0], [0.0, 1.7])
    assert np.allclose(echoed[1], [0.0, 8.0])


def test_cost_list_converts_to_r_list_of_numeric(ro):
    """The Python list-of-lists becomes an R list of numeric vectors."""
    r_obj = _bridge.py_to_r([[0, 1.7], [0, 8]])
    assert list(ro.r["class"](r_obj))[0] == "list"
    assert list(ro.r["length"](r_obj))[0] == 2
    first = r_obj[0]
    assert list(ro.r["class"](first))[0] == "numeric"
    assert np.allclose(list(first), [0.0, 1.7])


# --------------------------------------------------------------------------
# Regression: r_to_py must NOT scalar-collapse semantic vectors, so a
# length-1 vector field keeps its list container. Genuine scalars are still
# unwrapped by name in the result marshaling.
# --------------------------------------------------------------------------
def _bb_single_kwargs():
    """A one-component BB_data optimization (the length-1 vector case)."""
    return dict(
        outcome_name="pp3_oxytocin_mother",
        outcome_type="binary",
        glm_family="binomial",
        intervention_components=["coaching_updt"],
        center_characteristics=["birth_volume_100"],
        center_characteristics_optimization_values=1.75,
        intervention_lower_bounds=[1],
        intervention_upper_bounds=[40],
        cost_list=[[0, 1.7]],
        outcome_goal=0.85,
        outcome_goal_intention="maximize",
        confidence_set_grid_step_size=[1],
        quiet=True,
    )


def test_single_component_rec_int_is_list(bb_data):
    """A single-component optimize() returns rec_int as a length-1 list of
    floats (not a bare float); iterating and indexing both work."""
    res = lago.optimize(data=bb_data, **_bb_single_kwargs())
    rec = res["rec_int"]
    assert isinstance(rec, list), f"rec_int should be a list, got {type(rec)}"
    assert len(rec) == 1
    assert all(isinstance(v, float) for v in rec)
    # indexing and iteration must work (a bare float would crash both)
    assert isinstance(res["rec_int"][0], float)
    assert [v for v in res["rec_int"]] == rec
    # the echoed length-1 bounds keep their list container too
    assert isinstance(res["intervention_lower_bounds"], list)
    assert res["intervention_lower_bounds"] == [1.0]
    assert isinstance(res["intervention_upper_bounds"], list)
    assert res["intervention_upper_bounds"] == [40.0]


def test_multi_component_rec_int_unchanged(ro, bb_data):
    """A 2-component run still returns a length-2 rec_int list and matches
    direct R (the fix must not change the multi-component happy path)."""
    res = lago.optimize(data=bb_data, **_bb_kwargs())
    rec = res["rec_int"]
    assert isinstance(rec, list)
    assert len(rec) == 2
    assert all(isinstance(v, float) for v in rec)

    ro.globalenv["bb_data_r"] = _bridge.py_to_r(bb_data)
    ro.r(
        """
        res_r <- LAGOtrials::lago_optimization(
          data = bb_data_r,
          outcome_name = "pp3_oxytocin_mother",
          outcome_type = "binary",
          glm_family = "binomial",
          intervention_components = c("coaching_updt", "launch_duration"),
          center_characteristics = c("birth_volume_100"),
          center_characteristics_optimization_values = 1.75,
          intervention_lower_bounds = c(1, 1),
          intervention_upper_bounds = c(40, 5),
          cost_list_of_vectors = list(c(0, 1.7), c(0, 8)),
          outcome_goal = 0.85,
          outcome_goal_intention = "maximize",
          confidence_set_grid_step_size = c(1, 1),
          quiet = TRUE
        )
        """
    )
    rec_int_r = list(ro.r("res_r$rec_int"))
    est_r = list(ro.r("res_r$est_outcome_goal"))[0]
    assert np.allclose(
        np.array(res["rec_int"], dtype=float),
        np.array(rec_int_r, dtype=float),
    )
    assert np.isclose(res["est_outcome_goal"], est_r)


def test_scalar_fields_are_scalars(bb_data):
    """Genuinely-scalar result fields stay python scalars, not length-1
    lists, in both the single- and multi-component cases."""
    for kwargs in (_bb_single_kwargs(), _bb_kwargs()):
        res = lago.optimize(data=bb_data, **kwargs)
        assert isinstance(res["est_outcome_goal"], float)
        assert not isinstance(res["est_outcome_goal"], list)
        assert isinstance(res["rec_int_cost"], float)
        assert isinstance(res["confidence_set_size_percentage"], float)
        # est_outcome_ci is a named c(lower, upper) -> stays a dict
        assert isinstance(res["est_outcome_ci"], dict)
        assert set(res["est_outcome_ci"].keys()) == {"lower", "upper"}


def test_cost_list_nesting_preserved(ro, bb_data):
    """r_to_py of an R list of length-1 vectors yields a list-of-lists (not a
    flattened list), and a degree-0 cost_list [[5.0],[3.0]] round-trips with
    its nesting intact through py_to_r as well."""
    # r_to_py must NOT flatten list(c(5.0), c(3.0)) to [5.0, 3.0]
    rlist = ro.r("list(c(5.0), c(3.0))")
    assert _bridge.r_to_py(rlist) == [[5.0], [3.0]]
    # a mixed-length list-of-vectors still round-trips
    rlist2 = ro.r("list(c(0, 1.7), c(0, 8))")
    assert _bridge.r_to_py(rlist2) == [[0.0, 1.7], [0.0, 8.0]]

    # py_to_r of [[5.0],[3.0]] reaches R as a list of TWO length-1 numeric
    # vectors (two constant-cost components), not one 2-coefficient vector.
    r_obj = _bridge.py_to_r([[5.0], [3.0]])
    assert list(ro.r["class"](r_obj))[0] == "list"
    assert list(ro.r["length"](r_obj))[0] == 2
    assert list(ro.r["length"](r_obj[0]))[0] == 1
    assert list(ro.r["length"](r_obj[1]))[0] == 1
    # full round-trip preserves nesting
    assert _bridge.r_to_py(r_obj) == [[5.0], [3.0]]

    # and it round-trips as the cost echo of a real single-component run
    res = lago.optimize(
        data=bb_data,
        outcome_name="pp3_oxytocin_mother",
        outcome_type="binary",
        glm_family="binomial",
        intervention_components=["coaching_updt"],
        center_characteristics=["birth_volume_100"],
        center_characteristics_optimization_values=1.75,
        intervention_lower_bounds=[1],
        intervention_upper_bounds=[40],
        cost_list=[[0, 1.7]],
        outcome_goal=0.85,
        outcome_goal_intention="maximize",
        confidence_set_grid_step_size=[1],
        quiet=True,
    )
    echoed = res["cost_list_of_vectors"]
    assert isinstance(echoed, list) and len(echoed) == 1
    assert isinstance(echoed[0], list)
    assert np.allclose(echoed[0], [0.0, 1.7])


# --------------------------------------------------------------------------
# 4. get_confidence_set basic call returns expected fields
# --------------------------------------------------------------------------
def test_get_confidence_set(bb_data):
    opt = lago.optimize(data=bb_data, include_confidence_set=False, **{
        k: v for k, v in _bb_kwargs().items()
        if k != "confidence_set_grid_step_size"
    })
    predictors = bb_data[
        ["coaching_updt", "launch_duration", "birth_volume_100"]
    ]
    cs = lago.get_confidence_set(
        predictors_data=predictors,
        intervention_components=["coaching_updt", "launch_duration"],
        outcome_data=list(bb_data["pp3_oxytocin_mother"]),
        fitted_model=opt["model"],
        link="logit",
        outcome_goal=0.85,
        outcome_type="binary",
        intervention_lower_bounds=[1, 1],
        intervention_upper_bounds=[40, 5],
        confidence_set_grid_step_size=[1, 1],
        center_characteristics=["birth_volume_100"],
        center_characteristics_optimization_values=1.75,
        cost_list=[[0, 1.7], [0, 8]],
        rec_int=opt["rec_int"],
    )
    assert isinstance(cs, dict)
    assert set(cs.keys()) == {
        "confidence_set_size_percentage", "rec_int_ci", "cs"
    }
    assert isinstance(cs["confidence_set_size_percentage"], float)
    # rec_int_ci converts to a dict with lower/upper
    assert isinstance(cs["rec_int_ci"], dict)
    assert set(cs["rec_int_ci"].keys()) == {"lower", "upper"}
    # the confidence set is a pandas DataFrame with the documented columns
    assert isinstance(cs["cs"], pd.DataFrame)
    for col in ("CI_lower_bound", "CI_upper_bound", "cost"):
        assert col in cs["cs"].columns


def test_optimize_confidence_set_is_dataframe(bb_data):
    res = lago.optimize(data=bb_data, **_bb_kwargs())
    assert isinstance(res["confidence_set_size_percentage"], float)
    assert isinstance(res["cs"], pd.DataFrame)
    assert isinstance(res["est_outcome_ci"], dict)


# --------------------------------------------------------------------------
# sensitivity(): mirrors optimize()'s arg handling and calls lago_sensitivity()
# --------------------------------------------------------------------------
def _bb_sensitivity_kwargs():
    """Baseline BB_data optimization args for a sensitivity sweep (the
    confidence-set-only args are dropped: the sweep forces it off)."""
    kw = _bb_kwargs()
    cost = kw.pop("cost_list")
    kw.pop("confidence_set_grid_step_size", None)
    return kw, cost


def test_sensitivity_outcome_goal_sweep(bb_data):
    """The sweep returns a DataFrame with one row per value, holding
    value / <component> / rec_int_cost / est_outcome_goal / status."""
    kw, cost = _bb_sensitivity_kwargs()
    values = [0.8, 0.85, 0.9]
    res = lago.sensitivity(
        data=bb_data,
        parameter="outcome_goal",
        values=values,
        cost_list=cost,
        **kw,
    )
    assert isinstance(res, pd.DataFrame)
    assert len(res) == len(values)
    for col in ("value", "rec_int_cost", "est_outcome_goal", "status"):
        assert col in res.columns
    # one recommended-value column per intervention component
    assert "coaching_updt" in res.columns
    assert "launch_duration" in res.columns
    assert np.allclose(res["value"].to_numpy(dtype=float), values)
    assert (res["status"] == "ok").all()
    # non-vacuous: costs are real, finite numbers
    assert res["rec_int_cost"].notna().all()
    assert np.isfinite(res["rec_int_cost"].to_numpy(dtype=float)).all()


def test_sensitivity_cost_multiplier_argmin_invariant(bb_data):
    """A uniform cost rescaling never changes which intervention is cheapest,
    so the recommendation per component is unchanged across the sweep and the
    cost scales linearly with the multiplier (same invariance as the R test)."""
    kw, cost = _bb_sensitivity_kwargs()
    mult = [0.8, 1.0, 1.2]
    res = lago.sensitivity(
        data=bb_data,
        parameter="cost_multiplier",
        values=mult,
        cost_list=cost,
        **kw,
    )
    assert isinstance(res, pd.DataFrame)
    assert len(res) == len(mult)
    assert (res["status"] == "ok").all()

    comps = ["coaching_updt", "launch_duration"]
    recs = res[comps].to_numpy(dtype=float)
    # recommendation per component is invariant to the uniform rescaling
    assert np.allclose(recs[0], recs[1], atol=1e-4)
    assert np.allclose(recs[1], recs[2], atol=1e-4)
    # cost scales linearly off the baseline (multiplier 1.0)
    costs = res["rec_int_cost"].to_numpy(dtype=float)
    base = costs[1]
    assert np.isclose(costs[0], base * 0.8, rtol=1e-4)
    assert np.isclose(costs[2], base * 1.2, rtol=1e-4)


# --------------------------------------------------------------------------
# 5. visualize_cost: NOT launched (blocking browser app). We verify the
#    wrapper builds the correct R call / converts args to the right R types.
# --------------------------------------------------------------------------
def test_visualize_cost_builds_r_call(ro):
    kwargs = dict(
        component_names=["Component 1", "Component 2"],
        unit_costs=[0.5, 1.0],
        default_cost_fxn_type="linear",
        intervention_lower_bounds=[0, 0],
        intervention_upper_bounds=[10, 10],
    )
    r_kwargs = _bridge.build_r_kwargs(kwargs)
    assert list(ro.r["class"](r_kwargs["component_names"]))[0] == "character"
    assert list(ro.r["class"](r_kwargs["unit_costs"]))[0] == "numeric"
    assert list(r_kwargs["unit_costs"]) == [0.5, 1.0]
    assert list(ro.r["class"](r_kwargs["default_cost_fxn_type"]))[0] == "character"
    # the R function object exists and is callable
    fn = _bridge._get_rfunc("visualize_cost")
    assert fn is not None


# --------------------------------------------------------------------------
# lago_report renders an HTML file and returns its path
# --------------------------------------------------------------------------
def test_lago_report_returns_path(bb_data, tmp_path):
    res = lago.optimize(data=bb_data, **_bb_kwargs())
    out = str(tmp_path / "report.html")
    path = lago.lago_report(res, output_file=out, title="pytest report")
    assert isinstance(path, str)
    import os
    assert os.path.exists(path)
    assert os.path.getsize(path) > 0


# --------------------------------------------------------------------------
# Conversion helpers
# --------------------------------------------------------------------------
def test_dataframe_roundtrip(bb_data):
    r_df = _bridge.py_to_r(bb_data)
    back = _bridge.r_to_py(r_df)
    assert isinstance(back, pd.DataFrame)
    assert back.shape == bb_data.shape


def test_none_maps_to_r_null(ro):
    r_null = _bridge.py_to_r(None)
    assert list(ro.r["is.null"](r_null))[0] is True
    assert _bridge.r_to_py(ro.NULL) is None
