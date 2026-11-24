"""Tests for the fixes of bug bash 5 in the Python wrapper and the MCP server."""
import asyncio
import datetime
import decimal
import fractions
import io
import json
import os

import numpy as np
import pandas as pd
import pytest

if os.environ.get("LAGO_REQUIRE_R") != "1":
    pytest.importorskip("mcp.server.fastmcp", reason="mcp SDK not installed")

import lago  # noqa: E402
from lago import _bridge  # noqa: E402
from lago import mcp_server as S  # noqa: E402

BASE = dict(
    outcome_name="mpg", outcome_type="continuous", glm_family="gaussian", link="identity",
    intervention_components=["gear", "qsec"], intervention_lower_bounds=[0, 0], intervention_upper_bounds=[10, 350],
    include_confidence_set=False,
)
BB = dict(
    outcome_name="pp3_oxytocin_mother", outcome_type="binary",
    intervention_components=["coaching_updt", "launch_duration"], intervention_lower_bounds=[1, 1],
    intervention_upper_bounds=[40, 5], cost_list_of_vectors=[[0, 1700], [0, 8000]], outcome_goal=0.85,
    include_confidence_set=False, include_center_effects=True, center_column="site_name",
)


@pytest.fixture(scope="module", autouse=True)
def _stop_worker():
    """Stop the R worker process that the MCP calls of this file start."""
    yield
    S._WORKER.stop()


@pytest.fixture(scope="module")
def mtcars(ro):
    with _bridge.r_context():
        return _bridge.r_to_py(ro.r("mtcars")).reset_index(drop=True)[["mpg", "gear", "qsec", "vs"]]


def _r_format(ro, frame):
    """Each column of a data frame in R, as its class and its values."""
    with _bridge.r_context():
        f = ro.r("function(d) vapply(d, function(x) paste(class(x)[1], paste(ifelse(is.na(x), 'NA', as.character(x)), collapse = '|')), '')")
        return list(f(_bridge.py_to_r(frame)))


def _payload(result):
    from mcp.types import CallToolResult

    if isinstance(result, CallToolResult):
        return json.loads(result.content[0].text), bool(result.isError)
    content = result[0] if isinstance(result, tuple) else result
    return json.loads(content[0].text), False


def _call(name, args):
    return _payload(asyncio.run(S.mcp.call_tool(name, args)))


def _optimize(data, **extra):
    return lago.optimize(data=data, cost_list=[[0, 4], [4, 6]], outcome_goal=25, **{**BASE, **extra})


# --------------------------------------------------------------------------
# python 1. true and false with missing values reach R as NA
# --------------------------------------------------------------------------
def test_true_and_false_with_missing_values_reach_r_as_na(ro, mtcars):
    frame = pd.DataFrame({
        "a": pd.Series([True, np.nan, False], dtype=object),
        "b": pd.Series([np.nan, False, True], dtype=object),
        "c": pd.Series([None, np.bool_(True), pd.NA], dtype=object),
        "d": pd.Series([True, False, True], dtype=object),
    })
    assert _r_format(ro, frame) == ["logical TRUE|NA|FALSE", "logical NA|FALSE|TRUE",
                                    "logical NA|TRUE|NA", "logical TRUE|FALSE|TRUE"]
    # a yes or no covariate read by read_csv leaves out the rows it is missing
    buf = io.StringIO()
    flag = mtcars.vs.map({1: "True", 0: "False"}).where(np.arange(32) % 3 != 1, "")
    mtcars.assign(flag=flag).to_csv(buf, index=False)
    buf.seek(0)
    d = pd.read_csv(buf)
    assert d.flag.dtype == object
    nobs = ro.r("function(m) nobs(m)")
    with _bridge.r_context():
        first = _optimize(d, additional_covariates=["flag"])
        second = _optimize(d.assign(flag=d.flag.astype("boolean")), additional_covariates=["flag"])
        assert list(nobs(first["model"])) == list(nobs(second["model"])) == [21]
    assert first["rec_int"] == second["rec_int"]


# --------------------------------------------------------------------------
# python 2. Decimal, sparse and time span columns reach R as numbers
# --------------------------------------------------------------------------
def test_decimal_sparse_and_time_span_columns_reach_r_as_numbers(ro, mtcars):
    frame = pd.DataFrame({
        "dec": pd.Series([decimal.Decimal("1.5"), None, 2], dtype=object),
        "frac": pd.Series([fractions.Fraction(1, 4), 1.0, np.nan], dtype=object),
        "sparse": pd.arrays.SparseArray([0.0, 0.0, 2.5]),
        "td": pd.to_timedelta([1.5, None, 2], unit="D"),
        "td_obj": pd.Series([datetime.timedelta(hours=36), None, pd.Timedelta(days=2)], dtype=object),
        "mixed": pd.Series([1, "a", 2.0], dtype=object),
    })
    assert _r_format(ro, frame)[:5] == ["numeric 1.5|NA|2", "numeric 0.25|1|NA", "numeric 0|0|2.5",
                                        "numeric 1.5|NA|2", "numeric 1.5|NA|2"]
    plain = _optimize(mtcars)
    as_decimal = _optimize(mtcars.assign(
        mpg=pd.Series([decimal.Decimal(repr(v)) for v in mtcars.mpg], dtype=object),
        gear=pd.Series(pd.arrays.SparseArray(mtcars.gear.to_numpy())),
    ))
    assert as_decimal["rec_int"] == pytest.approx(plain["rec_int"])


# --------------------------------------------------------------------------
# python 3. budget refuses an n_grid that is not a whole number
# --------------------------------------------------------------------------
def test_budget_refuses_an_n_grid_that_is_not_a_whole_number(mtcars):
    args = dict(BASE, data=mtcars, cost_list=[[0, 4], [4, 6]], budget=60)
    for bad in (2.5, 2.9, "30", True, float("nan"), None):
        with pytest.raises(TypeError, match="`n_grid` must be a whole number, such as 25, and not"):
            lago.budget(n_grid=bad, **args)
    whole = lago.budget(n_grid=3.0, **args)
    assert len(whole["frontier"]) == len(lago.budget(n_grid=np.int64(3), **args)["frontier"])
    with pytest.raises(Exception, match="`n_grid` must be a single integer >= 2"):
        lago.budget(n_grid=1, **args)


# --------------------------------------------------------------------------
# python 4 and 5. a Period and a time with a fraction of a second keep their label
# --------------------------------------------------------------------------
def test_a_period_becomes_its_label(ro, mtcars):
    assert _bridge._plain(pd.Period("2020Q1", freq="Q")) == "2020Q1"
    assert _bridge._plain([pd.Period("2020-01", freq="M"), pd.NaT]) == ["2020-01", None]
    frame = pd.DataFrame({
        "p": pd.period_range("2020Q1", periods=3, freq="Q"),
        "q": pd.Series([pd.Period("2020-01", freq="M"), None, pd.Period("2020-02", freq="M")], dtype=object),
    })
    assert _r_format(ro, frame) == ["character 2020Q1|2020Q2|2020Q3", "character 2020-01|NA|2020-02"]
    data = mtcars.assign(period=pd.PeriodIndex(np.where(np.arange(32) % 2 == 0, "2020Q1", "2020Q2"), freq="Q"))
    by_period = _optimize(data, include_time_effects=True, time_effect_optimization_value=pd.Period("2020Q2", freq="Q"))
    by_text = _optimize(data.assign(period=data.period.astype(str)), include_time_effects=True,
                        time_effect_optimization_value="2020Q2")
    assert by_period["rec_int"] == by_text["rec_int"]


def test_a_time_keeps_its_fraction_of_a_second(ro, mtcars):
    assert _bridge._date_text(pd.Timestamp("2020-01-01 10:30:00.5")) == "2020-01-01 10:30:00.5"
    assert _bridge._date_text(datetime.datetime(2020, 1, 1, 10, 30, 0, 123)) == "2020-01-01 10:30:00.000123"
    assert _bridge._date_text(pd.Timestamp("2020-01-01 10:30:00")) == "2020-01-01 10:30:00"
    with _bridge.r_context():
        r_levels = list(ro.r('levels(factor(as.POSIXct(c("2020-01-01 10:30:00.5", "2020-01-01 10:30:00.7"), tz = "UTC")))'))
    times = pd.to_datetime(["2020-01-01 10:30:00.5", "2020-01-01 10:30:00.7"])
    assert [_bridge._date_text(t) for t in times] == r_levels
    # three periods a fraction of a second apart stay three periods
    stamps = pd.to_datetime(["2020-01-01 10:30:00.5", "2020-01-01 10:30:00.7", "2020-01-01 10:30:00.9"])
    data = mtcars.assign(period=stamps[np.arange(32) % 3])
    with _bridge.r_context():
        assert list(ro.r("function(d) nlevels(factor(d$period))")(_bridge.py_to_r(data))) == [3]
    by_time = _optimize(data, include_time_effects=True, time_effect_optimization_value=stamps[1])
    by_text = _optimize(data, include_time_effects=True, time_effect_optimization_value="2020-01-01 10:30:00.7")
    assert by_time["rec_int"] == by_text["rec_int"]


# --------------------------------------------------------------------------
# python 6. names that are empty, NA or used twice lose no element
# --------------------------------------------------------------------------
def test_names_that_are_not_keys_give_pairs(ro, mtcars):
    with _bridge.r_context():
        convert = lambda text: _bridge.r_to_py(ro.r(text))  # noqa: E731
        assert convert("list(a = 1, 2)") == [("a", [1.0]), ("", [2.0])]
        assert convert("list(a = 1, a = 2)") == [("a", [1.0]), ("a", [2.0])]
        assert convert("c(a = 1, a = 2)") == [("a", 1.0), ("a", 2.0)]
        assert convert('setNames(1:2, c("a", NA))') == [("a", 1), (None, 2)]
        assert convert("list(a = 1, b = 2)") == {"a": [1.0], "b": [2.0]}
        assert convert("c(a = 1, b = 2)") == {"a": 1.0, "b": 2.0}
    # a value swept twice keeps the message of each failed run
    data = mtcars.assign(period=np.where(np.arange(32) % 2 == 0, "P1", "P2"))
    out = lago.sensitivity(data=data, cost_list=[[0, 4], [4, 6]], outcome_goal=25,
                           **{k: v for k, v in BASE.items() if k != "include_confidence_set"},
                           include_time_effects=True, time_effect_optimization_value="P1",
                           parameter="time_effect_optimization_value", values=["P1", "P9", "P9"])
    assert out["status"].tolist() == ["ok", "error", "error"]
    messages = out["error_message"].tolist()
    assert messages[0] is None
    assert all("P9" in m for m in messages[1:])


# --------------------------------------------------------------------------
# inputs-names 4 and docs 4. a Series and a dict keep their names
# --------------------------------------------------------------------------
def test_a_series_and_a_dict_keep_their_names(ro, mtcars):
    names = ro.r("names")
    with _bridge.r_context():
        vec = _bridge.py_to_r(pd.Series({"qsec": 23.0, "gear": np.nan}))
        assert list(names(vec)) == ["qsec", "gear"]
        assert list(ro.r("is.na")(vec)) == [False, True]
        assert list(names(_bridge.py_to_r({"b": 1, "a": None}))) == ["b", "a"]
        # an integer index names the centers it holds
        assert list(names(_bridge.py_to_r(pd.Series([0.5, 0.5], index=[2, 10])))) == ["2", "10"]
        # the default positions and text values give no names
        assert _bridge._is_null(names(_bridge.py_to_r(pd.Series([1.0, 2.0]))))
        assert _bridge._is_null(names(_bridge.py_to_r(pd.Series(["gear", "qsec"], index=["x", "y"]))))
        with pytest.raises(TypeError, match="a dict with values of type str"):
            _bridge.py_to_r({"a": "x"})
        with pytest.raises(TypeError, match="elements of type int and str"):
            _bridge.py_to_r([1, "a", None])
        with pytest.raises(TypeError, match="elements of type dict"):
            _bridge.py_to_r([{"a": 1}])
    plain = _optimize(mtcars, intervention_upper_bounds=[10, 20])
    by_series = _optimize(mtcars, intervention_upper_bounds=pd.Series({"qsec": 20, "gear": 10}))
    by_dict = _optimize(mtcars, intervention_upper_bounds={"qsec": 20, "gear": 10})
    assert plain["rec_int"] == by_series["rec_int"] == by_dict["rec_int"]


def test_center_weights_from_a_series_keep_their_names(bb_data):
    data = bb_data.assign(center=bb_data.site_name)
    args = {k: v for k, v in BB.items() if k not in ("cost_list_of_vectors", "center_column")}
    target = "Chaubepur"
    named = lago.optimize(data=data, cost_list=BB["cost_list_of_vectors"], center_effects_optimization_values=target, **args)
    centers = sorted(data.center.unique(), reverse=True)
    weights = pd.Series([1.0 if c == target else 0.0 for c in centers], index=centers)
    by_series = lago.optimize(data=data, cost_list=BB["cost_list_of_vectors"], center_weights_for_outcome_goal=weights, **args)
    assert by_series["rec_int"] == pytest.approx(named["rec_int"])


# --------------------------------------------------------------------------
# mcp 1 and 2. center weights by name, and no true or false for a center
# --------------------------------------------------------------------------
def test_mcp_center_weights_take_an_object(bb_data):
    records = bb_data.to_dict(orient="records")
    target = "Chaubepur"
    named, err = _call("optimize", dict(BB, data_records=records, center_effects_optimization_values=target))
    assert not err, named
    weights = {c: (1.0 if c == target else 0.0) for c in sorted(bb_data.site_name.unique())}
    by_object, err = _call("optimize", dict(BB, data_records=records, center_weights_for_outcome_goal=weights))
    assert not err, by_object
    assert by_object["rec_int"] == pytest.approx(named["rec_int"])
    for name in ("optimize", "sensitivity", "budget"):
        tool = next(t for t in asyncio.run(S.mcp.list_tools()) if t.name == name)
        kinds = {a.get("type") for a in tool.inputSchema["properties"]["center_weights_for_outcome_goal"]["anyOf"]}
        assert {"array", "object"} <= kinds
        assert "matched to the centers by name" in tool.description


def test_mcp_refuses_true_and_false_for_a_center_or_a_period():
    out, err = _call("optimize", {"center_effects_optimization_values": True})
    assert err
    assert out["error"] == (
        "ValueError: `center_effects_optimization_values` must be a label or a number, not true. "
        "Give the center or the period as it is written in its column."
    )
    out, err = _call("optimize", {"time_effect_optimization_value": False})
    assert err
    assert "`time_effect_optimization_value` must be a label or a number, not false." in out["error"]


# --------------------------------------------------------------------------
# mcp 3. the weights column error names the first value that is not a number
# --------------------------------------------------------------------------
def test_the_weights_error_names_the_first_value_that_is_not_a_number(mtcars):
    records = [dict(r, w=1.0) for r in mtcars.to_dict(orient="records")]
    records[2]["w"] = None
    records[3]["w"] = "x"
    args = dict(BASE, data_records=records, cost_list_of_vectors=[[0, 4], [4, 6]], outcome_goal=25, weights_column="w")
    out = S.optimize(**args)
    assert out["error"] == "ValueError: the weights column 'w' must hold numbers, and it holds 'x'."
    # a missing weight among numbers written as text leaves its row out
    records[3]["w"] = "2"
    out = S.optimize(**args)
    assert "error" not in out, out
    nan_row = [dict(r) for r in records]
    nan_row[2]["w"] = float("nan")
    assert S.optimize(**dict(args, data_records=nan_row))["rec_int"] == out["rec_int"]


# --------------------------------------------------------------------------
# mcp 4. the NaN hint when both goals are missing because a goal was null
# --------------------------------------------------------------------------
def test_the_null_hint_comes_when_a_null_goal_leaves_no_goal(mtcars):
    args = dict(BASE, data_records=mtcars.to_dict(orient="records"), cost_list_of_vectors=[[0, 4], [4, 6]])
    hint = "The call sent `outcome_goal` as null. If you sent NaN or Infinity"
    out, err = _call("optimize", dict(args, outcome_goal=None))
    assert err
    assert "Both 'outcome_goal' and 'power_goal' are NULL" in out["error"] and hint in out["error"]
    out, err = _call("optimize", dict(args, outcome_goal=None, power_goal=None))
    assert "The call sent `outcome_goal` and `power_goal` as null." in out["error"]
    # without a null goal, there is no hint
    out, err = _call("optimize", args)
    assert err
    assert "Both 'outcome_goal' and 'power_goal' are NULL" in out["error"] and "sent" not in out["error"]
    # another error keeps its message
    out, err = _call("optimize", dict(args, outcome_goal=None, power_goal=0.8))
    assert err and "The call sent" not in out["error"]
    assert S._null_goal_hint({"error": "x"}) == {"error": "x"}
