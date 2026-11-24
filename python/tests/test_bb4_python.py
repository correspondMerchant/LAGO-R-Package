"""Tests for the fixes of bug bash 4 in the Python wrapper and the MCP server."""
import asyncio
import datetime
import decimal
import fractions
import json
import os
import warnings

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


@pytest.fixture(scope="module", autouse=True)
def _stop_worker():
    """Stop the R worker process that the MCP calls of this file start."""
    yield
    S._WORKER.stop()


@pytest.fixture(scope="module")
def mtcars(ro):
    with _bridge.r_context():
        return _bridge.r_to_py(ro.r("mtcars")).reset_index(drop=True)[["mpg", "gear", "qsec", "wt"]]


def _periods(n):
    return pd.to_datetime(np.where(np.arange(n) % 2 == 0, "2020-01-01", "2020-02-01"))


def _r_str(ro, value):
    """What R's class() says of each column of a data frame, or of a vector."""
    with _bridge.r_context():
        return list(ro.r("function(x) if (is.data.frame(x)) vapply(x, function(c) class(c)[1], '') else class(x)")(
            _bridge.py_to_r(value)
        ))


def _payload(result):
    from mcp.types import CallToolResult

    if isinstance(result, CallToolResult):
        return json.loads(result.content[0].text), bool(result.isError)
    content = result[0] if isinstance(result, tuple) else result
    return json.loads(content[0].text), False


def _call(name, args):
    return _payload(asyncio.run(S.mcp.call_tool(name, args)))


# --------------------------------------------------------------------------
# python 1. a column of Python dates reaches R as the text of its periods
# --------------------------------------------------------------------------
def test_a_column_of_python_dates_reaches_r_as_text(ro, mtcars):
    dates = mtcars.assign(period=_periods(len(mtcars)).date)
    assert _r_str(ro, dates[["period"]]) == ["character"]
    with _bridge.r_context():
        back = ro.r("function(d) d$period[1:2]")(_bridge.py_to_r(dates))
    assert list(back) == ["2020-01-01", "2020-02-01"]
    res = lago.optimize(
        data=dates, cost_list=[[0, 4], [4, 6]], outcome_goal=25, include_time_effects=True,
        time_effect_optimization_value=datetime.date(2020, 2, 1), **BASE,
    )
    assert len(res["rec_int"]) == 2
    # the data given is not changed
    assert isinstance(dates["period"][0], datetime.date)


def test_date_time_and_missing_date_columns_reach_r_as_text(ro):
    frame = pd.DataFrame({
        "t": pd.to_datetime(["2020-01-01", "2020-02-01 10:30"], format="ISO8601"),
        "d": [datetime.date(2020, 1, 1), None],
    })
    with _bridge.r_context():
        out = _bridge.py_to_r(frame)
        assert list(ro.r("function(d) d$t")(out)) == ["2020-01-01", "2020-02-01 10:30:00"]
        assert list(ro.r("function(d) is.na(d$d)")(out)) == [False, True]


# --------------------------------------------------------------------------
# python 3. whole numbers too large for R's integers reach R as numbers
# --------------------------------------------------------------------------
def test_large_whole_numbers_reach_r_as_numbers(ro, mtcars):
    frame = pd.DataFrame({
        "big": np.array([1, 2**40], dtype="int64"),
        "low": np.array([1, -(2**31)], dtype="int64"),
        "nullable": pd.array([2**40, None], dtype="Int64"),
        "small": np.array([1, 2], dtype="int64"),
    })
    assert _r_str(ro, frame) == ["numeric", "numeric", "numeric", "integer"]
    with _bridge.r_context():
        assert list(ro.r("function(d) d$big")(_bridge.py_to_r(frame))) == [1.0, float(2**40)]
    m2 = mtcars.assign(qsec=(mtcars.qsec * 1e8).astype(np.int64))
    res = lago.optimize(
        data=m2, cost_list=[[0, 4], [0, 6e-9]], outcome_goal=25,
        **{**BASE, "intervention_upper_bounds": [10, 3.5e10]},
    )
    assert len(res["rec_int"]) == 2


# --------------------------------------------------------------------------
# python 4. a NaN among text is a missing value
# --------------------------------------------------------------------------
def test_a_nan_among_text_or_logicals_is_na(ro):
    is_na = ro.r("is.na")
    with _bridge.r_context():
        v = _bridge.py_to_r(["a", np.nan])
        assert list(v)[0] == "a" and list(is_na(v)) == [False, True]
        v = _bridge.py_to_r(pd.Series(["a", np.nan, "b"]).unique())
        assert list(is_na(v)) == [False, True, False]
        v = _bridge.py_to_r([True, float("nan")])
        assert list(ro.r("class")(v)) == ["logical"] and list(is_na(v)) == [False, True]
        # a NaN among numbers stays NaN
        assert list(ro.r("is.nan")(_bridge.py_to_r([1.0, float("nan")]))) == [False, True]
        frame = pd.DataFrame({"s": ["a", np.nan, pd.NA, None]})
        assert list(ro.r("function(d) is.na(d$s)")(_bridge.py_to_r(frame))) == [False, True, True, True]
    assert _r_str(ro, frame) == ["character"]


# --------------------------------------------------------------------------
# python 5. clear conversions and errors for odd values
# --------------------------------------------------------------------------
def test_odd_values_convert_or_give_a_clear_error(ro):
    is_na = ro.r("is.na")
    with _bridge.r_context():
        nested = _bridge.py_to_r([[0, None], [1]])
        assert list(is_na(nested[0])) == [False, True]
        deep = _bridge.py_to_r([[[0, 1]], [1]])
        assert list(ro.r("function(x) class(x[[1]])")(deep)) == ["list"]
        for missing in (pd.NA, pd.NaT, np.datetime64("NaT")):
            assert list(is_na(_bridge.py_to_r(missing))) == [True]
        assert list(_bridge.py_to_r(range(3))) == [0.0, 1.0, 2.0]
        assert list(_bridge.py_to_r(decimal.Decimal("1.5"))) == [1.5]
        assert list(_bridge.py_to_r(fractions.Fraction(1, 4))) == [0.25]
        with pytest.raises(TypeError, match="a set has no order"):
            _bridge.py_to_r({1, 2})
        with pytest.raises(TypeError, match="type object"):
            _bridge.py_to_r(object())
        with pytest.raises(TypeError, match="a dict with values of type str"):
            _bridge.py_to_r({"a": "x"})
        # an rpy2 object still passes through
        assert _bridge.py_to_r(ro.NULL) is ro.NULL


# --------------------------------------------------------------------------
# python 2. a sweep of period labels keeps the message of each failed run
# --------------------------------------------------------------------------
def test_a_sweep_of_period_labels_keeps_its_error_messages(mtcars):
    data = mtcars.assign(period=_periods(len(mtcars)))
    out = lago.sensitivity(
        data=data, parameter="time_effect_optimization_value", values=["2020-01-01", "2021-07-01"],
        include_time_effects=True, cost_list=[[0, 4], [4, 6]], outcome_goal=25, **BASE,
    )
    assert out["status"].tolist() == ["ok", "error"]
    messages = out["error_message"].tolist()
    assert messages[0] is None
    assert "2021-07-01" in messages[1] and "not one of the periods" in messages[1]


def test_the_error_messages_of_a_sweep_match_text_and_numbers(ro):
    with _bridge.r_context():
        result = ro.r(
            """structure(data.frame(value = c("a", "b"), status = c("ok", "error")),
                         error_messages = c(b = "failed b"))"""
        )
        assert _bridge.r_sensitivity_to_df(result)["error_message"].tolist() == [None, "failed b"]
        result = ro.r(
            """structure(data.frame(value = c(0.1, 1/3), status = c("error", "error")),
                         error_messages = c("0.1" = "m1", "0.333333333333333" = "m2"))"""
        )
        assert _bridge.r_sensitivity_to_df(result)["error_message"].tolist() == ["m1", "m2"]


# --------------------------------------------------------------------------
# python 6. outcome_goal_intention is a scalar
# --------------------------------------------------------------------------
def test_outcome_goal_intention_is_a_scalar(mtcars):
    res = lago.optimize(data=mtcars, cost_list=[[0, 4], [4, 6]], outcome_goal=25, **BASE)
    assert res["outcome_goal_intention"] == "maximize"


# --------------------------------------------------------------------------
# python 7. get_confidence_set needs its costs and rec_int
# --------------------------------------------------------------------------
def test_get_confidence_set_needs_its_costs_and_rec_int(mtcars):
    args = dict(
        predictors_data=mtcars[["gear", "qsec"]], intervention_components=["gear", "qsec"],
        outcome_data=list(mtcars["mpg"]), fitted_model=None, link="identity", outcome_goal=25,
        outcome_type="continuous", intervention_lower_bounds=[0, 0], intervention_upper_bounds=[10, 350],
        confidence_set_grid_step_size=[1, 50],
    )
    with pytest.raises(TypeError, match="needs `cost_list`"):
        lago.get_confidence_set(**args, rec_int=[1, 1])
    with pytest.raises(TypeError, match="needs `rec_int`"):
        lago.get_confidence_set(**args, cost_list=[[0, 4], [4, 6]])
    with pytest.raises(TypeError, match="needs `rec_int`"):
        lago.get_confidence_set(**args, cost_list_of_vectors=[[0, 4], [4, 6]])
    fit = lago.optimize(data=mtcars, cost_list=[[0, 4], [4, 6]], outcome_goal=25, **BASE)
    args["fitted_model"] = fit["model"]
    out = lago.get_confidence_set(**args, cost_list_of_vectors=[[0, 4], [4, 6]], rec_int=fit["rec_int"])
    assert isinstance(out["confidence_set_size_percentage"], float)


# --------------------------------------------------------------------------
# python 8, mcp 2 and the object= gap: the docs say what works
# --------------------------------------------------------------------------
def test_the_docs_say_what_a_cost_multiplier_needs():
    assert "``unit_costs``" in lago.sensitivity.__doc__.split("values : ")[0]
    assert "requires ``cost_list``" not in lago.sensitivity.__doc__
    sens = S.sensitivity.__doc__
    assert "with `unit_costs`" in sens and "it needs\n    `cost_list_of_vectors`" not in sens
    assert "``object`` argument" in lago.__doc__
    assert "are required" in lago.get_confidence_set.__doc__


# --------------------------------------------------------------------------
# mcp 1 and 5. a period by its label, and a center by its number
# --------------------------------------------------------------------------
def _records_with_quarters(mtcars):
    frame = mtcars.assign(quarter=np.where(np.arange(len(mtcars)) % 2 == 0, "2024-Q1", "2024-Q2"))
    frame["site"] = np.arange(len(mtcars)) % 3 + 1
    return frame.to_dict(orient="records")


def _mcp_args(records, **extra):
    args = dict(
        data_records=records, outcome_name="mpg", outcome_type="continuous", intervention_components=["gear", "qsec"],
        intervention_lower_bounds=[0.0, 0.0], intervention_upper_bounds=[10.0, 350.0],
        cost_list_of_vectors=[[0.0, 4.0], [4.0, 6.0]], glm_family="gaussian", link="identity", outcome_goal=25.0,
    )
    args.update(extra)
    return args


def test_a_period_label_and_a_center_number_reach_r(mtcars):
    records = _records_with_quarters(mtcars)
    out, err = _call("optimize", _mcp_args(
        records, include_time_effects=True, period_column="quarter", time_effect_optimization_value="2024-Q2",
        include_confidence_set=False,
    ))
    assert not err, out
    out, err = _call("optimize", _mcp_args(
        records, include_time_effects=True, period_column="quarter", time_effect_optimization_value="2024-Q9",
        include_confidence_set=False,
    ))
    assert err and "2024-Q9" in out["error"]
    out, err = _call("budget", {
        **{k: v for k, v in _mcp_args(records).items() if k != "outcome_goal"}, "budget": 60.0, "n_grid": 3,
        "include_time_effects": True, "period_column": "quarter", "time_effect_optimization_value": "2024-Q1",
    })
    assert not err, out
    out, err = _call("sensitivity", _mcp_args(
        records, parameter="outcome_goal", values=[24.0], include_time_effects=True, period_column="quarter",
        time_effect_optimization_value="2024-Q2",
    ))
    assert not err, out
    for center in (2, 2.0, "2"):
        out, err = _call("optimize", _mcp_args(
            records, include_center_effects=True, center_column="site", center_effects_optimization_values=center,
            include_confidence_set=False,
        ))
        assert not err, (center, out)
    out, err = _call("optimize", _mcp_args(
        records, include_center_effects=True, center_column="site", center_effects_optimization_values=7,
        include_confidence_set=False,
    ))
    assert err and "7" in out["error"]


def test_a_number_for_a_center_becomes_its_text(mtcars):
    p = {"cost_list_of_vectors": [[0, 1]], "center_effects_optimization_values": 2.0}
    assert S._model_kwargs(p, None)["center_effects_optimization_values"] == "2"
    p["center_effects_optimization_values"] = 2.5
    assert S._model_kwargs(p, None)["center_effects_optimization_values"] == "2.5"
    p["center_effects_optimization_values"] = "north"
    assert S._model_kwargs(p, None)["center_effects_optimization_values"] == "north"


# --------------------------------------------------------------------------
# mcp 3. confidence_level has no float noise
# --------------------------------------------------------------------------
def test_confidence_level_is_rounded(mtcars):
    records = mtcars.to_dict(orient="records")
    out = S.optimize(**_mcp_args(records, confidence_set_alpha=0.07, confidence_set_grid_step_size=[1.0, 50.0]))
    assert "error" not in out, out
    assert out["confidence_level"] == 0.93
    assert json.dumps(out["confidence_level"]) == "0.93"
    out = S.optimize(**_mcp_args(records, confidence_set_grid_step_size=[1.0, 50.0]))
    assert out["confidence_level"] == 0.95


# --------------------------------------------------------------------------
# mcp 4. a text weights column names the column
# --------------------------------------------------------------------------
def test_a_text_weights_column_is_named(mtcars):
    records = [dict(r, w="a") for r in mtcars.to_dict(orient="records")]
    out = S.optimize(**_mcp_args(records, weights_column="w", include_confidence_set=False))
    assert out["error"] == "ValueError: the weights column 'w' must hold numbers, and it holds 'a'."
    records = [dict(r, w=1.0 if i else None) for i, r in enumerate(mtcars.to_dict(orient="records"))]
    records[1]["w"] = "x"
    out = S.optimize(**_mcp_args(records, weights_column="w", include_confidence_set=False))
    assert out["error"] == "ValueError: the weights column 'w' must hold numbers, and it holds 'x'."
    records = [dict(r, w=True) for r in mtcars.to_dict(orient="records")]
    out = S.optimize(**_mcp_args(records, weights_column="w", include_confidence_set=False))
    assert "error" not in out, out


# --------------------------------------------------------------------------
# mcp 5. an unknown tool and the null hint
# --------------------------------------------------------------------------
def test_an_unknown_tool_gives_the_json_error_result():
    out, err = _call("nope", {})
    assert err
    assert out == {
        "error": "ValueError: `nope` is not a tool of this server. Its tools are describe_data, optimize, sensitivity, budget.",
        "warnings": [],
    }


def test_the_null_hint_says_when_it_applies():
    out, err = _call("optimize", {"outcome_name": None})
    assert err
    assert "`outcome_name`: Input should be a valid string. If you sent NaN or Infinity, note that JSON has no NaN or Infinity" in out["error"]


def test_the_schemas_take_a_period_label_and_a_center_number():
    tools = {t.name: t for t in asyncio.run(S.mcp.list_tools())}
    for name in ("optimize", "sensitivity", "budget"):
        props = tools[name].inputSchema["properties"]
        kinds = {a.get("type") for a in props["time_effect_optimization_value"]["anyOf"]}
        assert {"number", "string"} <= kinds
        kinds = {a.get("type") for a in props["center_effects_optimization_values"]["anyOf"]}
        assert {"number", "string"} <= kinds
