"""Tests for the fixes of bug bash 6 in the Python wrapper and the MCP server."""
import asyncio
import datetime
import decimal
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
    outcome_name="y", outcome_type="continuous", glm_family="gaussian", link="identity",
    intervention_components=["x1", "x2"], intervention_lower_bounds=[0, 0], intervention_upper_bounds=[10, 5],
    outcome_goal=6, include_confidence_set=False,
)
COSTS = [[0, 1], [0, 2]]


@pytest.fixture(scope="module", autouse=True)
def _stop_worker():
    """Stop the R worker process that the MCP calls of this file start."""
    yield
    S._WORKER.stop()


@pytest.fixture(scope="module")
def trial(ro):
    """Four centers, each seen in 12 periods, with two components."""
    rng = np.random.default_rng(1)
    rows = []
    for c in range(1, 5):
        for p in range(1, 13):
            for _ in range(10):
                x1, x2 = rng.uniform(0, 10), rng.uniform(0, 5)
                y = 1 + 0.5 * x1 + 0.8 * x2 + 0.1 * c + 0.05 * p + rng.normal()
                rows.append(dict(center=c, period=p, x1=x1, x2=x2, y=y))
    return pd.DataFrame(rows)


@pytest.fixture(scope="module")
def mtcars(ro):
    with _bridge.r_context():
        return _bridge.r_to_py(ro.r("mtcars")).reset_index(drop=True)[["mpg", "gear", "qsec", "vs"]]


def _in_r(ro, value, code="function(x) paste(capture.output(dput(x)), collapse = ' ')"):
    """The R text of a value, as dput() writes it once py_to_r converted it."""
    with _bridge.r_context():
        return ro.r(code)(_bridge.py_to_r(value))[0]


def _columns(ro, frame):
    """Each column of a data frame in R, as its class and its values."""
    with _bridge.r_context():
        f = ro.r(
            "function(d) vapply(d, function(x) paste(class(x)[1], "
            "paste(ifelse(is.nan(x), 'NaN', ifelse(is.na(x), 'NA', as.character(x))), collapse = '|')), '')"
        )
        return list(f(_bridge.py_to_r(frame)))


def _optimize(data, **extra):
    return lago.optimize(data=data, cost_list=COSTS, **{**BASE, **extra})


def _payload(result):
    from mcp.types import CallToolResult

    if isinstance(result, CallToolResult):
        return json.loads(result.content[0].text), bool(result.isError)
    content = result[0] if isinstance(result, tuple) else result
    return json.loads(content[0].text), False


def _call(name, args):
    return _payload(asyncio.run(S.mcp.call_tool(name, args)))


# --------------------------------------------------------------------------
# py-bridge 1. a column of Python or numpy numbers is a numeric column in R
# --------------------------------------------------------------------------
def test_object_columns_of_numbers_reach_r_as_numbers(ro, trial):
    frame = pd.DataFrame({
        "a": pd.Series([1, None, 3], dtype=object),
        "b": pd.Series([np.float64(1.5), np.int64(2), None], dtype=object),
        "c": pd.DataFrame({"c": [1.5, 2.5, 3.5]}).astype(object)["c"],
        "d": pd.Series([decimal.Decimal("1.5"), 2, np.nan], dtype=object),
        "e": pd.Series([True, 1.0, 2.0], dtype=object),
    })
    assert _columns(ro, frame)[:4] == ["numeric 1|NA|3", "numeric 1.5|2|NA", "numeric 1.5|2.5|3.5",
                                       "numeric 1.5|2|NA"]
    # true and false among numbers are not numbers, so that column stays as it was
    assert not _columns(ro, frame)[4].startswith("numeric")
    plain = _optimize(trial)
    as_objects = _optimize(trial.astype({"x1": object, "y": object}))
    assert as_objects["rec_int"] == pytest.approx(plain["rec_int"])


# --------------------------------------------------------------------------
# py-bridge 2. uint32 and uint64 columns are integers, or doubles when large
# --------------------------------------------------------------------------
def test_unsigned_columns_reach_r_as_numbers(ro, trial):
    frame = pd.DataFrame({
        "a": np.array([1, 2], dtype="uint32"),
        "b": np.array([1, 2], dtype="uint64"),
        "c": np.array([1, 3_000_000_000], dtype="uint32"),
        "d": np.array([1, 2], dtype="uint16"),
    })
    assert _columns(ro, frame) == ["integer 1|2", "integer 1|2", "numeric 1|3e+09", "integer 1|2"]
    scaled = trial.assign(x2=(trial.x2 * 1000).round())
    as_int = _optimize(scaled.astype({"x2": "int64"}), intervention_upper_bounds=[10, 5000])
    as_uint = _optimize(scaled.astype({"x2": "uint32"}), intervention_upper_bounds=[10, 5000])
    assert as_uint["rec_int"] == pytest.approx(as_int["rec_int"])


# --------------------------------------------------------------------------
# py-bridge 3. a number used as a name is written as R writes the level
# --------------------------------------------------------------------------
def test_numeric_names_are_written_as_r_writes_levels(ro, trial):
    assert _in_r(ro, pd.Series([0.5, 0.5], index=[1.0, 2.5])) == "c(`1` = 0.5, `2.5` = 0.5)"
    assert _in_r(ro, {100000.0: 0.5, 2: 0.5}) == "c(`100000` = 0.5, `2` = 0.5)"
    # a whole number too large for R's integers is a double, as its column is
    assert _in_r(ro, {3_000_000_000: 1.0}) == "c(`3e+09` = 1)"
    assert _in_r(ro, {np.int64(7): 1.0}) == "c(`7` = 1)"
    floats = trial.assign(center=trial.center.astype(float))
    by_center = floats.groupby("center").size() / len(floats)
    assert list(by_center.index) == [1.0, 2.0, 3.0, 4.0]
    named = _optimize(floats, include_center_effects=True, center_weights_for_outcome_goal=by_center)
    plain = _optimize(floats, include_center_effects=True, center_weights_for_outcome_goal=by_center.tolist())
    assert named["rec_int"] == pytest.approx(plain["rec_int"])


def test_refused_names_get_python_advice(trial):
    weights = pd.Series([0.25] * 4, index=["a", "b", "c", "d"])
    with pytest.raises(Exception) as err:
        _optimize(trial, include_center_effects=True, center_weights_for_outcome_goal=weights)
    assert "unname()" not in str(err.value)
    assert "as a plain list in place of a pandas Series or a dict" in str(err.value)
    assert _bridge.python_hint("or remove the name with unname().") == (
        "or give the values without names, as a plain list in place of a pandas Series or a dict."
    )
    assert _bridge.python_hint("no advice here") == "no advice here"


# --------------------------------------------------------------------------
# py-bridge 4. a categorical with categories that are not text
# --------------------------------------------------------------------------
def test_categorical_columns_keep_their_categories_as_text(ro, trial):
    frame = pd.DataFrame({
        "a": pd.Categorical([2, 1, 10]),
        "b": pd.Categorical(pd.to_datetime(["2020-01-01", "2020-02-01", None])),
        "c": pd.Categorical([1.0, 2.5, 1.0], categories=[2.5, 1.0], ordered=True),
        "d": pd.Categorical(["u", "v", "u"]),
        "e": pd.Categorical([1, "1", 1]),
    })
    levels = "function(d) vapply(d, function(x) paste(class(x)[1], paste(levels(x), collapse = '|')), '')"
    with _bridge.r_context():
        out = list(ro.r(levels)(_bridge.py_to_r(frame)))
    assert out[:4] == ["factor 1|2|10", "factor 2020-01-01|2020-02-01", "ordered 2.5|1", "factor u|v"]
    # 1 and "1" would both be "1", so that column is left to rpy2
    assert out[4].startswith("character")
    assert _columns(ro, frame)[1] == "factor 2020-01-01|2020-02-01|NA"
    assert _in_r(ro, pd.Categorical(pd.to_datetime(["2020-01-01"]))) == '"2020-01-01"'
    start = pd.Timestamp("2020-01-01")
    dated = trial.assign(period=pd.Categorical(start + pd.to_timedelta((trial.period - 1) * 31, unit="D")))
    out = _optimize(dated, include_time_effects=True, time_effect_optimization_value=datetime.date(2020, 1, 1))
    plain = _optimize(dated.assign(period=dated.period.astype(object)), include_time_effects=True,
                      time_effect_optimization_value=datetime.date(2020, 1, 1))
    assert out["rec_int"] == pytest.approx(plain["rec_int"])


# --------------------------------------------------------------------------
# py-bridge 5. times with a time zone are written in the zone of the column
# --------------------------------------------------------------------------
def test_times_with_a_zone_name_the_period_of_the_same_instant(ro, trial):
    start = pd.Timestamp("2020-01-01", tz="US/Eastern")
    zoned = trial.assign(period=pd.to_datetime([start + pd.DateOffset(months=int(p) - 1) for p in trial.period]))
    last = zoned.period.max()
    same = _optimize(zoned, include_time_effects=True, time_effect_optimization_value=last)
    other = _optimize(zoned, include_time_effects=True, time_effect_optimization_value=last.tz_convert("UTC"))
    assert other["rec_int"] == pytest.approx(same["rec_int"])
    values = pd.Series([last.tz_convert("UTC"), (last - pd.DateOffset(months=1)).tz_convert("Asia/Tokyo")])
    swept = lago.sensitivity(data=zoned, cost_list=COSTS, **BASE, include_time_effects=True,
                             time_effect_optimization_value=last, parameter="time_effect_optimization_value",
                             values=values)
    assert list(swept.status) == ["ok", "ok"], list(swept.error_message)
    assert list(swept.value) == ["2020-12-01", "2020-11-01"]
    # a column of times in more than one zone is written in the zone of the first
    mixed = pd.DataFrame({"t": pd.Series([pd.Timestamp("2020-01-01 05:00", tz="UTC"),
                                          pd.Timestamp("2020-01-01", tz="US/Eastern"), None], dtype=object)})
    assert _columns(ro, mixed) == ["character 2020-01-01 05:00:00|2020-01-01 05:00:00|NA"]
    with _bridge.r_context():
        moved = _bridge._periods_in_data_zone({
            "data": mixed.rename(columns={"t": "period"}),
            "time_effect_optimization_value": [pd.Timestamp("2020-01-01", tz="US/Eastern"), 3],
        })
    assert moved["time_effect_optimization_value"][0].tzinfo == datetime.timezone.utc
    assert moved["time_effect_optimization_value"][1] == 3
    # a period column without a zone, or no period column, leaves the values as they are
    for data in (trial, trial.drop(columns="period"), trial.assign(period=pd.Series([None] * len(trial), dtype=object)),
                 pd.DataFrame([[1, 2]], columns=["period", "period"]), None):
        kwargs = {"data": data, "time_effect_optimization_value": last.tz_convert("UTC")}
        assert _bridge._periods_in_data_zone(kwargs) is kwargs


# --------------------------------------------------------------------------
# py-bridge 6. a 0-d array is a scalar, and a time span is a number of days
# --------------------------------------------------------------------------
def test_zero_d_arrays_and_time_spans(ro, trial):
    assert _in_r(ro, np.array(5.0)) == "5"
    assert _in_r(ro, [np.array(10.0), 5]) == "c(10, 5)"
    assert _in_r(ro, pd.Timedelta("36h")) == "1.5"
    assert _in_r(ro, datetime.timedelta(days=2)) == "2"
    assert _in_r(ro, np.timedelta64(2, "D")) == "2"
    assert _in_r(ro, np.timedelta64("NaT")) == "NA"
    assert _in_r(ro, np.array([1, 2], dtype="timedelta64[D]")) == "c(1, 2)"
    assert _in_r(ro, pd.to_timedelta(["1D", "12h"])) == "c(1, 0.5)"
    assert _columns(ro, pd.DataFrame({"t": pd.Series([np.timedelta64(1, "D"), None], dtype=object)})) == ["numeric 1|NA"]
    out = _optimize(trial, intervention_upper_bounds=[np.array(10.0), 5])
    assert out["intervention_upper_bounds"] == [10.0, 5.0]
    spans = trial.assign(period=pd.to_timedelta(trial.period, unit="D"))
    by_span = _optimize(spans, include_time_effects=True, time_effect_optimization_value=pd.Timedelta(days=12))
    by_number = _optimize(spans, include_time_effects=True, time_effect_optimization_value=12)
    assert by_span["rec_int"] == pytest.approx(by_number["rec_int"])


# --------------------------------------------------------------------------
# mcp 1 (bridge side). a NaN in a float column is NA in R, not NaN
# --------------------------------------------------------------------------
def test_nan_in_a_float_column_reaches_r_as_na(ro, mtcars):
    frame = pd.DataFrame({
        "a": [1.0, np.nan],
        "b": np.array([np.nan, 2.0], dtype="float32"),
        "c": pd.Series([1, None], dtype=object),
        "d": [1.5, 2.5],
    })
    assert _columns(ro, frame) == ["numeric 1|NA", "numeric NA|2", "numeric 1|NA", "numeric 1.5|2.5"]
    center = (mtcars.vs + 1).astype(float).where(np.arange(32) != 0)
    records = [dict(r, w=None if i == 0 else 1.0) for i, r in enumerate(mtcars.assign(site=center).to_dict(orient="records"))]
    p = dict(cost_list_of_vectors=[[0, 4], [4, 6]], unit_costs=None, weights_column="w")
    assert S._model_kwargs(p, pd.DataFrame.from_records(records))["weights"][:2] == [None, 1.0]
    out = S.optimize(
        data_records=records, outcome_name="mpg", outcome_type="continuous", glm_family="gaussian", link="identity",
        intervention_components=["gear", "qsec"], intervention_lower_bounds=[0, 0], intervention_upper_bounds=[10, 350],
        cost_list_of_vectors=[[0, 4], [4, 6]], outcome_goal=25, include_confidence_set=False,
        include_center_effects=True, center_column="site",
    )
    assert "error" not in out, out
    assert not any("NaN" in w for w in out["warnings"])


# --------------------------------------------------------------------------
# py-api 1. a run gets an error message only when it failed
# --------------------------------------------------------------------------
def test_sensitivity_messages_go_to_the_failed_runs(trial):
    swept = lago.sensitivity(data=trial, cost_list=COSTS, **BASE, include_time_effects=True,
                             time_effect_optimization_value=12, parameter="time_effect_optimization_value",
                             values=[12, 12 + 1e-10, 99])
    messages = list(swept.error_message)
    assert list(swept.status) == ["ok", "error", "error"]
    assert messages[0] is None
    assert "12.0000000001" in messages[1]
    assert "(99)" in messages[2]
    fine = lago.sensitivity(data=trial, cost_list=COSTS, **BASE, parameter="outcome_goal", values=[5, 6])
    assert list(fine.error_message) == [None, None]


# --------------------------------------------------------------------------
# py-api 2. n_grid may be a whole Decimal
# --------------------------------------------------------------------------
def test_budget_n_grid_takes_a_whole_decimal(trial):
    assert lago._whole_number(decimal.Decimal("25"), "n_grid") == 25
    assert lago._whole_number(decimal.Decimal("25.0"), "n_grid") == 25
    for bad in (decimal.Decimal("2.5"), decimal.Decimal("NaN"), decimal.Decimal("Infinity"), True, "25"):
        with pytest.raises(TypeError, match="must be a whole number"):
            lago._whole_number(bad, "n_grid")
    base = {k: v for k, v in BASE.items() if k != "outcome_goal"}
    out = lago.budget(data=trial, cost_list=COSTS, **base, budget=8, n_grid=decimal.Decimal("5"))
    assert out["feasible"] is True


# --------------------------------------------------------------------------
# py-api 3. the frames returned have the default index
# --------------------------------------------------------------------------
def test_sensitivity_and_frontier_frames_have_a_default_index(trial):
    swept = lago.sensitivity(data=trial, cost_list=COSTS, **BASE, parameter="outcome_goal", values=[5, 6])
    assert isinstance(swept.index, pd.RangeIndex) and list(swept.index) == [0, 1]
    base = {k: v for k, v in BASE.items() if k != "outcome_goal"}
    frontier = lago.budget(data=trial, cost_list=COSTS, **base, budget=8, n_grid=5)["frontier"]
    assert isinstance(frontier.index, pd.RangeIndex) and frontier.index[0] == 0


# --------------------------------------------------------------------------
# mcp 2. true and false are not center weights or glm weights
# --------------------------------------------------------------------------
MCP = dict(outcome_name="mpg", outcome_type="continuous", glm_family="gaussian", link="identity",
           intervention_components=["gear", "qsec"], intervention_lower_bounds=[0, 0],
           intervention_upper_bounds=[10, 350], cost_list_of_vectors=[[0, 4], [4, 6]])


def test_true_and_false_are_refused_as_weights(mtcars):
    records = mtcars.to_dict(orient="records")
    args = dict(MCP, data_records=records, outcome_goal=25, include_confidence_set=False,
                include_center_effects=True, center_column="gear")
    out, is_error = _call("optimize", dict(args, center_weights_for_outcome_goal=[True, 0.5, 0.5]))
    assert is_error
    assert out["error"] == ("ValueError: `center_weights_for_outcome_goal` must hold numbers, not true. "
                            "Give one weight per center, such as 0.25.")
    out, is_error = _call("optimize", dict(args, center_weights_for_outcome_goal={"3": 0.5, "4": False, "5": 0.5}))
    assert is_error and "not false" in out["error"]
    assert S._refuse_booleans({"center_weights_for_outcome_goal": [0.2, 0.8]}) is None
    assert S._refuse_booleans({"center_weights_for_outcome_goal": "[true]"}) is None
    flags = pd.DataFrame({"w": np.array([True, False])})
    p = dict(cost_list_of_vectors=[[0, 1]], unit_costs=None, weights_column="w")
    with pytest.raises(ValueError, match="must hold numbers, and it holds"):
        S._model_kwargs(p, flags)


# --------------------------------------------------------------------------
# mcp 3. the null goal hint in the runs, and a sweep where every run failed
# --------------------------------------------------------------------------
def test_a_sweep_where_every_run_failed_is_an_error(mtcars):
    records = mtcars.to_dict(orient="records")
    out, is_error = _call("sensitivity", dict(MCP, data_records=records, outcome_goal=None,
                                              parameter="cost_multiplier", values=[1, 2]))
    assert is_error
    assert out["error"].startswith("RuntimeError: every run of the sweep failed. The first run failed with: Both")
    assert "The call sent `outcome_goal` as null." in out["error"]
    assert len(out["runs"]) == 2
    assert all("The call sent `outcome_goal` as null." in r["error_message"] for r in out["runs"])
    assert isinstance(out["warnings"], list)
    out, is_error = _call("sensitivity", dict(MCP, data_records=records, outcome_goal=25,
                                              parameter="outcome_goal", values=[25, 26]))
    assert not is_error and [r["error_message"] for r in out["runs"]] == [None, None]
    # with no null goal, a failed run keeps its message as it is
    assert S._sweep_result({"runs": [{"status": "ok", "error_message": None},
                                     {"status": "error", "error_message": "x"}]})["runs"][1]["error_message"] == "x"
    assert S._sweep_result({"runs": []}) == {"runs": []}


# --------------------------------------------------------------------------
# inputs-names 3. a cost list as a dict keyed by component
# --------------------------------------------------------------------------
def test_a_cost_list_can_be_a_dict_keyed_by_component(ro, trial):
    assert _in_r(ro, {"x2": [0, 2], "x1": np.array([0, 1])}) == "list(x2 = c(0, 2), x1 = c(0, 1))"
    assert _in_r(ro, {"a": pd.Series([1.0, 2.0], index=["u", "v"]), "b": (3,)}) == "list(a = c(1, 2), b = 3)"
    by_list = _optimize(trial)
    by_name = lago.optimize(data=trial, cost_list={"x2": [0, 2], "x1": [0, 1]}, **BASE)
    assert by_name["rec_int"] == pytest.approx(by_list["rec_int"])
    # R puts the cost list in the order of the components
    assert by_name["cost_list_of_vectors"] == [[0.0, 1.0], [0.0, 2.0]]
    # an empty dict is still a named numeric vector, and a dict of text is refused
    assert _in_r(ro, {}) == "structure(numeric(0), names = character(0))"
    with pytest.raises(TypeError, match="A dict must hold numbers"):
        _bridge.py_to_r({"a": "text"})
