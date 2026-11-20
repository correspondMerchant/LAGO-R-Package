"""Tests for the fixes of bug bash 3 in the Python wrapper and the MCP server."""
import asyncio
import datetime
import json
import os
import re
import warnings

import numpy as np
import pandas as pd
import pytest

if os.environ.get("LAGO_REQUIRE_R") != "1":
    pytest.importorskip("mcp.server.fastmcp", reason="mcp SDK not installed")

import lago  # noqa: E402
from lago import _bridge  # noqa: E402
from lago import mcp_server as S  # noqa: E402

COMPONENTS = ["coaching_updt", "launch_duration"]


@pytest.fixture(scope="module")
def bb_csv(ro, tmp_path_factory):
    path = tmp_path_factory.mktemp("bb3") / "bb.csv"
    ro.r(f'write.csv(LAGOtrials::BB_data, "{path}", row.names = FALSE)')
    return str(path)


def _bb_args(**extra):
    args = dict(
        outcome_name="pp3_oxytocin_mother", outcome_type="binary", intervention_components=list(COMPONENTS),
        intervention_lower_bounds=[1.0, 1.0], intervention_upper_bounds=[40.0, 5.0],
        cost_list_of_vectors=[[0.0, 1.7], [0.0, 8.0]],
    )
    args.update(extra)
    return args


def _payload(result):
    """The JSON body and the error flag of a FastMCP call_tool result."""
    from mcp.types import CallToolResult

    if isinstance(result, CallToolResult):
        return json.loads(result.content[0].text), bool(result.isError)
    content = result[0] if isinstance(result, tuple) else result
    return json.loads(content[0].text), False


def _call(name, args):
    return _payload(asyncio.run(S.mcp.call_tool(name, args)))


# --------------------------------------------------------------------------
# 1. the warnings R gives before an error are not lost
# --------------------------------------------------------------------------
def test_wrapper_issues_the_warnings_before_an_r_error(bb_data):
    from rpy2.rinterface_lib.embedded import RRuntimeError

    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        with pytest.raises(RRuntimeError, match="prev_recommended_interventions"):
            lago.optimize(
                data=bb_data, outcome_name="pp3_oxytocin_mother", outcome_type="binary",
                intervention_components=COMPONENTS, intervention_lower_bounds=[1, 1],
                intervention_upper_bounds=[40, 5], cost_list=[[0, 1.7], [0, 8]],
                outcome_goal=0.999, prev_recommended_interventions=[1],
            )
    messages = [str(w.message) for w in caught if issubclass(w.category, lago.LagoRWarning)]
    assert len(messages) == 2
    assert all("lower bound" in m for m in messages)


def test_mcp_error_result_carries_the_warnings_before_the_error(bb_csv):
    out = S.optimize(**_bb_args(data_csv=bb_csv, outcome_goal=0.999, prev_recommended_interventions=[1.0]))
    assert out["error"].startswith("RRuntimeError: The length of prev_recommended_interventions")
    assert len(out["warnings"]) == 2
    assert all("lower bound" in m for m in out["warnings"])


def test_a_call_without_an_error_still_returns_its_value(bb_data):
    with _bridge.collect_warnings() as found:
        res = lago.optimize(
            data=bb_data, outcome_name="pp3_oxytocin_mother", outcome_type="binary",
            intervention_components=COMPONENTS, intervention_lower_bounds=[1, 1],
            intervention_upper_bounds=[40, 5], cost_list=[[0, 1.7], [0, 8]],
            outcome_goal=0.85, include_confidence_set=False,
        )
    assert len(res["rec_int"]) == 2
    assert found


# --------------------------------------------------------------------------
# 2. each tool describes only its own arguments, and refuses unknown ones
# --------------------------------------------------------------------------
def _tool(name):
    """The tool as a client sees it, through the public list_tools()."""
    tools = {t.name: t for t in asyncio.run(S.mcp.list_tools())}
    return tools[name]


def test_budget_description_leaves_out_what_budget_does_not_take():
    text = _tool("budget").description
    for name in ("group_column", "treatment_value", "prev_recommended_interventions"):
        assert name not in text
    for tool in ("optimize", "sensitivity"):
        text = _tool(tool).description
        for name in ("group_column", "treatment_value", "prev_recommended_interventions"):
            assert name in text, (tool, name)


@pytest.mark.parametrize("tool", ["optimize", "sensitivity", "budget"])
def test_every_argument_in_the_model_doc_is_an_argument_of_the_tool(tool):
    fn = getattr(S, tool)
    names = set(re.findall(r"`([a-z_]+)`", S._model_doc(fn)))
    params = set(_tool(tool).inputSchema["properties"])
    assert names and names <= params, names - params


@pytest.mark.parametrize("tool", ["describe_data", "optimize", "sensitivity", "budget"])
def test_an_unknown_argument_is_a_json_error(tool):
    assert _tool(tool).inputSchema["additionalProperties"] is False
    body, is_error = _call(tool, {"bogus_arg": 1})
    assert is_error
    assert f"`bogus_arg` is not an argument of {tool}." in body["error"]
    assert body["warnings"] == []


def test_budget_refuses_an_argument_of_optimize(bb_csv):
    body, is_error = _call("budget", _bb_args(data_csv=bb_csv, budget=50.0, group_column="x"))
    assert is_error
    assert body["error"] == "ValueError: the arguments do not fit budget. `group_column` is not an argument of budget."


def test_good_arguments_still_run_the_tool(bb_csv):
    body, is_error = _call("describe_data", {"data_csv": bb_csv})
    assert not is_error and body["n_rows"] > 0


# --------------------------------------------------------------------------
# 3. a schema error is the same JSON error as every other error
# --------------------------------------------------------------------------
def test_a_wrong_type_and_a_missing_argument_are_json_errors(bb_csv):
    body, is_error = _call("optimize", _bb_args(data_csv=bb_csv, outcome_goal=0.8, intervention_lower_bounds=1))
    assert is_error
    assert body == {
        "error": "ValueError: the arguments do not fit optimize. `intervention_lower_bounds`: Input should be a valid list.",
        "warnings": [],
    }
    args = _bb_args(data_csv=bb_csv)
    del args["outcome_name"]
    body, is_error = _call("optimize", args)
    assert is_error and "`outcome_name` is required." in body["error"]


def test_a_null_number_is_a_json_error_that_names_nan(bb_csv):
    # a NaN reaches the tool as null
    body, is_error = _call("optimize", _bb_args(data_csv=bb_csv, outcome_goal=0.8, intervention_upper_bounds=[40.0, None]))
    assert is_error
    assert "`intervention_upper_bounds.1`: Input should be a valid number." in body["error"]
    assert "JSON has no NaN or Infinity" in body["error"]


# --------------------------------------------------------------------------
# 4. pandas arrays, categoricals and dates become plain values
# --------------------------------------------------------------------------
def test_plain_turns_pandas_arrays_into_lists():
    assert _bridge._plain(pd.Series(["a", "b", "a"]).unique()) == ["a", "b"]
    assert _bridge._plain(pd.array(["a", None], dtype="string")) == ["a", None]
    assert _bridge._plain(pd.array([1, None], dtype="Int64")) == [1, None]
    assert _bridge._plain(pd.Series([1.5, np.nan])) == [1.5, pytest.approx(np.nan, nan_ok=True)]
    assert _bridge._plain(pd.Categorical([2, None, 10])) == ["2", None, "10"]
    assert _bridge._plain(pd.Series(["x", "y"], dtype="category")) == ["x", "y"]
    assert _bridge._plain(pd.NA) is None


def test_plain_turns_dates_into_the_text_r_gives():
    assert _bridge._plain(np.datetime64("2020-01-01T00:00:00.000000000")) == "2020-01-01"
    assert _bridge._plain(np.datetime64("2020-01-01")) == "2020-01-01"
    assert _bridge._plain(np.datetime64("NaT")) is None
    assert _bridge._plain(pd.Timestamp("2020-01-01 10:30")) == "2020-01-01 10:30:00"
    assert _bridge._plain(datetime.date(2020, 3, 4)) == "2020-03-04"
    assert _bridge._plain(pd.NaT) is None
    assert _bridge._plain(np.array(["2020-01-01", "NaT"], dtype="datetime64[ns]")) == ["2020-01-01", None]
    assert _bridge._plain(pd.Series(pd.to_datetime(["2020-01-02"]))) == ["2020-01-02"]
    assert _bridge._plain(np.array([[1, 2], [3, 4]])) == [[1, 2], [3, 4]]


def test_optimize_takes_pandas_arrays(bb_data):
    res = lago.optimize(
        data=bb_data, outcome_name="pp3_oxytocin_mother", outcome_type="binary",
        intervention_components=pd.Series(COMPONENTS + COMPONENTS[:1]).unique(),
        intervention_lower_bounds=pd.array([1, 1], dtype="Int64"), intervention_upper_bounds=[40, 5],
        cost_list=[[0, 1.7], [0, 8]], outcome_goal=0.85, include_confidence_set=False,
    )
    assert len(res["rec_int"]) == 2


def test_a_date_names_the_period_of_a_date_column(mtcars_dates):
    res = lago.optimize(
        data=mtcars_dates, outcome_name="mpg", outcome_type="continuous", intervention_components=["gear", "qsec"],
        intervention_lower_bounds=[0, 0], intervention_upper_bounds=[10, 350], cost_list=[[0, 4], [4, 6]],
        glm_family="gaussian", link="identity", outcome_goal=25, include_confidence_set=False,
        include_time_effects=True, time_effect_optimization_value=np.datetime64("2020-02-01"),
    )
    assert len(res["rec_int"]) == 2


@pytest.fixture(scope="module")
def mtcars_dates(ro):
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    with localconverter(ro.default_converter + pandas2ri.converter):
        mt = ro.conversion.get_conversion().rpy2py(ro.r("mtcars"))
    mt = mt[["mpg", "gear", "qsec"]].reset_index(drop=True)
    mt["period"] = pd.to_datetime(["2020-01-01", "2020-02-01"] * 16)
    return mt


# --------------------------------------------------------------------------
# 5. missing values in text columns and in arguments
# --------------------------------------------------------------------------
def test_text_na_of_an_r_data_frame_becomes_none(ro):
    with _bridge.r_context():
        df = _bridge.r_to_py(ro.r("data.frame(c = c('x', NA), n = c(1.5, NA), k = c('a', 'b'))"))
        assert df["c"].tolist() == ["x", None]
        assert df["c"].isna().tolist() == [False, True]
        assert S._records(df)[1]["c"] is None
        back = _bridge.py_to_r(df)
        assert list(ro.r("function(d) is.na(d$c)")(back)) == [False, True]
        assert list(ro.r("function(d) is.na(d$k)")(back)) == [False, False]


def test_none_in_a_list_becomes_the_r_na_of_its_type(ro):
    is_na = ro.r("is.na")
    cls = ro.r("function(x) class(x)")
    with _bridge.r_context():
        cases = [
            ([True, None], "logical"),
            ([1, None], "numeric"),
            (["a", None], "character"),
            ([None, None], "logical"),
            (pd.Series([1, None], dtype="Int64"), "numeric"),
            (pd.Series([True, None], dtype="boolean"), "logical"),
        ]
        for value, kind in cases:
            vec = _bridge.py_to_r(value)
            assert cls(vec)[0] == kind, value
            assert list(is_na(vec))[1] is True, value
        assert list(_bridge.py_to_r([]) ) == []
        with pytest.raises(TypeError, match="mixed/unsupported"):
            _bridge.py_to_r([1, "a", None])


# --------------------------------------------------------------------------
# 6. the docs name every field, and say what NaN does
# --------------------------------------------------------------------------
def test_sensitivity_docs_name_every_column():
    for text in (_tool("sensitivity").description, lago.sensitivity.__doc__):
        for name in ("shrinking_method_used", "effective_outcome_goal", "error_message"):
            assert name in text


def test_optimize_docs_give_effective_outcome_goal_only_with_a_power_goal():
    text = " ".join(_tool("optimize").description.split())
    assert "with a power goal `effective_outcome_goal`" in text


def test_the_docs_say_what_a_nan_does():
    for tool in ("optimize", "sensitivity", "budget"):
        text = " ".join(_tool(tool).description.split())
        assert "JSON has no NaN or Infinity" in text
        assert "An argument the tool does not take is an error." in text


def test_the_server_uses_no_private_part_of_the_sdk():
    import inspect

    source = inspect.getsource(S)
    for private in ("_tool_manager", "_mcp_server", "fn_metadata", "FuncMetadata"):
        assert private not in source, private


def test_a_list_or_object_sent_as_json_text_is_still_read(bb_csv):
    # FastMCP reads a list sent as JSON text, so the check before it does too
    body, is_error = _call(
        "optimize",
        _bb_args(data_csv=bb_csv, intervention_lower_bounds="[1, 1]", outcome_goal=0.85, include_confidence_set=False),
    )
    assert not is_error, body
