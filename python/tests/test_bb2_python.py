"""Tests for the fixes of bug bash 2 in the Python wrapper and the MCP server."""
import asyncio
import json
import os
import signal
import subprocess
import sys
import time
from decimal import Decimal

import numpy as np
import pandas as pd
import pytest

if os.environ.get("LAGO_REQUIRE_R") != "1":
    pytest.importorskip("mcp.server.fastmcp", reason="mcp SDK not installed")

import lago  # noqa: E402
from lago import _bridge  # noqa: E402
from lago import mcp_server as S  # noqa: E402

PYTHON_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


@pytest.fixture(scope="module")
def mtcars(ro):
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    with localconverter(ro.default_converter + pandas2ri.converter):
        mt = ro.conversion.get_conversion().rpy2py(ro.r("mtcars"))
    return mt[["mpg", "gear", "qsec"]].reset_index(drop=True)


@pytest.fixture(scope="module")
def bb_csv(ro, tmp_path_factory):
    path = tmp_path_factory.mktemp("bb2") / "bb.csv"
    ro.r(f'write.csv(LAGOtrials::BB_data, "{path}", row.names = FALSE)')
    return str(path)


def _args(cost=(([0.0, 4.0], [4.0, 6.0]))):
    return dict(
        outcome_name="mpg", outcome_type="continuous", intervention_components=["gear", "qsec"],
        intervention_lower_bounds=[0.0, 0.0], intervention_upper_bounds=[10.0, 350.0],
        cost_list_of_vectors=[list(c) for c in cost], glm_family="gaussian", link="identity",
    )


def _payload(result):
    """The JSON body and the error flag of a FastMCP call_tool result."""
    from mcp.types import CallToolResult

    if isinstance(result, CallToolResult):
        return json.loads(result.content[0].text), bool(result.isError)
    content = result[0] if isinstance(result, tuple) else result
    return json.loads(content[0].text), False


# --------------------------------------------------------------------------
# 1. cancelling a call that waits does not stop the call that runs
# --------------------------------------------------------------------------
def test_cancelling_a_waiting_call_leaves_the_running_call_alone(mtcars):
    records = mtcars.to_dict(orient="records")

    async def scenario():
        running = asyncio.create_task(S.mcp.call_tool("budget", dict(data_records=records, budget=200.0, n_grid=25, **_args())))
        await asyncio.sleep(1.0)
        proc = S._WORKER._proc
        waiting = asyncio.create_task(S.mcp.call_tool("describe_data", dict(data_records=records)))
        await asyncio.sleep(0.3)
        waiting.cancel()
        try:
            await waiting
        except asyncio.CancelledError:
            pass
        return proc, await running

    proc, result = asyncio.run(scenario())
    body, is_error = _payload(result)
    assert not is_error and "error" not in body, body
    assert body["feasible"] is True
    # the running call kept its worker
    assert proc is not None and proc.is_alive() and S._WORKER._proc is proc
    S._WORKER.stop()


def test_a_call_cancelled_while_it_waits_never_runs():
    worker = S._RWorker()
    call = S._Call()
    worker.cancel(call)
    assert call.cancelled
    assert worker.call("describe_data", {}, call) is None
    # it did not even start the worker
    assert worker._proc is None


def test_a_dead_worker_is_cleared_by_its_own_call(monkeypatch):
    worker = S._RWorker()
    worker.start()
    proc = worker._proc
    worker.kill()
    proc.join(5)
    # as if the process died after the call found it alive
    monkeypatch.setattr(worker, "start", lambda: None)
    with pytest.raises((EOFError, OSError)):
        worker.call("describe_data", {})
    assert worker._proc is None and worker._running is None
    monkeypatch.undo()
    # the next call starts a new worker
    assert worker.call("describe_data", {})["error"].startswith("ValueError")
    worker.stop()
    worker.kill()


# --------------------------------------------------------------------------
# 2. stopping the server stops its R worker, even in the middle of a call
# --------------------------------------------------------------------------
def _children(pid):
    out = []
    for entry in os.listdir("/proc"):
        if not entry.isdigit():
            continue
        try:
            with open(f"/proc/{entry}/stat") as f:
                stat = f.read()
            with open(f"/proc/{entry}/cmdline", "rb") as f:
                cmd = f.read().decode(errors="replace")
        except OSError:
            continue
        if int(stat.rsplit(")", 1)[1].split()[1]) == pid and "spawn_main" in cmd:
            out.append(int(entry))
    return out


def _alive(pid):
    try:
        with open(f"/proc/{pid}/stat") as f:
            return f.read().rsplit(")", 1)[1].split()[0] != "Z"
    except OSError:
        return False


@pytest.mark.skipif(not sys.platform.startswith("linux"), reason="reads /proc")
@pytest.mark.parametrize("sig", [signal.SIGTERM, signal.SIGKILL])
def test_the_worker_does_not_outlive_the_server(bb_csv, sig):
    env = dict(os.environ, PYTHONPATH=os.pathsep.join([PYTHON_DIR] + [p for p in [os.environ.get("PYTHONPATH")] if p]))
    server = subprocess.Popen([sys.executable, "-m", "lago.mcp_server"], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                              stderr=subprocess.DEVNULL, env=env, cwd=PYTHON_DIR, text=True)
    workers = []
    try:
        def send(msg):
            server.stdin.write(json.dumps(msg) + "\n")
            server.stdin.flush()

        send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {
            "protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "test", "version": "0"}}})
        assert json.loads(server.stdout.readline())["id"] == 1
        send({"jsonrpc": "2.0", "method": "notifications/initialized"})
        workers = _children(server.pid)
        assert len(workers) == 1
        # a long budget search, to have the worker busy in R
        send({"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "budget", "arguments": dict(
            data_csv=bb_csv, outcome_name="pp3_oxytocin_mother", outcome_type="binary",
            intervention_components=["coaching_updt", "launch_duration"],
            intervention_lower_bounds=[1, 1], intervention_upper_bounds=[40, 5],
            unit_costs=[1700, 8000], default_cost_fxn_type="linear", budget=50000, n_grid=150)}})
        time.sleep(4)
        assert _alive(workers[0])
        server.send_signal(sig)
        assert server.wait(10) == -sig
        for _ in range(50):
            if not _alive(workers[0]):
                break
            time.sleep(0.1)
        assert not _alive(workers[0])
    finally:
        if server.poll() is None:
            server.kill()
            server.wait(10)
        for pid in workers:
            if _alive(pid):
                os.kill(pid, signal.SIGKILL)


def test_the_sigterm_handler_kills_the_worker_and_ends_the_process(monkeypatch):
    killed, sent, handlers = [], [], {}
    monkeypatch.setattr(S._WORKER, "kill", lambda: killed.append(True))
    monkeypatch.setattr(signal, "signal", lambda signum, handler: handlers.__setitem__(signum, handler))
    monkeypatch.setattr(os, "kill", lambda pid, signum: sent.append((pid, signum)))
    S._on_sigterm(signal.SIGTERM, None)
    assert killed == [True]
    assert handlers == {signal.SIGTERM: signal.SIG_DFL}
    assert sent == [(os.getpid(), signal.SIGTERM)]


def test_the_watchdog_exits_when_the_parent_changes(monkeypatch):
    exited = []

    class Exit(Exception):
        pass

    def fake_exit(code):
        exited.append(code)
        raise Exit

    monkeypatch.setattr(os, "_exit", fake_exit)
    with pytest.raises(Exit):
        S._watch_parent(os.getppid() + 1, interval=0.01)
    assert exited == [1]


# --------------------------------------------------------------------------
# 3. sensitivity and budget keep every digit, as optimize does
# --------------------------------------------------------------------------
def test_tables_keep_full_precision(mtcars):
    records = mtcars.to_dict(orient="records")
    tiny = _args(cost=([0.0, 4e-9], [4e-9, 6e-9]))
    opt = S.optimize(data_records=records, outcome_goal=30.0, include_confidence_set=False, **tiny)
    sens = S.sensitivity(data_records=records, parameter="outcome_goal", values=[30.0], **tiny)
    assert "error" not in opt and "error" not in sens, (opt, sens)
    run = sens["runs"][0]
    # a cost near 1e-7 used to come back rounded to 10 decimal places
    assert run["rec_int_cost"] == pytest.approx(opt["rec_int_cost"], rel=1e-12)
    assert run["rec_int_cost"] != round(run["rec_int_cost"], 10)
    assert [run["gear"], run["qsec"]] == pytest.approx(opt["rec_int"], rel=1e-12)

    b = S.budget(data_records=records, budget=2e-7, n_grid=6, **tiny)
    assert "error" not in b, b
    direct = lago.budget(data=mtcars, budget=2e-7, n_grid=6, cost_list=tiny.pop("cost_list_of_vectors"), quiet=True, **tiny)
    costs = [r["rec_int_cost"] for r in b["frontier"]]
    want = [None if v != v else float(v) for v in direct["frontier"]["rec_int_cost"]]
    assert costs == want
    assert any(c is not None and c != round(c, 10) for c in costs)


def test_records_turn_every_cell_into_plain_json():
    df = pd.DataFrame({
        "x": [0.12345678901234567], "n": [np.int64(3)], "b": [np.bool_(True)], "s": ["a"],
        "na": pd.array([pd.NA], dtype="Int64"), "nan": [np.nan], "inf": [np.inf], "none": [None], "d": [Decimal("1.5")],
    })
    rows = S._records(df)
    assert rows == [{"x": 0.12345678901234567, "n": 3, "b": True, "s": "a", "na": None, "nan": None,
                     "inf": None, "none": None, "d": "1.5"}]
    assert type(rows[0]["n"]) is int and type(rows[0]["b"]) is bool
    json.dumps(rows, allow_nan=False)


def test_the_confidence_set_keeps_full_precision(mtcars):
    records = mtcars.to_dict(orient="records")
    opt = S.optimize(data_records=records, outcome_goal=30.0, confidence_set_grid_step_size=[1.0, 1.0], **_args())
    assert "error" not in opt and opt["confidence_set"], opt
    direct = lago.optimize(data=mtcars, outcome_goal=30.0, confidence_set_grid_step_size=[1.0, 1.0],
                           cost_list=_args().pop("cost_list_of_vectors"),
                           **{k: v for k, v in _args().items() if k != "cost_list_of_vectors"})
    assert opt["confidence_set"] == S._records(direct["cs"])


# --------------------------------------------------------------------------
# 4. numpy and pandas inputs are accepted
# --------------------------------------------------------------------------
def test_py_to_r_takes_numpy_and_pandas_values(ro):
    def r_values(value):
        return list(_bridge.py_to_r(value))

    with _bridge.r_context():
        assert r_values(np.int64(3)) == [3.0]
        assert r_values(np.float32(0.5)) == [0.5]
        assert r_values(np.bool_(True)) == [True]
        assert r_values(np.array([1, 2])) == [1.0, 2.0]
        assert r_values([np.int64(1), 2.5]) == [1.0, 2.5]
        assert r_values(pd.Series([1.5, 2.5])) == [1.5, 2.5]
        assert r_values(pd.Index(["a", "b"])) == ["a", "b"]
        assert r_values((np.bool_(False), True)) == [False, True]
        nested = _bridge.py_to_r(np.array([[0.0, 1.0], [2.0, 3.0]]))
        assert [list(v) for v in nested] == [[0.0, 1.0], [2.0, 3.0]]


def test_optimize_takes_numpy_and_pandas_arguments(mtcars):
    common = dict(data=mtcars, outcome_name="mpg", outcome_type="continuous", outcome_goal=30.0,
                  glm_family="gaussian", link="identity", include_confidence_set=False)
    plain = lago.optimize(intervention_components=["gear", "qsec"], intervention_lower_bounds=[0.0, 0.0],
                          intervention_upper_bounds=[10.0, 350.0], cost_list=[[0.0, 4.0], [4.0, 6.0]], **common)
    fancy = lago.optimize(intervention_components=mtcars.columns[1:3], intervention_lower_bounds=np.array([0, 0]),
                          intervention_upper_bounds=pd.Series([np.int64(10), 350]),
                          cost_list=np.array([[0.0, 4.0], [4.0, 6.0]]), **common)
    assert fancy["rec_int"] == plain["rec_int"]


# --------------------------------------------------------------------------
# 5. a failed tool call is an error result, with the JSON body as its text
# --------------------------------------------------------------------------
def test_a_failed_call_is_an_error_result_over_json_rpc():
    from mcp import types

    handler = S.mcp._mcp_server.request_handlers[types.CallToolRequest]
    req = types.CallToolRequest(method="tools/call", params=types.CallToolRequestParams(name="describe_data", arguments={}))
    res = asyncio.run(handler(req)).root
    assert res.isError is True
    body = json.loads(res.content[0].text)
    assert "exactly one" in body["error"] and body["warnings"] == []


def test_a_failed_r_call_is_an_error_result_with_its_warnings(mtcars):
    records = mtcars.to_dict(orient="records")
    res = asyncio.run(S.mcp.call_tool("budget", dict(data_records=records, budget=-1.0, **_args())))
    body, is_error = _payload(res)
    assert is_error and "budget" in body["error"] and isinstance(body["warnings"], list)
    ok = asyncio.run(S.mcp.call_tool("describe_data", dict(data_records=records)))
    body, is_error = _payload(ok)
    assert not is_error and body["n_rows"] == 32
    S._WORKER.stop()


def test_a_worker_that_dies_gives_an_error_result(monkeypatch):
    def dead(*args):
        raise EOFError()

    monkeypatch.setattr(S._WORKER, "call", dead)
    res = asyncio.run(S.mcp.call_tool("describe_data", {}))
    body, is_error = _payload(res)
    assert is_error and "the R worker stopped during the call" in body["error"] and body["warnings"] == []


# --------------------------------------------------------------------------
# 6. every result has a warnings list
# --------------------------------------------------------------------------
def test_every_result_has_a_warnings_list(mtcars):
    records = mtcars.to_dict(orient="records")
    assert S.describe_data(data_records=records)["warnings"] == []
    assert S.describe_data()["warnings"] == []
    assert S.optimize(outcome_goal=30.0, **_args())["warnings"] == []
    assert S.sensitivity(parameter="outcome_goal", values=[30.0], **_args())["warnings"] == []
    assert S.budget(budget=10.0, **_args())["warnings"] == []
    assert S._error(ValueError("x"), ["w"]) == {"error": "ValueError: x", "warnings": ["w"]}

