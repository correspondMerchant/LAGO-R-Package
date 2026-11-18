"""Shared pytest fixtures for the lago-python wrapper tests.

These tests EMBED R via rpy2 and require the LAGOtrials R package to be installed.
If R / rpy2 / LAGOtrials cannot be reached, the whole test module is skipped with a
clear reason rather than erroring.
"""
import os
import warnings

import pytest


def _missing(reason):
    """Skip the tests that need R, or fail them in CI, where R must be there.

    The python-test workflow sets LAGO_REQUIRE_R=1, so a broken setup fails the
    job instead of passing it with every test skipped.
    """
    if os.environ.get("LAGO_REQUIRE_R") == "1":
        pytest.fail(reason)
    pytest.skip(reason)


@pytest.fixture(scope="session")
def ro():
    try:
        import rpy2.robjects as ro
    except Exception as exc:  # pragma: no cover - environment guard
        _missing(f"rpy2 is not importable: {exc}")
    try:
        from rpy2.robjects.packages import importr
        importr("LAGOtrials")
    except Exception as exc:  # pragma: no cover - environment guard
        _missing(f"LAGOtrials R package not importable via rpy2: {exc}")
    return ro


@pytest.fixture(scope="session")
def bb_data(ro):
    """R's built-in LAGOtrials BB_data as a pandas DataFrame."""
    from rpy2.robjects.packages import importr, data
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    lagor = importr("LAGOtrials")
    bb_r = data(lagor).fetch("BB_data")["BB_data"]
    with localconverter(ro.default_converter + pandas2ri.converter):
        return ro.conversion.get_conversion().rpy2py(bb_r)


@pytest.fixture(scope="session")
def infert(ro):
    """R's built-in infert dataset as a pandas DataFrame."""
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    infert_r = ro.r("infert")
    with localconverter(ro.default_converter + pandas2ri.converter):
        return ro.conversion.get_conversion().rpy2py(infert_r)


@pytest.fixture(autouse=True)
def _silence_r_warnings():
    """LAGOtrials emits expected data/model-fit warnings; keep test output clean."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        yield
