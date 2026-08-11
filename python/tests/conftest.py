"""Shared pytest fixtures for the lago-python wrapper tests.

These tests EMBED R via rpy2 and require the LAGO R package to be installed.
If R / rpy2 / LAGO cannot be reached, the whole test module is skipped with a
clear reason rather than erroring.
"""
import warnings

import pytest


@pytest.fixture(scope="session")
def ro():
    ro = pytest.importorskip("rpy2.robjects")
    try:
        from rpy2.robjects.packages import importr
        importr("LAGO")
    except Exception as exc:  # pragma: no cover - environment guard
        pytest.skip(f"LAGO R package not importable via rpy2: {exc}")
    return ro


@pytest.fixture(scope="session")
def bb_data(ro):
    """R's built-in LAGO BB_data as a pandas DataFrame."""
    from rpy2.robjects.packages import importr, data
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    lagor = importr("LAGO")
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
    """LAGO emits expected data/model-fit warnings; keep test output clean."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        yield
