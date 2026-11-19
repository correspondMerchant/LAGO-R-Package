"""Internal rpy2 bridge for the LAGOtrials Python wrapper.

This module embeds R through rpy2 and calls the real ``LAGOtrials`` R functions. It
performs NO LAGOtrials math of its own; it only converts Python-native inputs to R
objects, invokes the exported R functions, and converts their results back to
Python-native types.

R and the installed ``LAGOtrials`` R package are required at runtime. rpy2 must be
able to reach R (``R_HOME`` set to the R installation). See the package README.
"""

from __future__ import annotations

import contextlib
import contextvars
import functools
import threading
import warnings

# Lazy, memoized rpy2 / R initialization so that ``import lago`` does not touch
# R until a wrapper function is actually called.
_state: dict = {}

# One R call at a time, re-entrant as one public function may call another.
_r_lock = threading.RLock()


class LagoRWarning(UserWarning):
    """A warning that the LAGOtrials R package gave, passed on to Python."""


# The list that collects the R warnings of the current context, when a caller
# such as the MCP server asks for them. A context variable keeps calls in
# different threads or tasks apart, which warnings.catch_warnings() does not.
_collected: contextvars.ContextVar = contextvars.ContextVar("lago_r_warnings", default=None)


@contextlib.contextmanager
def collect_warnings():
    """Collect the R warnings of the calls inside, instead of issuing them.

    Yields a list that holds each distinct warning message, in order.
    """
    found: list = []
    token = _collected.set(found)
    try:
        yield found
    finally:
        _collected.reset(token)


@contextlib.contextmanager
def r_context():
    """Run the code inside with rpy2's default conversion rules set.

    rpy2 keeps its conversion rules in a ``contextvars.ContextVar``. A thread,
    or an asyncio task that was not created from the context that first loaded
    rpy2, starts without them, and every conversion then fails with "Conversion
    rules for `rpy2.robjects` appear to be missing". The MCP server runs each
    tool call in a context of its own, so without this only its first call
    worked.

    R runs one call at a time, and rpy2's table of the R objects it keeps alive
    is not safe when two threads change it at once. A lock lets one thread in
    at a time, so every public function can be called from any thread or task.
    """
    import rpy2.robjects as ro
    from rpy2.robjects.conversion import localconverter

    with _r_lock, localconverter(ro.default_converter):
        _ensure()
        yield


def in_r_context(fn):
    """Decorate a public function so its whole body runs in :func:`r_context`."""

    @functools.wraps(fn)
    def wrapper(*args, **kwargs):
        with r_context():
            return fn(*args, **kwargs)

    return wrapper


# Calls an R function with a list of arguments, and returns its value with the
# warnings it gave, so they can be passed on to Python instead of only being
# printed to the R console.
_CALL_WITH_WARNINGS = """function(f, args) {
  warned <- character(0)
  value <- withCallingHandlers(do.call(f, args), warning = function(w) {
    warned <<- c(warned, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = unique(warned))
}"""


# Result fields that are GENUINELY single-valued and should be presented to the
# caller as a bare Python scalar (float/int/bool/str), not a length-1 list. Any
# result field NOT listed here keeps whatever container r_to_py produced: a
# semantic vector (rec_int, intervention_lower_bounds/upper_bounds, grid step
# sizes, intervention_components, cost_list_of_vectors, ...) stays a Python list
# (or list-of-lists) even when it holds a single value, and a named R vector
# (est_outcome_ci, rec_int_ci) stays a dict. This is the allowlist half of the
# "vectors stay lists, only real scalars collapse" design.
SCALAR_RESULT_FIELDS = frozenset({
    # lago_budget() scalars
    "budget",
    "feasible",
    "binding",
    "est_outcome",
    "intention",
    "run_error",
    # lago_optimization() / get_confidence_set() numeric scalars
    "rec_int_cost",
    "est_outcome_goal",
    "confidence_set_size_percentage",
    # echoed scalar metadata / inputs recap
    "outcome_type",
    "outcome_goal",
    "power_goal",
    "effective_outcome_goal",
    "shrinking_method_used",
    "outcome_name",
    "family",
    "link",
    "input_nrow",
    "input_ncol",
    "include_center_effects",
    "include_time_effects",
    "include_interaction_terms",
})


def scalarize(name: str, value):
    """Collapse a length-1 list to its element for genuinely-scalar fields.

    Applied per result field by name: only fields in
    :data:`SCALAR_RESULT_FIELDS` are unwrapped, and only when the converted
    value is a length-1 list. Everything else (multi-element lists, dicts,
    DataFrames, None, raw rpy2 objects) is returned unchanged, so semantic
    vector fields keep their list container even at length 1.
    """
    if name in SCALAR_RESULT_FIELDS and isinstance(value, list) and len(value) == 1:
        return value[0]
    return value


def scalarize_dict(d: dict) -> dict:
    """Apply :func:`scalarize` to every entry of a result dict, by key."""
    return {k: scalarize(k, v) for k, v in d.items()}


def _ensure() -> dict:
    """Initialize rpy2, import the LAGOtrials R package, and cache the handles."""
    if _state:
        return _state

    import rpy2.robjects as ro
    import rpy2.rinterface as ri
    from rpy2.robjects.packages import importr
    from rpy2.robjects import pandas2ri
    from rpy2.robjects.conversion import localconverter

    lago = importr("LAGOtrials")

    _state.update(
        ro=ro,
        ri=ri,
        lago=lago,
        pandas2ri=pandas2ri,
        localconverter=localconverter,
        rfuncs={},
    )
    return _state


def _pandas_converter():
    """Combined default + pandas<->R converter."""
    s = _ensure()
    return s["ro"].default_converter + s["pandas2ri"].converter


def _is_na(value) -> bool:
    """True for an R NA of any type, as rpy2 gives it, or a float NaN."""
    ri = _ensure()["ri"]
    if any(value is na for na in (ri.NA_Logical, ri.NA_Integer, ri.NA_Character)):
        return True
    return isinstance(value, float) and value != value


def _is_null(obj) -> bool:
    """True for the R NULL singleton."""
    s = _ensure()
    ri = s["ri"]
    if obj is ri.NULL:
        return True
    null_type = getattr(ri, "NULLType", None)
    if null_type is not None and isinstance(obj, null_type):
        return True
    # Fallback: robjects wraps NULL too.
    return str(type(obj)).endswith("NULLType'>")


def _plain(value):
    """Turn numpy and pandas values into plain Python values.

    A numpy scalar becomes its Python number, bool or str. A numpy array, a
    pandas Series or a pandas Index becomes a list (a 2D array a list of
    lists). The elements of a list or tuple are turned too. Anything else is
    returned unchanged. numpy comes with pandas, which the wrapper needs.
    """
    import numpy as np
    import pandas as pd

    if isinstance(value, np.generic):
        return value.item()
    if isinstance(value, (np.ndarray, pd.Series, pd.Index)):
        return [_plain(el) for el in value.tolist()]
    if isinstance(value, (list, tuple)):
        return [_plain(el) for el in value]
    return value


def py_to_r(value):
    """Convert a single Python value to the corresponding R object.

    - pandas.DataFrame -> R data.frame
    - bool / list-of-bool -> logical vector
    - int|float / list-of-number -> numeric vector
    - str / list-of-str -> character vector
    - list-of-lists (e.g. cost_list_of_vectors) -> R list of numeric vectors
    - None -> R NULL
    - numpy scalars and arrays, pandas Series and Index -> as the plain
      Python values they hold
    - an existing rpy2 object -> passed through unchanged
    """
    s = _ensure()
    ro = s["ro"]
    import pandas as pd
    from rpy2.robjects.vectors import (
        FloatVector,
        StrVector,
        BoolVector,
    )

    if value is None:
        return ro.NULL

    if isinstance(value, pd.DataFrame):
        with s["localconverter"](_pandas_converter()):
            return ro.conversion.get_conversion().py2rpy(value)

    value = _plain(value)

    # bool must be checked before int (bool is a subclass of int)
    if isinstance(value, bool):
        return BoolVector([value])
    if isinstance(value, (int, float)):
        return FloatVector([float(value)])
    if isinstance(value, str):
        return StrVector([value])

    if isinstance(value, (list, tuple)):
        seq = list(value)
        if len(seq) > 0 and all(isinstance(el, (list, tuple)) for el in seq):
            # list-of-lists -> unnamed R list of numeric vectors
            rlist = ro.r["list"]
            return rlist(
                *[FloatVector([float(x) for x in el]) for el in seq]
            )
        if len(seq) > 0 and all(isinstance(el, bool) for el in seq):
            return BoolVector(seq)
        if all(isinstance(el, (int, float)) and not isinstance(el, bool)
               for el in seq):
            return FloatVector([float(x) for x in seq])
        if all(isinstance(el, str) for el in seq):
            return StrVector(seq)
        raise TypeError(
            "Cannot convert list with mixed/unsupported element types to an "
            "R vector: {!r}".format(seq)
        )

    # Already an rpy2 object (escape hatch) -> pass through unchanged.
    return value


def r_to_py(obj):
    """Convert an R object to a Python-native value.

    NULL -> None, data.frame -> pandas.DataFrame, named vector -> dict,
    UNNAMED vector -> Python list (ALWAYS, even length 1), R list ->
    dict/list (each element converted recursively, so a list of numeric
    vectors round-trips as a list of lists with its nesting intact).
    Anything that does not convert cleanly (e.g. a fitted glm model) is
    returned as the raw rpy2 object.

    This function deliberately does NOT collapse a length-1 vector to a bare
    scalar: doing so would corrupt the CONTAINER TYPE of semantic vectors
    (``rec_int``, ``intervention_lower_bounds``/``upper_bounds``, grid step
    sizes, a degree-0 cost component, ...) whenever they happen to hold a
    single value. Unwrapping to a scalar is instead applied field-by-field
    in the result marshaling (see :data:`SCALAR_RESULT_FIELDS` and
    :func:`scalarize`) only for fields that are genuinely single-valued.
    """
    s = _ensure()
    ro = s["ro"]
    ri = s["ri"]
    from rpy2.robjects.vectors import (
        FloatVector,
        IntVector,
        BoolVector,
        StrVector,
        ListVector,
        DataFrame,
        FactorVector,
    )

    if _is_null(obj):
        return None

    if isinstance(obj, DataFrame):
        with s["localconverter"](_pandas_converter()):
            return ro.conversion.get_conversion().rpy2py(obj)

    if isinstance(obj, FactorVector):
        levels = list(obj.levels)
        out = []
        for code in list(obj):
            # R factor codes are 1-based; NA shows up as a large negative /
            # NA integer.
            try:
                idx = int(code)
            except (TypeError, ValueError):
                out.append(None)
                continue
            out.append(levels[idx - 1] if 1 <= idx <= len(levels) else None)
        return out

    if isinstance(obj, ListVector):
        names = obj.names
        if _is_null(names):
            # Each element is converted on its own, so a length-1 numeric
            # vector element stays a length-1 list -> the list-of-vectors
            # nesting (e.g. cost_list_of_vectors) is preserved, never
            # flattened.
            return [r_to_py(el) for el in obj]
        return {str(n): r_to_py(obj.rx2(n)) for n in list(names)}

    if isinstance(obj, (FloatVector, IntVector, BoolVector, StrVector)):
        vals = list(obj)
        names = obj.names
        if not _is_null(names):
            return {str(k): v for k, v in zip(list(names), vals)}
        # A plain (unnamed) vector ALWAYS becomes a Python list, even length
        # 1. Scalar unwrapping is a result-field concern, not a conversion
        # concern (see scalarize / SCALAR_RESULT_FIELDS).
        return vals

    # Fallback: hand back the raw rpy2 object (e.g. a fitted glm model).
    return obj


def _get_rfunc(name: str):
    """Fetch (and cache) an exported LAGOtrials R function object.

    Uses a plain robjects Function via ``LAGOtrials::name`` so that keyword argument
    names are passed to R verbatim (no underscore/dot signature translation).
    """
    s = _ensure()
    cache = s["rfuncs"]
    if name not in cache:
        cache[name] = s["ro"].r("LAGOtrials::" + name)
    return cache[name]


def build_r_kwargs(kwargs: dict) -> dict:
    """Convert a dict of Python kwargs to a dict of R objects (no R call)."""
    return {k: py_to_r(v) for k, v in kwargs.items()}


def call_lago(name: str, kwargs: dict):
    """Convert kwargs, call the exported LAGOtrials R function, return the R result.

    Each warning the R function gives is issued again as a Python
    :class:`LagoRWarning`, so a caller can read it with
    :func:`warnings.catch_warnings`.
    """
    s = _ensure()
    rfunc = _get_rfunc(name)
    r_kwargs = build_r_kwargs(kwargs)
    if "call_with_warnings" not in s:
        s["call_with_warnings"] = s["ro"].r(_CALL_WITH_WARNINGS)
    out = s["call_with_warnings"](rfunc, s["ro"].r["list"](**r_kwargs))
    found = _collected.get()
    for message in (str(m) for m in out.rx2("warnings")):
        if found is None:
            # 4 points at the line that called the public function
            warnings.warn(message, LagoRWarning, stacklevel=4)
        elif message not in found:
            found.append(message)
    return out.rx2("value")


def r_sensitivity_to_df(result):
    """Convert an R ``lago_sensitivity`` result to a pandas ``DataFrame``.

    ``lago_sensitivity()`` returns a ``data.frame`` that carries an extra S3
    class (``"lago_sensitivity"``). :func:`r_to_py` already maps an R
    ``data.frame`` to a ``pandas.DataFrame``; this thin, well-named wrapper
    reuses it and additionally tolerates the case where the extra class keeps
    rpy2 from tagging the object as a ``DataFrame`` (then :func:`r_to_py`
    yields a column dict, which is rebuilt into a ``DataFrame`` with the column
    order preserved).
    """
    import pandas as pd

    out = r_to_py(result)
    if isinstance(out, dict):
        # A named R list converts to {column_name: list}; the dict preserves
        # the R column order, so the DataFrame keeps value / <components> /
        # rec_int_cost / est_outcome_goal / status in order.
        out = pd.DataFrame({k: v for k, v in out.items()})
    if not isinstance(out, pd.DataFrame):
        raise TypeError(
            "lago_sensitivity() result did not convert to a data.frame; got "
            "{}".format(type(out).__name__)
        )
    # R keeps what each run did beyond its numbers as attributes: whether the
    # goal was out of reach so the shrinking method was used, the goal used,
    # and the error of a failed run. They become columns, so a run that missed
    # its goal or failed does not read as a normal result.
    attr = _ensure()["ro"].r["attr"]
    n = len(out)
    for name in ("shrinking_method_used", "effective_outcome_goal"):
        values = r_to_py(attr(result, name))
        if isinstance(values, list) and len(values) == n:
            # a failed run has NA, which rpy2 gives as its own NA object
            out[name] = [None if _is_na(v) else v for v in values]
    messages = [None] * n
    errors = r_to_py(attr(result, "error_messages"))
    if isinstance(errors, dict):
        # R names each error by its swept value, as as.character() writes it
        for value, message in errors.items():
            for i, v in enumerate(out["value"]):
                try:
                    if abs(float(value) - float(v)) <= 1e-9 * max(1.0, abs(float(v))):
                        messages[i] = message
                except (TypeError, ValueError):
                    continue
    out["error_message"] = messages
    return out


def lago_result_to_dict(result) -> dict:
    """Convert a "lago" result object into a Python dict.

    The fitted ``model`` (an R glm object) does not convert cleanly, so it is
    kept as the raw rpy2 object under ``"model"``. The whole underlying R
    object is stored under ``"_r_object"`` so it can be handed to
    ``lago.lago_report()`` without re-running the optimization.
    """
    out = {}
    names = result.names
    if _is_null(names):
        # Unnamed (unexpected) - convert positionally.
        return {"_r_object": result, "_values": r_to_py(result)}
    for n in list(names):
        val = result.rx2(n)
        if n == "model":
            out["model"] = val  # raw rpy2 glm object
        else:
            # r_to_py never scalar-collapses; unwrap here ONLY for the fields
            # that are genuinely single-valued, so semantic vectors (rec_int,
            # bounds, cost_list_of_vectors, ...) keep their list container even
            # when length 1.
            out[n] = scalarize(str(n), r_to_py(val))
    out["_r_object"] = result
    return out
