"""Internal rpy2 bridge for the LAGO Python wrapper.

This module embeds R through rpy2 and calls the real ``LAGO`` R functions. It
performs NO LAGO math of its own; it only converts Python-native inputs to R
objects, invokes the exported R functions, and converts their results back to
Python-native types.

R and the installed ``LAGO`` R package are required at runtime. rpy2 must be
able to reach R (``R_HOME`` set to the R installation). See the package README.
"""

from __future__ import annotations

# Lazy, memoized rpy2 / R initialization so that ``import lago`` does not touch
# R until a wrapper function is actually called.
_state: dict = {}


# Result fields that are GENUINELY single-valued and should be presented to the
# caller as a bare Python scalar (float/int/bool/str), not a length-1 list. Any
# result field NOT listed here keeps whatever container r_to_py produced: a
# semantic vector (rec_int, intervention_lower_bounds/upper_bounds, grid step
# sizes, intervention_components, cost_list_of_vectors, ...) stays a Python list
# (or list-of-lists) even when it holds a single value, and a named R vector
# (est_outcome_ci, rec_int_ci) stays a dict. This is the allowlist half of the
# "vectors stay lists, only real scalars collapse" design.
SCALAR_RESULT_FIELDS = frozenset({
    # lago_optimization() / get_confidence_set() numeric scalars
    "rec_int_cost",
    "est_outcome_goal",
    "confidence_set_size_percentage",
    # echoed scalar metadata / inputs recap
    "outcome_type",
    "outcome_goal",
    "power_goal",
    "effective_outcome_goal",
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
    """Initialize rpy2, import the LAGO R package, and cache the handles."""
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


def py_to_r(value):
    """Convert a single Python value to the corresponding R object.

    - pandas.DataFrame -> R data.frame
    - bool / list-of-bool -> logical vector
    - int|float / list-of-number -> numeric vector
    - str / list-of-str -> character vector
    - list-of-lists (e.g. cost_list_of_vectors) -> R list of numeric vectors
    - None -> R NULL
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
    """Fetch (and cache) an exported LAGO R function object.

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
    """Convert kwargs, call the exported LAGO R function, return the R result."""
    rfunc = _get_rfunc(name)
    r_kwargs = build_r_kwargs(kwargs)
    return rfunc(**r_kwargs)


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
    if isinstance(out, pd.DataFrame):
        return out
    if isinstance(out, dict):
        # A named R list converts to {column_name: list}; the dict preserves
        # the R column order, so the DataFrame keeps value / <components> /
        # rec_int_cost / est_outcome_goal / status in order.
        return pd.DataFrame({k: v for k, v in out.items()})
    raise TypeError(
        "lago_sensitivity() result did not convert to a data.frame; got "
        "{}".format(type(out).__name__)
    )


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
