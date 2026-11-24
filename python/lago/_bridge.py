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
# printed to the R console. An error is caught here too and returned as its
# message, so the warnings R gave before it are not lost.
_CALL_WITH_WARNINGS = """function(f, args) {
  warned <- character(0)
  failed <- NULL
  value <- tryCatch(
    withCallingHandlers(do.call(f, args), warning = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    }),
    error = function(e) {
      failed <<- conditionMessage(e)
      NULL
    }
  )
  list(value = value, warnings = unique(warned), error = failed)
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
    "outcome_goal_intention",
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


def _date_text(value):
    """A date or a time as the text R gives for it, or None for NaT.

    A time at midnight becomes its date, such as "2020-01-01", and any other
    time also gets its time of day, such as "2020-01-01 10:30:00". A time with
    a fraction of a second keeps it, to the microsecond and without trailing
    zeros, such as "2020-01-01 10:30:00.5". That is how R writes the period
    levels of a Date or a date and time column, so the text names the same
    period.
    """
    import datetime

    import pandas as pd

    if value is pd.NaT:
        return None
    if isinstance(value, datetime.datetime):
        if value.time() == datetime.time(0):
            return value.strftime("%Y-%m-%d")
        text = value.strftime("%Y-%m-%d %H:%M:%S")
        if value.microsecond:
            text += ".{:06d}".format(value.microsecond).rstrip("0")
        return text
    return value.strftime("%Y-%m-%d")


def _plain(value):
    """Turn numpy and pandas values into plain Python values.

    A numpy scalar becomes its Python number, bool or str. A numpy array, a
    pandas Series, a pandas Index or a pandas array (such as the
    ``StringArray`` that ``Series.unique()`` gives, or an ``Int64`` array)
    becomes a list (a 2D array a list of lists). A missing value of pandas
    (``pd.NA`` or ``NaT``) becomes None, and a float NaN stays NaN. A
    categorical value becomes its text. A date or a time becomes text, as
    :func:`_date_text` writes it. The elements of a list or tuple are turned
    too. A ``Decimal`` or a ``Fraction`` becomes a float, and a ``range``
    a list. A pandas ``Period`` becomes its text label, such as "2020Q1".
    Anything else is returned unchanged. numpy comes with pandas,
    which the wrapper needs.
    """
    import datetime

    import numpy as np
    import pandas as pd

    import decimal
    import fractions

    if value is None or value is pd.NA or value is pd.NaT:
        return None
    if isinstance(value, np.datetime64):
        return _date_text(pd.Timestamp(value))
    if isinstance(value, np.generic):
        return value.item()
    if isinstance(value, datetime.date):
        return _date_text(value)
    if isinstance(value, pd.Period):
        return str(value)
    if isinstance(value, (decimal.Decimal, fractions.Fraction)):
        return float(value)
    if isinstance(value, range):
        return list(value)
    if isinstance(value, (pd.Series, pd.Index)):
        value = value.array
    if isinstance(value, pd.Categorical):
        return [None if pd.isna(el) else str(el) for el in value]
    if isinstance(value, np.ndarray):
        if value.dtype.kind == "M":
            return [_plain(el) for el in value]
        return [_plain(el) for el in value.tolist()]
    if isinstance(value, pd.api.extensions.ExtensionArray):
        return [_plain(el) for el in value]
    if isinstance(value, (list, tuple)):
        return [_plain(el) for el in value]
    return value


def py_to_r(value):
    """Convert a single Python value to the corresponding R object.

    - pandas.DataFrame -> R data.frame, with date, large whole number and
      text columns made ready first (see :func:`_frame_for_r`)
    - bool / list-of-bool -> logical vector
    - int|float / list-of-number -> numeric vector
    - str / list-of-str -> character vector
    - list-of-lists (e.g. cost_list_of_vectors) -> R list of vectors, each
      converted as a list
    - None -> R NULL
    - a missing value on its own (``pd.NA``, ``NaT``) -> R's NA
    - numpy scalars and arrays, pandas Series, Index and arrays, ``Decimal``,
      ``Fraction`` and ``range`` -> as the plain Python values they hold (see
      :func:`_plain`)
    - None in a list -> the R NA of the type of the other elements, and so
      is a float NaN in a list of text or of true and false
    - a pandas Series of numbers whose index is not a ``RangeIndex``, such
      as ``df[components].max()`` -> a named numeric vector, with the index
      labels as its names, so R matches it to the components (or centers)
      by name
    - a dict of numbers, such as ``{"gear": 5, "qsec": 20}`` -> a named
      numeric vector, with the keys as its names
    - an existing rpy2 object -> passed through unchanged

    Anything else, such as a set or a dict of text, raises a ``TypeError``
    that names its type.
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
            return ro.conversion.get_conversion().py2rpy(_frame_for_r(value))

    if isinstance(value, (set, frozenset)):
        raise TypeError(
            "Cannot convert a set to an R vector, as a set has no order. "
            "Give a list instead: {!r}".format(value)
        )

    named = _named_numbers(value)
    if named is not None:
        names, numbers = named
        vector = FloatVector([ro.NA_Real if v is None else float(v) for v in numbers])
        vector.names = StrVector(names)
        return vector

    value = _plain(value)

    # a missing value on its own, such as pd.NA or NaT, is R's NA
    if value is None:
        return BoolVector([ro.NA_Logical])
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
            # list-of-lists -> unnamed R list of vectors, each converted as
            # a list of its own, so a None in one becomes NA
            return ro.r["list"](*[py_to_r(el) for el in seq])
        # None, from a missing value, becomes the NA of the vector type. A
        # float NaN among text or true and false is missing too, as pandas
        # writes a missing text value as NaN.
        given = [el for el in seq if not _is_missing(el)]
        if seq and all(el is None for el in seq):
            return BoolVector([ro.NA_Logical] * len(seq))
        if given and all(isinstance(el, bool) for el in given):
            return BoolVector([el if isinstance(el, bool) else ro.NA_Logical for el in seq])
        if all(isinstance(el, (int, float)) and not isinstance(el, bool)
               for el in given):
            return FloatVector([ro.NA_Real if el is None else float(el) for el in seq])
        if all(isinstance(el, str) for el in given):
            return StrVector([el if isinstance(el, str) else ro.NA_Character for el in seq])
        kinds = sorted({type(el).__name__ for el in given})
        raise TypeError(
            "Cannot convert a list with elements of type {} to an R vector. "
            "Give a list of numbers, of text, or of true and false values: "
            "{!r}".format(" and ".join(kinds), seq)
        )

    # Already an rpy2 object (escape hatch) -> pass through unchanged.
    if type(value).__module__.split(".")[0] == "rpy2":
        return value
    raise TypeError(
        "Cannot convert a value of type {} to an R object. Give a number, "
        "text, true or false, a date, a list of them, a pandas DataFrame or "
        "None: {!r}".format(type(value).__name__, value)
    )


def _named_numbers(value):
    """The names and numbers of a named numeric value, or None for another value.

    A dict of numbers gives its keys and its values. A pandas Series of
    numbers gives its index labels and its values, unless its index is a
    ``RangeIndex``, the positions pandas gives a Series by default. A missing
    value is None. A dict that holds anything but numbers raises a
    ``TypeError`` that names the types it holds, and a Series that holds
    anything but numbers gives None, so it converts as a plain list.
    """
    import pandas as pd

    if isinstance(value, dict):
        names, values = list(value.keys()), [_plain(v) for v in value.values()]
    elif isinstance(value, pd.Series) and not isinstance(value.index, pd.RangeIndex):
        names, values = list(value.index), _plain(value)
    else:
        return None
    numbers = all(
        _is_missing(v) or (isinstance(v, (int, float)) and not isinstance(v, bool)) for v in values
    )
    if not numbers:
        if isinstance(value, dict):
            kinds = sorted({type(v).__name__ for v in values})
            raise TypeError(
                "Cannot convert a dict with values of type {} to an R object. A "
                "dict must hold numbers, one for each name: {!r}".format(" and ".join(kinds), value)
            )
        return None
    return [str(_plain(n)) for n in names], [None if _is_missing(v) else v for v in values]


def _is_missing(value) -> bool:
    """True for None or a float NaN, the missing values a plain list holds."""
    return value is None or (isinstance(value, float) and value != value)


def _frame_for_r(df):
    """A copy of a DataFrame with its columns in the forms rpy2 converts well.

    rpy2 writes Python dates in a column as day numbers counted from year 1,
    and a column of very large whole numbers as text. So a column of
    ``datetime.date`` or ``datetime`` values, or of numpy datetimes, becomes
    text, as :func:`_date_text` writes it, which is how a date names a period
    elsewhere in the wrapper. A whole number column with a value that does
    not fit R's integers becomes a float column, as R itself stores such
    numbers as doubles. A column of text with missing values, written as
    None, NaN or ``pd.NA``, becomes text with None where a value is missing.
    A column of true and false values with missing values, as ``read_csv``
    gives it, becomes a nullable boolean column, so R gets ``NA`` where a
    value is missing. A column of ``Decimal`` or ``Fraction`` numbers becomes
    a float column. A sparse column is made dense first. A column of time
    spans (``timedelta``) becomes a float column of the number of days, so 36
    hours is 1.5. A column of pandas ``Period`` values becomes the text label
    of each period, such as "2020Q1". The DataFrame given is not changed.
    """
    import datetime
    import decimal
    import fractions

    import numpy as np
    import pandas as pd

    def missing(v):
        return v is None or v is pd.NA or v is pd.NaT or (isinstance(v, (float, np.floating)) and v != v)

    out = df
    limit = 2**31 - 1  # R's NA integer is -2**31, so this is R's range
    for i in range(df.shape[1]):
        col = df.iloc[:, i]
        new = None
        if isinstance(col.dtype, pd.SparseDtype):
            # rpy2 cannot convert a sparse column, and falls back to text
            col = new = col.sparse.to_dense()
        if isinstance(col.dtype, pd.PeriodDtype):
            new = pd.Series([None if missing(v) else str(v) for v in col], index=col.index, dtype=object)
        elif pd.api.types.is_timedelta64_dtype(col.dtype):
            new = pd.Series(col / pd.Timedelta(days=1), index=col.index, dtype="float64")
        elif pd.api.types.is_datetime64_any_dtype(col.dtype):
            new = pd.Series([_date_text(v) for v in col], index=col.index, dtype=object)
        elif pd.api.types.is_integer_dtype(col.dtype) and not pd.api.types.is_bool_dtype(col.dtype):
            given = col.dropna()
            if len(given) and (given.max() > limit or given.min() < -limit):
                new = pd.Series(col.to_numpy(dtype="float64", na_value=np.nan), index=col.index)
        elif col.dtype == object:
            values = col.tolist()
            given = [v for v in values if not missing(v)]
            if given and all(isinstance(v, datetime.date) for v in given):
                new = pd.Series([None if missing(v) else _date_text(v) for v in values], index=col.index, dtype=object)
            elif given and all(isinstance(v, pd.Period) for v in given):
                new = pd.Series([None if missing(v) else str(v) for v in values], index=col.index, dtype=object)
            elif given and all(isinstance(v, datetime.timedelta) for v in given):
                days = [np.nan if missing(v) else pd.Timedelta(v) / pd.Timedelta(days=1) for v in values]
                new = pd.Series(days, index=col.index, dtype="float64")
            elif given and all(isinstance(v, (bool, np.bool_)) for v in given):
                # rpy2 reads a NaN among true and false as TRUE
                new = pd.Series(pd.array([None if missing(v) else bool(v) for v in values], dtype="boolean"), index=col.index)
            elif given and any(isinstance(v, (decimal.Decimal, fractions.Fraction)) for v in given) and all(
                isinstance(v, (int, float, decimal.Decimal, fractions.Fraction, np.number))
                and not isinstance(v, (bool, np.bool_))
                for v in given
            ):
                new = pd.Series([np.nan if missing(v) else float(v) for v in values], index=col.index, dtype="float64")
            elif given and len(given) < len(values) and all(isinstance(v, str) for v in given):
                new = pd.Series([None if missing(v) else v for v in values], index=col.index, dtype=object)
        if new is not None:
            if out is df:
                out = df.copy()
            out.isetitem(i, new)
    return out


def r_to_py(obj):
    """Convert an R object to a Python-native value.

    NULL -> None, data.frame -> pandas.DataFrame, named vector -> dict,
    UNNAMED vector -> Python list (ALWAYS, even length 1), R list ->
    dict/list (each element converted recursively, so a list of numeric
    vectors round-trips as a list of lists with its nesting intact).
    A vector or a list whose names are not a key for each element, because a
    name is empty, ``NA`` or used twice, becomes a list of ``(name, value)``
    pairs, so no element is lost. An empty name is ``""`` and an ``NA`` name
    is None there.
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
            frame = ro.conversion.get_conversion().rpy2py(obj)
        # pandas has no NA for a plain integer or bool column, so rpy2 hands
        # R's NA through as its sentinel, -2147483648 or True. Such columns
        # become nullable ones, with NA where R had NA. A text column gets
        # None where R had NA, instead of rpy2's NA_character_ object.
        is_na = ro.baseenv["is.na"]
        for name, col in zip(list(obj.names), list(obj)):
            if isinstance(col, FactorVector) or not isinstance(col, (IntVector, BoolVector, StrVector)):
                continue
            missing = [bool(m) for m in is_na(col)]
            if not any(missing):
                continue
            import pandas as pd

            values = [None if m else v for v, m in zip(frame[name].tolist(), missing)]
            if isinstance(col, StrVector):
                frame[name] = pd.Series(values, index=frame.index, dtype=object)
            else:
                dtype = "boolean" if isinstance(col, BoolVector) else "Int64"
                frame[name] = pd.array(values, dtype=dtype)
        return frame

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
        return _by_name(names, [r_to_py(el) for el in obj])

    if isinstance(obj, (FloatVector, IntVector, BoolVector, StrVector)):
        vals = list(obj)
        # R's NA in an integer, logical or text vector becomes None. A real
        # NA is already a float NaN.
        if not isinstance(obj, FloatVector):
            missing = list(ro.baseenv["is.na"](obj))
            vals = [None if m else v for v, m in zip(vals, missing)]
        names = obj.names
        if not _is_null(names):
            return _by_name(names, vals)
        # A plain (unnamed) vector ALWAYS becomes a Python list, even length
        # 1. Scalar unwrapping is a result-field concern, not a conversion
        # concern (see scalarize / SCALAR_RESULT_FIELDS).
        return vals

    # Fallback: hand back the raw rpy2 object (e.g. a fitted glm model).
    return obj


def _by_name(names, values):
    """The values of a named R vector or list, keyed by their names.

    This is a dict when every name is a key of its own. When a name is empty,
    ``NA`` or used twice, a dict would lose elements, so it is a list of
    ``(name, value)`` pairs instead, with None for an ``NA`` name.
    """
    is_na = _ensure()["ro"].baseenv["is.na"]
    keys = [None if m else str(n) for n, m in zip(list(names), is_na(names))]
    if None in keys or "" in keys or len(set(keys)) < len(keys):
        return list(zip(keys, values))
    return dict(zip(keys, values))


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
    :func:`warnings.catch_warnings`. When the R function fails, its warnings
    are issued first, and then an ``RRuntimeError`` with the R error message
    is raised.
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
    failed = out.rx2("error")
    if not _is_null(failed):
        # raised after the warnings, so a caller gets both
        from rpy2.rinterface_lib.embedded import RRuntimeError

        raise RRuntimeError(str(failed[0]))
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
    # a dict, or (name, message) pairs when a value was swept twice
    errors = list(errors.items()) if isinstance(errors, dict) else errors
    if isinstance(errors, list):
        # R names each error by its swept value, as as.character() writes it.
        # A text value, such as a period label or a date, matches by its
        # text, and a number by its value, as R writes a number with 15
        # significant digits.
        for value, message in errors:
            for i, v in enumerate(out["value"]):
                if isinstance(v, str):
                    if v == value:
                        messages[i] = message
                    continue
                try:
                    if abs(float(value) - float(v)) <= 1e-9 * max(1.0, abs(float(v))):
                        messages[i] = message
                except (TypeError, ValueError):
                    continue
    # object dtype keeps None for a run that did not fail, where pandas 3
    # would make a text column with NaN
    out["error_message"] = pd.Series(messages, index=out.index, dtype=object)
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
