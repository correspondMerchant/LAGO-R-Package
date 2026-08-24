"""lago-python: a Python wrapper around the LAGO R package.

This package EMBEDS R via rpy2. It does not reimplement any LAGO math; every
function here calls the corresponding exported R function in the installed
``LAGO`` R package and converts inputs/outputs between Python-native types and
R objects.

Requirements at runtime: a working R installation, the ``LAGO`` R package
installed in R, plus rpy2 and pandas on the Python side. This is NOT an R-free
install; it gives Python users LAGO's API in Python syntax.

Exposed functions (mirroring the exported R functions):
    optimize()           -> lago_optimization()
    sensitivity()        -> lago_sensitivity()
    get_confidence_set() -> get_confidence_set()
    visualize_cost()     -> visualize_cost()   (opens a browser, BLOCKS)
    lago_report()        -> lago_report()
"""

from __future__ import annotations

from . import _bridge

__all__ = [
    "optimize",
    "sensitivity",
    "get_confidence_set",
    "visualize_cost",
    "lago_report",
]

__version__ = "0.1.0"


def optimize(
    data,
    outcome_name,
    outcome_type,
    intervention_components,
    intervention_lower_bounds,
    intervention_upper_bounds,
    outcome_goal=None,
    cost_list=None,
    quiet=True,
    **kwargs,
):
    """Run a LAGO optimization by calling R's ``lago_optimization()``.

    Parameters
    ----------
    data : pandas.DataFrame
        The input dataset. Converted to an R ``data.frame``.
    outcome_name : str
        Name of the outcome column.
    outcome_type : str
        ``"binary"`` or ``"continuous"``.
    intervention_components : list[str]
        Names of the intervention component columns.
    intervention_lower_bounds, intervention_upper_bounds : list[float]
        Per-component bounds.
    outcome_goal : float, optional
        The desired outcome level. At least one of ``outcome_goal`` or
        ``power_goal`` (via kwargs) must be given.
    cost_list : list[list[float]], optional
        The cost-function coefficients per component, as a Python list of
        lists. Maps to R's ``cost_list_of_vectors`` (a list of numeric
        vectors). Either this or ``unit_costs`` must be supplied.
    quiet : bool, default True
        Suppress R's paced console output. Defaults to True for programmatic
        use; the returned value is identical either way.
    **kwargs
        Any other argument of ``lago_optimization()`` is passed through
        verbatim (e.g. ``glm_family``, ``link``, ``center_characteristics``,
        ``center_characteristics_optimization_values``,
        ``confidence_set_grid_step_size``, ``power_goal``, ``unit_costs``,
        ``outcome_goal_intention``, ``include_confidence_set``, ...).

    Returns
    -------
    dict
        Result fields converted to Python-native types:
        ``rec_int`` (list of floats), ``est_outcome_goal`` (float),
        ``rec_int_cost`` (float), ``est_outcome_ci`` (dict lower/upper when
        present), ``confidence_set_size_percentage`` (float when present),
        ``cs`` (pandas.DataFrame when a non-empty confidence set exists, else
        None), plus echoed metadata. The fitted ``model`` is returned as the
        raw rpy2 glm object (it does not convert cleanly). The underlying R
        result object is kept under ``"_r_object"`` for use with
        :func:`lago_report`.
    """
    call_kwargs = dict(
        data=data,
        outcome_name=outcome_name,
        outcome_type=outcome_type,
        intervention_components=intervention_components,
        intervention_lower_bounds=intervention_lower_bounds,
        intervention_upper_bounds=intervention_upper_bounds,
        quiet=quiet,
    )
    if outcome_goal is not None:
        call_kwargs["outcome_goal"] = outcome_goal
    if cost_list is not None:
        call_kwargs["cost_list_of_vectors"] = cost_list
    call_kwargs.update(kwargs)

    result = _bridge.call_lago("lago_optimization", call_kwargs)
    return _bridge.lago_result_to_dict(result)


def sensitivity(
    data,
    outcome_name,
    outcome_type,
    intervention_components,
    intervention_lower_bounds,
    intervention_upper_bounds,
    parameter,
    values,
    outcome_goal=None,
    cost_list=None,
    quiet=True,
    **kwargs,
):
    """Run a LAGO sensitivity sweep by calling R's ``lago_sensitivity()``.

    This mirrors :func:`optimize`'s argument handling (same baseline
    optimization arguments, same ``cost_list`` -> ``cost_list_of_vectors``
    mapping, same ``**kwargs`` pass-through) and adds the two arguments that
    describe the sweep: ``parameter`` and ``values``. Each swept value triggers
    one full ``lago_optimization()`` run (with the confidence set forced off on
    the R side for speed), and the recommendation, its cost, and the estimated
    outcome are reported per run.

    Parameters
    ----------
    data : pandas.DataFrame
        The input dataset. Converted to an R ``data.frame``.
    outcome_name : str
        Name of the outcome column.
    outcome_type : str
        ``"binary"`` or ``"continuous"``.
    intervention_components : list[str]
        Names of the intervention component columns.
    intervention_lower_bounds, intervention_upper_bounds : list[float]
        Per-component bounds.
    parameter : str
        What to vary. Either the name of a scalar numeric argument of
        ``lago_optimization()`` that affects the recommendation (for example
        ``"outcome_goal"``, ``"power_goal"``, ``"shrinkage_threshold"``) or the
        special string ``"cost_multiplier"`` (each run multiplies every cost
        coefficient by one element of ``values``; requires ``cost_list`` and all
        ``values`` positive).
    values : list[float]
        A non-empty list of finite numbers. One run per element.
    outcome_goal : float, optional
        The desired outcome level, forwarded as the baseline for every run.
        When sweeping ``parameter="outcome_goal"`` it is overridden per run, so
        it need not be supplied in that case.
    cost_list : list[list[float]], optional
        The cost-function coefficients per component, as a Python list of
        lists. Maps to R's ``cost_list_of_vectors``. Required when
        ``parameter="cost_multiplier"``.
    quiet : bool, default True
        Suppress R's paced console output. Forwarded to each run.
    **kwargs
        Any other argument of ``lago_optimization()`` is passed through
        verbatim as a baseline argument (e.g. ``glm_family``, ``link``,
        ``center_characteristics``, ``power_goal``,
        ``outcome_goal_intention``, ...).

    Returns
    -------
    pandas.DataFrame
        One row per element of ``values``, matching R's ``lago_sensitivity()``
        ``data.frame``: a ``value`` column, one numeric column per intervention
        component (named by the component) holding its recommended value, a
        ``rec_int_cost`` column, an ``est_outcome_goal`` column, and a
        ``status`` column (``"ok"`` for a successful run, ``"error"``
        otherwise). Returning a ``pandas.DataFrame`` is consistent with how
        :func:`optimize` surfaces R ``data.frame`` fields (e.g. the confidence
        set ``cs``) via :func:`_bridge.r_to_py`.
    """
    call_kwargs = dict(
        data=data,
        outcome_name=outcome_name,
        outcome_type=outcome_type,
        intervention_components=intervention_components,
        intervention_lower_bounds=intervention_lower_bounds,
        intervention_upper_bounds=intervention_upper_bounds,
        quiet=quiet,
    )
    if outcome_goal is not None:
        call_kwargs["outcome_goal"] = outcome_goal
    if cost_list is not None:
        call_kwargs["cost_list_of_vectors"] = cost_list
    call_kwargs.update(kwargs)
    # parameter/values name the sweep. They match lago_sensitivity()'s named
    # formals (they follow `...` in the R signature but are supplied by exact
    # name, so R binds them to the formals rather than swallowing them into the
    # forwarded baseline `...`).
    call_kwargs["parameter"] = parameter
    call_kwargs["values"] = values

    result = _bridge.call_lago("lago_sensitivity", call_kwargs)
    return _bridge.r_sensitivity_to_df(result)


def get_confidence_set(
    predictors_data,
    intervention_components,
    outcome_data,
    fitted_model,
    link,
    outcome_goal,
    outcome_type,
    intervention_lower_bounds,
    intervention_upper_bounds,
    confidence_set_grid_step_size,
    cost_list=None,
    rec_int=None,
    **kwargs,
):
    """Compute a confidence set by calling R's ``get_confidence_set()``.

    Parameters
    ----------
    predictors_data : pandas.DataFrame
        The predictor columns the model was fitted on. Converted to an R
        ``data.frame``.
    intervention_components : list[str]
    outcome_data : list[float]
        The outcome vector.
    fitted_model : rpy2 object
        A fitted glm model, e.g. ``optimize(...)["model"]``.
    link : str
        ``"logit"`` or ``"identity"``.
    outcome_goal : float
    outcome_type : str
        ``"binary"`` or ``"continuous"``.
    intervention_lower_bounds, intervention_upper_bounds : list[float]
    confidence_set_grid_step_size : list[float]
    cost_list : list[list[float]]
        Cost coefficients per component (maps to ``cost_list_of_vectors``).
    rec_int : list[float]
        The recommended intervention, e.g. ``optimize(...)["rec_int"]``.
    **kwargs
        Other ``get_confidence_set()`` arguments (e.g.
        ``center_characteristics``,
        ``center_characteristics_optimization_values``,
        ``confidence_set_alpha``).

    Returns
    -------
    dict
        ``confidence_set_size_percentage`` (float),
        ``rec_int_ci`` (dict lower/upper, or None),
        ``cs`` (pandas.DataFrame, or None when empty).
    """
    call_kwargs = dict(
        predictors_data=predictors_data,
        intervention_components=intervention_components,
        outcome_data=outcome_data,
        fitted_model=fitted_model,
        link=link,
        outcome_goal=outcome_goal,
        outcome_type=outcome_type,
        intervention_lower_bounds=intervention_lower_bounds,
        intervention_upper_bounds=intervention_upper_bounds,
        confidence_set_grid_step_size=confidence_set_grid_step_size,
    )
    if cost_list is not None:
        call_kwargs["cost_list_of_vectors"] = cost_list
    if rec_int is not None:
        call_kwargs["rec_int"] = rec_int
    call_kwargs.update(kwargs)

    result = _bridge.call_lago("get_confidence_set", call_kwargs)
    out = _bridge.r_to_py(result)
    # r_to_py no longer scalar-collapses length-1 vectors, so unwrap the
    # genuinely-scalar fields (confidence_set_size_percentage) by name here.
    # rec_int_ci (named c(lower, upper)) stays a dict; cs stays a DataFrame.
    if isinstance(out, dict):
        out = _bridge.scalarize_dict(out)
    return out


def visualize_cost(
    component_names,
    unit_costs,
    default_cost_fxn_type,
    intervention_lower_bounds,
    intervention_upper_bounds,
):
    """Launch the R Shiny + D3 cost-function visualizer (R-side, unchanged).

    .. warning::
        This OPENS A BROWSER and BLOCKS until you close the app. The R Shiny
        app must be closed with its "Return list to R & close" button for the
        cost list to be returned; closing the browser tab returns nothing.

    Python's only role is to trigger the R app and receive the cost list it
    returns. When the app is closed via the button, R returns the current
    cost-function coefficient list; this function converts it to a Python
    list-of-lists (one list of floats per component) and returns it.

    Parameters
    ----------
    component_names : list[str]
    unit_costs : list[float]
    default_cost_fxn_type : str
        ``"linear"`` or ``"cubic"``.
    intervention_lower_bounds, intervention_upper_bounds : list[float]

    Returns
    -------
    list[list[float]]
        The cost-function coefficient list as it stood when the app closed,
        suitable to pass back as ``cost_list`` to :func:`optimize`. May be
        None if the app was closed without returning a list.
    """
    call_kwargs = dict(
        component_names=component_names,
        unit_costs=unit_costs,
        default_cost_fxn_type=default_cost_fxn_type,
        intervention_lower_bounds=intervention_lower_bounds,
        intervention_upper_bounds=intervention_upper_bounds,
    )
    result = _bridge.call_lago("visualize_cost", call_kwargs)
    return _bridge.r_to_py(result)


def lago_report(result, output_file=None, title=None, open=False, **kwargs):
    """Render an HTML report by calling R's ``lago_report()``.

    Parameters
    ----------
    result : dict | rpy2 object
        A result from :func:`optimize` (its ``"_r_object"`` is used), or a raw
        rpy2 "lago" object.
    output_file : str, optional
        Path to write the HTML report to. If None, R uses a temp file.
    title : str, optional
        Report title.
    open : bool, default False
        Whether R should open the report in a browser.
    **kwargs
        Passed through to ``rmarkdown::render()`` via ``lago_report()``.

    Returns
    -------
    str
        The path to the rendered HTML file.
    """
    if isinstance(result, dict):
        x = result.get("_r_object")
        if x is None:
            raise ValueError(
                "result dict has no '_r_object'; pass the dict returned by "
                "lago.optimize(...) or a raw rpy2 'lago' object."
            )
    else:
        x = result  # assume raw rpy2 "lago" object

    call_kwargs = {"x": x, "open": open}
    if output_file is not None:
        call_kwargs["output_file"] = output_file
    if title is not None:
        call_kwargs["title"] = title
    call_kwargs.update(kwargs)

    rendered = _bridge.call_lago("lago_report", call_kwargs)
    path = _bridge.r_to_py(rendered)
    # lago_report returns a length-1 character vector. r_to_py now yields a
    # list (it no longer scalar-collapses), so unwrap to the bare path string.
    if isinstance(path, list) and len(path) == 1:
        path = path[0]
    return path
