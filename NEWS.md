# LAGOtrials (development version)

* `lago_optimization()` is much faster in two cases. The search for the best reachable outcome now stops each start after 100 iterations. Under OpenBLAS, one start could run to the default 4000 iterations, which made a run with wide bounds about 20 times slower than with the reference BLAS. An outcome goal within round off above the best reachable outcome now aims the optimizer at that outcome, which meets the goal within the round off, where every start used to run to its last iteration. `lago_budget()` meets such goals near the top of the reachable range, and on a continuous outcome it now extends its goal grid one goal at a time. Together, a budget search that affords the best reachable outcome on `mtcars` with wide bounds went from 228 to 15 seconds under OpenBLAS.

* In the cost designer of `visualize_cost()` and `lago_app()`, a coefficient whose slider range is very small or very large is now shown in a unit of its own size, and its label names the unit, for example "A^4 coefficient (shown in units of 1e-18)". Its range then shrinks to the size of the coefficient. On wide bounds such as 0 to 1000000, the sliders of the higher order coefficients can now bend the curve, where one step used to change the cost by far more than the whole curve. Ordinary sliders are unchanged.

* The MCP server checks tool arguments with public parts of the `mcp` Python package only. It used to replace private parts of `FastMCP`, which a new release of that package could break without warning. The behavior is unchanged: an unknown argument or an argument of the wrong type is a JSON error result, and a list sent as JSON text is still read.

* `lago_optimization()` warns when a value in `center_characteristics_optimization_values` is outside the range of that characteristic in the rows the model is fitted on. The recommendation is then for a center the model has not seen, so the estimated outcome and its confidence interval are an extrapolation. With few centers the interval can be so wide that it covers every probability from 0 to 1. The warning names each such characteristic, the value and the range. A true or false characteristic is read as 0 and 1, and a text or factor one as its 0 or 1 dummy. The check comes after every input error, so with `options(warn = 2)` it never hides one.

* `get_confidence_set()` called directly gives the same warning, checked against the rows its `fitted_model` was fitted on. Through `lago_optimization()` the warning still comes once.

* `lago_optimization()` refuses a value of `center_characteristics_optimization_values` that is `NA`, `NaN`, `Inf` or `-Inf`. Such a value names no center, and it used to give a confident estimate or a misleading error about the optimizer.

* `lago_app()` can save every setting to a file with Save settings, and put them back on the same data with Load settings. Loading on data that lacks a column the settings name says which columns are missing. `lago_app(result)` with a `lago` result opens on the data and the settings of its call, so one setting can be changed and the run repeated without typing the setup again. Settings the app cannot show, such as an interaction of three components, are listed when it opens.

* `lago_app()` takes center level data, with one row per center and period. The outcome comes from a column of successes or of proportions and a column of totals, and the app builds the `proportion` and `center_sample_size` columns the package reads. The app now offers the bundled PULESA data `main_pulesa_data` as an example, set up as in the package's PULESA model with clinic and period effects. For the weighted average center, each center's weight can now be set, starting at its share of the patients, with a check that the weights add up to 1.

* `lago_app()` has advanced settings, each in a section that stays closed until it is needed. The outcome model can take a family for a continuous outcome, a link, and weights from a column. The intervention step can set the confidence level and the grid steps of the confidence set, switch the optimizer to a grid search with its own steps, and give the recommendation of the previous stage for the shrinking method, with a button that fills in the current recommendation. Each setting is checked before a run, and the R code includes it. The confidence interval in the result now shows the level it was computed at, which was always given as 95%.

* `lago_report()` now passes `quiet` and `envir` on to `rmarkdown::render()`, where they used to fail with "formal argument matched by multiple actual arguments", and says so when given `input`, `output_dir` or `params`, which it sets itself. An existing folder as `output_file` gets the report inside it, as `lago_report.html`.

* `lago_report()` now renders a copy of its template in a temporary folder. It used to write its working files next to the template in the installed package, so it failed with "Permission denied" when the R library could not be written to, as with a library installed for all users.

* The help pages now render their formatting. Markdown was never turned on for the package, so a link such as [lago_optimization()] and the code marks around an argument name were printed as typed. Links now work, code is set in code font, and the lists of cost functions and of optimization methods are real lists. The text itself is unchanged, apart from three places where a sentence that followed a list ran into its last item.

* `lago_app()` now opens the cost designer of `visualize_cost()` in a dialog, from the "Shape the costs visually" button in the intervention step. The designer starts on the costs in use, draws the total and marginal cost of each component, and lets you shape them with sliders or by dragging the curve. "Use these costs" puts them in as cost coefficients and keeps the bounds. Costs that do not fit the designer's linear or cubic form are named in the dialog, since applying replaces them.

* `lago_app()` has two new tabs that start from the result of a run. Sensitivity runs `lago_sensitivity()` over even steps or typed values of the outcome goal, a multiplier of all costs, the power goal settings or the period, and shows a plot, a table and a CSV download. Budget runs `lago_budget()` for a budget that starts at the cost of the recommendation, explains whether the budget binds, plots the goals searched, and can put the best outcome in as the outcome goal. The Results tab can download the HTML report of `lago_report()`. The R code tab and the value returned by Done include the sweep and the budget search.

* Fixed the sliders of `visualize_cost()` moving a coefficient when they only got the keyboard focus. A slider now keeps its value when you tab through it, and the arrow keys move it by one step.

* `lago_optimization()` matches named `center_weights_for_outcome_goal` to the centers by name. It used to ignore the names and match the weights by position, which silently gave the wrong average center when the names were in another order. Names that are not the centers are refused.

* `time_effect_optimization_value` can be the label of a text, factor or Date period, such as `"2024-Q1"`. It used to have to be a number, so such a period could never be chosen. A number such as 4 also finds the period labeled `"04"` or `"4.0"`.

* `lago_optimization()` refuses weights that are negative, infinite or all zero, a `center_sample_size` column that is not numbers or has a negative value, and a binary outcome or center level proportion outside 0 to 1, each with a message that names the problem. They used to fail with unclear errors from `glm()`. `include_time_effects = NA` is refused clearly too.

* The check for a center characteristic value outside the data now reads a Date characteristic as its number, as the model does, and for center level data it uses the rows with a positive `center_sample_size`.

* With a power goal, a missing value in the `group` column is now named in the error. Without one, rows with no arm are left out of the overall test, and the test is not shown, with a warning, when an arm has fewer than two rows.

* The power goal search no longer fails for a power goal at or below 0.05 or very close to 1. With a large next stage it used to round the power-implied outcome goal up to the next of 1000 grid points, so the recommendation could cost much more than needed. It now finds that goal between the grid points.

* `lago_optimization()` checks the power inputs whatever the `icc`. `power_goal_cluster_id` must name a column whenever it is given, and `power_goal`, `num_centers_in_next_stage`, `patients_per_center_in_next_stage`, `icc` and `shrinkage_threshold` must each be one finite number, with whole numbers for the next stage. The error for an unknown `power_goal_approach` no longer repeats itself.

* `get_confidence_set()` called directly checks `rec_int`, `confidence_set_alpha` and `outcome_goal`. A wrong `rec_int` used to give the interval of another point with no error.

* An outcome goal within round off of the best reachable outcome now counts as reached. It used to fall back to the shrinking method.

* The note that a `cost_list_of_vectors` overrides `default_cost_fxn_type` is shown only when `default_cost_fxn_type` was set. The warnings that a bound is inside the range of the data are now shown once in a sweep or a budget search, not once per run.

* `lago_budget()` now finds the cheap interventions below its goal grid for a continuous outcome. Before, a budget between the cost of the cheapest intervention and the cost of the lowest goal on the grid was reported as infeasible, and the warning gave the wrong cheapest cost.

* `lago_budget()` now halves the gap at the affordability boundary until it is narrow, after its refinement sweep. Before, a small `n_grid` could return an outcome well below the best affordable one, and could call it the maximum reachable outcome.

* `lago_sensitivity(parameter = "cost_multiplier")` now works for a result fitted with `unit_costs` by scaling `unit_costs`. When neither `cost_list_of_vectors` nor `unit_costs` is available, the error now names both.

* `print()` of a `lago_sensitivity()` result with one successful run now names that run once, instead of "X at v and X at v".

* `print()` of a `lago_budget()` result now rounds the recommended intervention, its cost and the estimated outcome as `print()` of a `lago_optimization()` result does.

* `lago_report()` now shows any `title` literally. A title with a double quote no longer fails with a YAML parser error, backslashes are kept, and markdown characters such as `*` are not turned into formatting.

* The hover read-outs in the `lago_report()` plots now round numbers the way the report text does. Costs show 5 significant figures and component values show 4 decimals, without e notation for ordinary numbers (for example `cost: 23928` instead of `cost: 2.393e+4`).

* The `lago_report()` confidence set section is now always shown. When the set is empty, was not requested, or was not computed because the shrinking method was used, the report gives the same reason as `print()`. The `lago_report()` help page now describes the report sections accurately, and says that the plot is drawn for one or two components only.

* The inputs recap of `print()`, `summary()` and `lago_report()` now shows the cost coefficients of each component rounded to 5 significant figures and labeled by component, instead of the R source of the cost list with every digit.

* A p-value of the overall intervention-effect test that is too small to represent now shows as `< 2.2e-16` instead of `0`, in the console and in the report.

* For a continuous outcome, `print()` now says that the overall intervention-effect test is only available for binary outcomes, instead of asking for a `group` column the data may already have.

* The confidence level label is now short for any `confidence_set_alpha`. For example `confidence_set_alpha = 1/3` gives `66.67%` instead of `66.6666666667%`.

* `lago_app()` reads an uploaded CSV that is not UTF-8 as Latin-1 and says so, instead of silently stopping at the first accented letter and losing the rows after it.

* `lago_app()` refuses a period to optimize for that has no row the model can use, and by default picks the latest period that has one.

* `lago_app()` refuses an outcome, component, covariate, center characteristic or weights column named like a column the app builds for the package (proportion, center_sample_size, group, center or period), and asks to rename it or choose another one. Before, such a choice gave a confusing error or, for a covariate named center, a wrong run.

* `lago_app()` refuses a continuous outcome with values of exactly 0 or 1 under the logit link with the normal model, and points to the proportion model, instead of failing with a raw `glm()` error.

* `lago_app()` names columns that are not valid R names, such as "launch duration" in data passed to it, before the run, and shows how to rename them with `make.names()`.

* When `lago_app()` cannot fit a binary outcome with the identity link, it now says to choose the logit link, along with the error of the package.

* In the cost designer (`visualize_cost()` and the designer in `lago_app()`), the copied list, the list returned to R and the costs applied to the app now always match the drawn curve. Before, a drag that moved a coefficient by less than half a slider step left them on the old coefficients.

* The cost designer's warning that the marginal cost should be positive now checks the marginal cost itself. Before, it checked the total cost. A separate warning now shows when the total cost goes below 0.

* The cost designer no longer reports a valid cost function as decreasing when narrow bounds sit far from 0, for example on [1000, 1001]. Its checks now allow for the round-off of evaluating the polynomial.

* Dragging a cubic cost curve now works when the bounds are very narrow, for example on [0, 0.001]. Before, every such drag was ignored.

* The cost designer's sliders now scale with a small unit cost. For a unit cost of 1e-6 a slope slider now runs over about 1e-4 in steps of 1e-6, instead of over 1 in steps of 1e-5.

* `visualize_cost()` now refuses bounds that are not finite and unit costs below 0 with a clear error. Before, an infinite bound stopped the app with an error.

* The unit cost label on the marginal cost chart now keeps significant digits, so a unit cost of 0.001 no longer reads as 0.00.

* Fixed the MCP server stopping the call that was running when a client canceled a different call that was still waiting for its turn. The running call failed with "the R worker stopped during the call". A canceled call that is still waiting now just never runs.

* The MCP server now stops its R worker when it gets `SIGTERM`, and the worker exits within a second or two when the server is killed in any other way. A worker in the middle of a long call used to keep running on its own until the call ended.

* The MCP server's `sensitivity` and `budget` tools, and the confidence set of `optimize`, now give every number at full precision. They were rounded to 10 decimal places, so a cost in small units could come back as 0 or differ from what `optimize` gives.

* The Python wrapper now accepts `numpy` arrays and numbers, pandas Series and pandas Index values wherever it takes a list or a number, for example `intervention_components=df.columns[1:3]` or bounds given as a `numpy` array. They used to fail with an unclear conversion error.

* The MCP server now marks a failed tool call as an error result (`isError` true), with the JSON body `{"error": "...", "warnings": [...]}` as its text. It used to report failures as successful results.

* Every MCP tool result now has a `warnings` list, as the documentation says. The results of `describe_data`, and errors found before R is called, used to have none.

* The Python wrapper turns an R `NA` in an integer, logical or text result into a missing value. It used to come through as -2147483648, or as `True` for a logical column.

* The `main_pulesa_data` and `new_pulesa_data` help pages now describe the `MI_*` columns correctly. They are 0/1 indicators for values that were missing and filled with a constant, not interaction terms. The `new_pulesa_data` page also notes that `DeliveryB`, `RemoteMonitoring` and `PerformanceImprovement` contain `NA`.

* The `BB_data` help page now describes `center_type`, `pp2_hand_hygiene` and `pp3_oxytocin_mother` correctly and lists the observed `launch_duration` values. The vignette now gives the correct unit costs (1700 and 8000).

* The basic `lago_optimization()` example now uses `mtcars` and gives a recommendation with a confidence set. The README BetterBirth output now shows the warnings that a real run gives.

* The numerical optimizer now gives the same recommendation whatever the units of a continuous outcome. Its constraint is now on the scale of the goal, as its objective is on the scale of the cost. Before, an outcome in very large or very small units could stop it at a costlier point.

* With a power goal on the identity link, a control rate below 0 is now held at 0. Before, the conditional approach could return a goal set by where its variance stopped being defined. The unconditional power search also sums the stage 1 data once rather than at every grid point, so it is faster on large data.

* A clustered power goal (`icc` above 0) on individual level data no longer fails with the message that an arm has fewer than two centers. On center level data, a power goal now needs patients in both arms, and the center count for `icc` leaves out centers with no `center_sample_size`.

* `lago_optimization()` refuses `prev_recommended_interventions` with `NA` clearly, and its out of bounds message names the argument correctly. It refuses an `outcome_goal`, `confidence_set_alpha` or `include_confidence_set` that is `NA` or has more than one value.

* All zero weights, or all zero `center_sample_size`, are now refused with a clear message also without center or time effects.

* Named `center_weights_for_outcome_goal` can now name an explicit `NA` center (from `addNA()`) as `"NA"` or `NA`. A date-time `time_effect_optimization_value` is matched by its label.

* A rerun of a result in `lago_sensitivity()` or `lago_budget()` no longer shows the note that a cost list overrides `default_cost_fxn_type` when it was not set.

* `get_confidence_set()` called directly matches a named `rec_int` to the components by name, refuses names that are not the components, and checks the length of `cost_list_of_vectors`, the bounds and the grid steps. An error inside it, or inside the input checks of `lago_optimization()`, no longer carries the whole data set in its call.

* The help pages now say that named center weights are matched by name and that a row with an `NA` weight is left out of the fit, and the package description no longer says that `lago_optimization()` uses cluster robust variance for clustered designs. The README lists all four MCP tools.

* `lago_budget()` no longer says the budget did not bind, or that it found the maximum reachable outcome, when that maximum costs more than the budget. The search now also narrows the cost at the boundary, so the recommendation uses the budget more fully. The printed cost is rounded down when rounding would show it above the budget.

* A sweep with `lago_sensitivity()` or `lago_budget()` shows each distinct warning from the shared model fit once, not once for every run.

* `lago_sensitivity()` refuses to sweep `time_effect_optimization_value` without `include_time_effects = TRUE`, and `icc` without a `power_goal`, since those sweeps would return identical rows.

* `lago_sensitivity()` can sweep period labels or dates for `time_effect_optimization_value`, and `plot()` shows them on a discrete axis.

* The help for `lago_sensitivity()` documents the `effective_outcome_goal` and `shrinking_method_used` attributes.

* `lago_report()` now shows every character of the title as typed, in both the heading and the page title. Parentheses and square brackets no longer turn into math, and markdown links, dollar signs, quotes, backslashes and HTML tags are not interpreted.

* The printed and reported results now write numbers in full, such as 100000 and 0.00001 instead of 1e+05 and 1e-05. The text now matches the hover read-outs of the report.

* A confidence set that holds a very small share of the grid now shows as "< 0.01% of the grid" instead of "0% of the grid".

* The outcome goal is now rounded like the estimated outcome, so a goal of 70/3 shows as 23.3333.

* The inputs recap now says whether the outcome goal is to be maximized or minimized, and `lago_optimization()` stores `outcome_goal_intention` in its result.

* `lago_report()` now refuses an `output_file` that does not end in `.html` or `.htm`, so it can no longer fail in pandoc or overwrite a file such as an `.Rmd`. An empty or missing `title` now uses the default title.

* `lago_app()` now reads an uploaded CSV that is not UTF-8 as Windows-1252, even when a text cell starts with an accented letter or a curly quote. Before, such a file failed with an error that wrongly blamed the separator or decimal mark.

* `lago_app()` now reads every row of a UTF-8 CSV in the C locale. The R code it shows for an upload now reads the file the same way in any locale.

* Column names with accents in an uploaded CSV, such as one with an accented first letter, now keep their letters in any locale.

* `lago_app()` now tells you to save a UTF-16 CSV as UTF-8, instead of failing with a hint about the separator and decimal mark.

* When `lago_app()` starts from a result whose center weights have names, each center now keeps its own weight.

* `lago_app()` now refuses a cluster column named like a column the app builds, such as center, which it would otherwise replace without a word.

* The `lago_app()` check that the logit link with the normal model has no outcome of exactly 0 or 1 now only looks at the rows the model fits.

* The cost designer now warns when a total cost falls across its bounds, even for a cubic on narrow bounds far from 0 or for a very small unit cost. Before, a fall made of many small steps could pass the check.

* The warning below the coefficients in the cost designer is about negative marginal costs, and it now shows when the marginal cost check fails.

* Dragging the cost curve past a slider's range now widens the range in proportion to the new value, so a small unit cost keeps a fine slider.

* The default slider range of a cost coefficient now grows with the bounds, so a high order coefficient can bend the curve on bounds near 0. The slider step grows with the range and the value, so the arrow keys work for very large coefficients.

* The cost summary in the cost designer shows small costs and narrow bounds with significant digits instead of rounding them to 0.

* `visualize_cost()` now gives a clear error for a `default_cost_fxn_type` of more than one value, for zero components, and for missing or empty component names.

* When an R function fails, the Python wrapper now issues the warnings R gave before the error and then raises it. The MCP server's error result now lists those warnings too.

* The `budget` tool of the MCP server no longer describes arguments it does not take. Every tool now refuses an argument it does not take, instead of ignoring it.

* An argument of the wrong type, a missing argument, or an unknown argument to an MCP tool now gives the same JSON error result as every other error, instead of plain text.

* The Python wrapper now takes pandas arrays (such as the result of `Series.unique()` or an `Int64` array), categorical values, and dates as arguments. A date becomes the text R gives for it, so it can name a period.

* A missing text value in a data frame from R is now `None` in Python. A missing value in a list argument now reaches R as `NA` of the type of the other elements.

* The MCP docs now list every column of a `sensitivity` run and say that `effective_outcome_goal` of `optimize` comes only with a power goal. They also say that a NaN or Infinity in JSON reaches a tool as null, which means "not given" for an optional argument.

* `lago_optimization()` caps every start of its cost search. A start that landed on a saddle point of an interaction model, which cannot reach the goal, used to run 4000 iterations, so a run could take 30 to 40 seconds. Such a run now takes about 2 seconds, with the same recommendation.

* With interaction terms, the shrinking method now holds the other components at the corner of their bounds with the highest outcome, and reads each component's slope there, its own coefficient plus its interaction coefficients times the other components. A goal just out of reach used to send the recommendation back to the stage 1 intervention. It now stays near the best corner. Without interaction terms the result is unchanged.

* The confidence set of a continuous outcome no longer fails with "system is computationally singular" when a component sits far from 0, such as values near 100000. The variance is now computed from a QR decomposition, as `glm()` fits the model.

* The default cubic cost now falls back to the linear cost, with a warning, on bounds so narrow and far from 0 that round off would swamp the cubic. Its total used to rise and fall at random there, and the recommended cost could be below the cost at either bound.

* A grid search is now about twice as fast.

* `get_confidence_set()` called directly refuses center weights whose length is not the number of centers of `fitted_model`, with a message that says so, and reads a number column for the center or the period as the model's factor, as `lago_optimization()` does.

* An explicit `NA` period level, made with `addNA()`, can now be chosen with `time_effect_optimization_value = NA_character_`.

* The warning about very large standard errors, a sign of separation, is now given for binary and proportion models only. For a normal model it depended on the outcome's units.

* A center whose rows all have weight 0 is now warned about once.

* The package now declares that it needs R 4.0.0 or later, which `lago_app()` already needed.

* A warning from the input checks of `lago_optimization()` is now headed "In validate_inputs(...)" instead of by the whole function.

* `time_effect_optimization_value = NA_character_` now selects an explicit NA period level made with `addNA()`.

* Named bounds, unit costs, unit cost doses, cost lists, step sizes, previous recommended interventions and center characteristic values are now matched to the components by name. Names that are not exactly those components, each once, are refused. Unnamed vectors are matched by position as before.

* A named `icc` pair is now matched to the arms by name, as `c(control = , treatment = )`.

* An intervention component, main component, center characteristic or additional covariate given twice is now refused. So is a column used in two roles, such as the outcome as a covariate or a characteristic that is also a component. A characteristic that was also a component used to be counted twice, which gave a recommendation that missed the goal.

* `lago_optimization()` now refuses an interaction column that is not the product of its components on the rows the model uses. The documentation now says that a column named like "a:b" holding the product must exist, and the error for a missing one shows how to create it. Main components must be numeric columns of the data.

* Bounds that are NA, NaN or infinite are now refused clearly, and a one dimensional array, as from `tapply()`, is accepted as a vector.

* Data with no row where every model variable and its weight are observed is now refused clearly before the fit.

* Inf or -Inf in the outcome, a component, a characteristic or a covariate is now refused clearly.

* A quasibinomial outcome must be from 0 to 1, and a continuous outcome with a logit link strictly between 0 and 1. Other values are refused clearly.

* A text, factor or logical covariate or center characteristic with only one value on the usable rows is now refused clearly.

* String arguments such as `outcome_name`, `link` and `default_cost_fxn_type` must be one string, and `quiet` and `include_interaction_terms` must be TRUE or FALSE. Other values are refused with a clear message.

* Column names that are reserved in R, such as `..1`, and data with two columns of the same name used by the model are now refused.

* Values for effects that are turned off are now ignored with a warning: `center_effects_optimization_values` without center effects, `time_effect_optimization_value` without time effects, and `center_characteristics_optimization_values` without center characteristics. `unit_cost_doses` are also ignored with a warning when a cost list or a linear cost is used.

* Empty cost vectors and NA or infinite cost coefficients in `cost_list_of_vectors` are now refused.

* A date-time `time_effect_optimization_value` in another time zone now names the same period.

* The warnings that a bound lies inside the data now use only the rows the model is fitted on. The warning that the goal is at or past the observed mean now also fires for a minimize goal equal to the mean.

* The grid search note about more than 3 components counts the main components rather than interaction terms. The message for a wrong length of `prev_recommended_interventions` names the main components.

* `lago_sensitivity()` now refuses a sweep of `shrinkage_threshold`, which `lago_optimization()` does not use. It also refuses sweeps of `num_centers_in_next_stage` and `patients_per_center_in_next_stage` without a `power_goal`. These sweeps used to return identical rows.

* `lago_sensitivity()` and `lago_budget()` now refuse an unnamed argument in `...` when `object` is given. Before, such an argument was silently dropped.

* `lago_sensitivity()` and `lago_budget()` now explain that the data must be named as `data =` when a data frame is given as the first argument.

* `lago_sensitivity()` now accepts factor and date-time (`POSIXct`) values for a `time_effect_optimization_value` sweep and matches them by label, as `lago_optimization()` does.

* The printed summary of a `lago_sensitivity()` result now keeps the sweep order for period labels and dates, as the plot does. It also writes numbers without e notation.

* `plot()` of a `lago_sensitivity()` result no longer draws a line, or shows a ggplot2 note, for a single point. The components plot now counts the rows it drops, not the points.

* The documentation of the `effective_outcome_goal` attribute of a `lago_sensitivity()` result now says it is `NA` for a run without a power goal.

* `lago_budget()` now refuses an `n_grid` that is a fraction or text, which it used to cut down to a whole number.

* `lago_budget()` now decides whether the budget binds from the cost and intervention of the better goals it reached. A large outcome no longer reports a binding budget because of round off. A flat outcome near its maximum now reports a binding budget when a costlier intervention exists.

* The console output and the report text now round a number in the same way as the hover read-outs of the report. A cut digit of 5 rounds up, so for example 0.10005 shows as 0.1001 everywhere.

* Large numbers no longer show made up digits at the end, such as 10000000000000002 for 1e16.

* The estimated outcome is now rounded like the bounds of its confidence interval, so it never shows outside the printed interval.

* P-values are written in full, such as 0.00001 instead of 1e-05. A missing p-value or test statistic now shows as "not available".

* A confidence set of 0.005% of the grid now shows as 0.01% and not 0%, and a set just short of the whole grid shows as "> 99.99%" and not 100%.

* When the data has a 'group' column but the overall test could not be run, `print()` and `summary()` now give the reason, instead of asking for a 'group' column.

* The inputs recap now shows an interaction component such as x1:x2 without backticks.

* `print()` and `lago_report()` no longer fail on a result whose `outcome_goal` field was removed.

* The confidence label of a `confidence_set_alpha` close to 1, such as 0.99999, now shows as 0.001% and not 0%.

* A very large or very small cost coefficient now shows in e notation, such as 1e+25, and no longer with made up digits.

* `lago_report()` now refuses `output_format`, `output_options`, `output_yaml`, `run_pandoc` and `runtime`, because the report is always one self-contained HTML file. It also refuses unnamed extra arguments and a `quiet` that is not TRUE or FALSE.

* A report title can now hold any control character, such as a tab, an escape or NEL. Before, most of them stopped the render with a YAML error, and NEL showed as a space.

* `lago_report()` now says when the output folder does not exist or is a file, and its message about the file extension names both .html and .htm.

* `lago_app()` now gives an uploaded file the same column names as `read.csv()`, so the shown code reads the same column when two names clash.

* `lago_app()` turns uploaded column names that are not ASCII, such as names with accents, into ASCII names and lists them in step 1, so the model and the shown code work in any locale.

* `lago_app()` now says when an uploaded file uses separators between the thousands, a decimal point while Comma is chosen, or a dot for a missing value.

* `lago_app()` now gives a clear message for an empty file, a file with only a header row, and UTF-16 text without a byte order mark.

* In `lago_app()`, "Use this outcome as the goal" now rounds the goal toward the outcome the budget reaches, so the new run reaches it.

* In `lago_app()`, the budget now starts at the cost of the recommendation rounded up, so it always buys the recommendation.

* In `lago_app()`, Done no longer returns a sweep or a budget search after a later run of that tab failed.

* In `lago_app()`, switching to an upload with no file, or one that cannot be read, now clears the last result.

* Load settings in `lago_app()` now keeps the cost coefficients. They used to be lost, so a loaded cost list became a cost of 1 per unit.

* `lago_app()` now refuses a center, period, treatment arm or total column whose name is one the app builds before it reads that column.

* `lago_app()` started from a result now uses the grid and confidence set steps the package used for that result, so a new run gives the same result.

* `lago_app()` now checks that the next stage sizes are whole numbers, that time effects have at least two usable periods, and that each arm has an observed outcome for a power goal.

* `lago_app()` started from a result or from settings now keeps the center weights exactly.

* `lago_app()` now offers the ICC center column for data with one row per center, and counts only centers with patients for center level data.

* `lago_app()` now accepts any power goal above 0 and below 1, the same as `lago_optimization()`.

* The cost designer's linear sliders work again on narrow bounds far from 0, such as [1000, 1001]. Each cost form now has its own slider units, and switching forms keeps the readings in the right units.

* The cost designer's slider ranges now follow how much each term changes the cost over the bounds, so one step of a cubic slider no longer changes the cost by far more than the whole curve spans.

* The cost designer now warns when a curve is not accurate on its bounds, as happens for the cubic form on narrow bounds far from 0, and suggests the linear form. A curve that misses its dragged points is drawn in red instead of looking valid.

* The cost designer's tooltips now round numbers like the report does, so ordinary costs no longer show in e notation.

* A cost designer link with more than 10 components now shows a note saying why it opened the example.

* On bounds below 0, the cost designer's starting cubic curve now begins at a cost of 0 instead of a negative cost.

* In Python, a DataFrame column of `datetime.date` or `datetime` values, or of numpy datetimes, now reaches R as the text of each date, so it names its periods. Before, Python dates became day numbers.

* In Python, a whole number column with a value too large for R's integers now reaches R as numbers instead of text.

* In Python, a NaN among text or true and false values, such as a missing value from pandas, is now R's `NA` instead of an error.

* In Python, a None inside a nested list now becomes `NA`. A missing value on its own (`pd.NA`, `NaT`) is R's `NA`, and `range`, `Decimal` and `Fraction` values convert to numbers. A set, a dict or another value that cannot be converted gives a clear error that names its type.

* `lago.sensitivity()` now keeps the error message of each failed run when it sweeps period labels or dates.

* `lago.optimize()` now returns `outcome_goal_intention` as a string instead of a list of one string.

* `lago.get_confidence_set()` now says when `cost_list` or `rec_int` is missing, and its docs say that both are required.

* The docs of `lago.sensitivity()` and of the MCP `sensitivity` tool now say that a cost multiplier works with `unit_costs` as well as with a cost list. The Python docs say that the `object` argument of `lago_sensitivity()` and `lago_budget()` is not supported.

* The MCP tools now take a period label, such as "2024-Q2", for `time_effect_optimization_value`, and a number for `center_effects_optimization_values`.

* The MCP `optimize` tool now reports `confidence_level` without float noise, so an alpha of 0.07 gives 0.93.

* The MCP tools now name the weights column when it holds a value that is not a number.

* The MCP server now answers a call to a tool it does not have with its usual JSON error result. The hint about NaN and Infinity in its null errors now says it applies only if the client sent one.

# LAGOtrials 1.2.0

* New `lago_app()` runs the whole LAGO optimization in a point and click Shiny app on your own computer, with no R code to write (#37). It covers the data (your own data frame, the bundled examples or a CSV file), the outcome and goal with the power goal, the intervention components with their bounds and costs, interaction terms, center characteristics, fixed center and time effects, and additional covariates. Each step says what is missing before a run, every setting has a short explanation, and the result shows the recommendation, the confidence set and the package's warnings. The app shows the R code that reproduces the run, and returns the last result to R when you click Done. It is reachable only from your own computer.

* `visualize_cost()` now also serves its app only to your own computer, even when the `shiny.host` option is set to another address.

* Fixed the numerical optimizer trusting a solver result outside the intervention bounds. `NlcOptim::solnl()` can return such a point, far outside the bounds with some BLAS libraries, as on Ubuntu 26.04 with OpenBLAS 0.3.32. The highest outcome search then took an outcome only reachable outside the bounds, so an unreachable goal looked reachable and the recommendation was the upper bounds instead of the shrinking method. The cost search let such a point set the lowest cost, so cheaper points inside the bounds were left out and a costlier recommendation was returned. Every solver result is now moved onto the bounds before its outcome, its cost and the goal are checked, so results no longer depend on the platform.

# LAGOtrials 1.1.1

* Fixed the shrinking method depending on the outcome's units when the outcome is small, such as a rare binary outcome or a continuous outcome in large units. Its search stopped as soon as the outcome came within an absolute 1e-6 of the goal. It now stops within 1e-6 relative to the goal below 1.

* The default cubic cost now handles wide bounds, such as 0 to 5000, which failed with a singular system. A badly conditioned solve is redone in each component's share of its range. Narrow bounds, such as 1 to 1.00001 or 10 to 10.0001, could give a cost curve whose average unit cost was far off, by 27% and 15% in these examples, and the default cubic is now checked against an accurate solve and made flatter until it matches. A curve the plain solve gave within 0.1% is unchanged.

* Fixed the numerical optimizer refusing a reachable goal when all its diagonal restarts failed, before it tried the other starts and the point of the highest outcome.

* An unreachable power goal now always uses the shrinking method, and shrinks to the stage-1 intervention on both links. On the identity link the model can predict the goal of 1 it is set to, so it used to optimize to that goal, and `lago_budget()` counted it as reached. `lago_budget()` now never counts a shrunk run as reached. The power goal warning now says the recommendation is shrunk, to the stage-1 intervention or to `prev_recommended_interventions` when given, and the run no longer also warns that the maximum achievable outcome is below the goal.

* Fixed a binary `lago_budget()` for a rare outcome when maximizing, or for an outcome near 1 when minimizing. Its goal grid starts at 0.02 and ends at 0.98, so a budget that only affords goals beyond it was reported as infeasible. The grid now also fills that gap.

* `lago_budget()` now treats estimated outcomes within 1e-6 of each other as tied. Goals below the no intervention outcome all give the same intervention, and on some platforms the solver noise between them was above the old tolerance, so it could refine a gap far below the best affordable goal and leave budget unspent.

* Fixed `visualize_cost()` replacing an exact cubic coefficient with the slider's rounded value after the slider's range was narrowed.

* `print()`, `summary()` and `lago_report()` now show values closer to 0 than 0.1 with 4 significant digits rather than 4 decimals. This covers the estimate, the interval bounds, the recommended intervention, the effective goal and the test statistic. Values closer to 0 than 1e-6 still show as 0.

* A covariate that `glm()` could not estimate is no longer also reported as held at 0, and a factor covariate with only some levels that `glm()` could not estimate is no longer reported as dropped. `get_confidence_set()` now refuses, with a clear message, a model with any other coefficient `glm()` could not estimate, instead of returning an empty set.

* Center level data now keeps an explicit `include_center_effects = FALSE`. It used to fit center effects anyway with no message, since the argument could not tell FALSE from not given. Its default is now `NULL`, which means TRUE for center level data and FALSE otherwise. Center level data also refuses an `outcome_name` other than `"proportion"`, the column it is fitted on, instead of fitting `proportion` while other parts read the named column.

* Fixed the shrinking method for center level data and weighted data. The stage-1 intervention it shrinks towards was the plain mean of the rows, so an unreachable goal gave a different recommendation than the same patients one row each. It is now weighted by `center_sample_size`, or by the `glm()` weights.

* The confidence set grid now includes each upper bound, as the optimizer grid already did. When the step did not divide the range, the set could miss the corner where the recommendation sits and come back empty. `confidence_set_grid_step_size` must now be finite and positive.

* Fixed the numerical optimizer giving a costlier recommendation when an intervention component is in large units. A restart that ended a hair outside a bound was left out, so a point on the bound was lost, and the solver's fixed tolerances could stop a wide component early. A restart within 1e-8 of a bound now counts as inside it, and a last start is solved in each component's share of its range. Results that were already right are unchanged.

* Fixed the unconditional power goal being held above the stage-1 control rate, which overstated it when the planned control rate, for example at low center characteristic values, was lower.

* Overriding `unit_costs`, `default_cost_fxn_type` or `unit_cost_doses` in `lago_sensitivity()` or `lago_budget()` on a stored result now replaces the stored `cost_list_of_vectors`. The stored list used to stay in place and win.

* Fixed binary `lago_budget()` never trying goals near 0 or 1, so a rare outcome could get a much worse result than the budget buys. The goal grid now extends towards 0 and 1 without passing them.

* The default cubic cost no longer fails for a component held at one dose (equal bounds), which now gets the linear cost, or for narrow bounds far from 0, such as 1000 to 1100. Bounds so far from 0 for their width that the cubic cannot be computed accurately are refused with a clear message.

* The printed cost IQR keeps 3 significant digits for a cost below 1, so a cost in small units no longer shows as 0 - 0.

* A continuous outcome with a family other than gaussian on the identity link, such as quasibinomial, now uses the variance `glm()` reports, which weighs the rows by the family's variance function. Its intervals used to ignore that function.

* The MCP server's `optimize` and `sensitivity` tools now take the next-stage size, `power_goal_approach`, `icc` and `power_goal_cluster_id`, so a power goal can be run through them.

* Fixed the unconditional power goal counting significance in the harmful direction. Its test statistic was squared, so an outcome at which the next stage would show that the intervention is worse also met the power goal. When the treatment arm started below control it could return such a goal, which has almost no power to show benefit, or NA. Only outcomes that show benefit now count.

* Fixed a conditional power goal that cannot be reached silently becoming an outcome goal of 0, which recommended no intervention. Both approaches now warn that the power goal cannot be reached and set the power-implied goal to 1, so the shrinking method is used. The warning no longer blames `icc` when `icc` is 0 or not given.

* Fixed `lago_budget()` with a power goal counting a run as reached when it was shrunk because the power-implied goal could not be reached. It then called a shrunk point the maximum reachable outcome. Such runs are now not reached, and the warning names the power goal.

* Fixed the shrinking method losing precision when an intervention component is in large units. Its search for the needed coefficient stopped at an absolute width of 1e-6, so a small coefficient was found too coarsely, and at very large units the recommendation fell back to the stage-1 values. The width is now relative to the coefficient below 1, so other results are unchanged.

* The interval bounds of the confidence set and of the estimated outcome now keep 4 significant digits below 0.1 rather than 3 decimals. A rare binary outcome showed intervals such as 0.002 to 0.003, and some confidence set rows showed an interval that did not contain the goal although the row was in the set. Bounds of 0.1 or more are unchanged.

* Fixed the binary confidence set running out of memory with four intervention components on the default grid, since it built a matrix with one row and one column per grid point to read its diagonal.

* `glm_family = "quasibinomial"` now takes the logit link by default. It used to fail with an unclear error unless `link` was given.

* Column names that are not valid R names, such as names with spaces, are now refused with a clear message. They used to fail with a parse error, or could fit a different model.

* Fixed `visualize_cost()` losing the small high order coefficients of a cubic cost. Its sliders report values rounded to 5 decimals, so the curve it drew and the cost list it returned and copied could cost half of the curve it set up. The app now keeps the exact values it sets, for the drawn curve too, and the copied text keeps full precision.

* `lago_optimization()` now warns when both `center_weights_for_outcome_goal` and `center_effects_optimization_values` are given, since the center weights are used and the named center is ignored. It also warns that `weights` is ignored for center level data, whose rows are weighted by `center_sample_size`.

* The check that the outcome goal is above or below the observed mean now weighs the rows as `glm()` does, so for center level data it uses the mean over patients rather than over rows.

* `lago_sensitivity()` now refuses an intervention component named like one of its result columns (`value`, `rec_int_cost`, `est_outcome_goal`, `status`), which used to overwrite or hide it. `lago_budget()` likewise refuses a component named like a column it uses in its search (`outcome_goal`, `est_outcome`, `reached`, `affordable` and the others), which used to change its result. The printed summary of `lago_sensitivity()` now pairs each cost with its own value, so a descending sweep is no longer described backwards, and its `baseline` attribute is found for a value built with `seq()`.

* When the outcome goal cannot be reached, `print()`, `summary()`, `plot()` and `lago_report()` now say that the shrinking method was used and no confidence set is computed, rather than telling the user to set `include_confidence_set = TRUE`. The result has a new field `shrinking_method_used`. `print()` of a `lago_budget()` result now says minimum for a minimize budget that does not bind.

* `get_confidence_set()` now accepts a single clustering vector as `cluster_id` and refuses more than two with a clear message.

* With `unit_cost_doses`, the pinned cubic cost now also does not depend on the units of the dose.

* New argument `unit_cost_doses` in `lago_optimization()`: the dose each unit cost in `unit_costs` was measured at. The default cubic cost is then pinned there, so the cost per unit at that dose is the measured unit cost. Without it the cubic cost only matches each unit cost on average over the bounds, which could be far off at the measured dose, and a warning now says so. The cost is otherwise unchanged (#47).

* Fixed the numerical optimizer returning a recommendation that misses the outcome goal. `NlcOptim::solnl()` can stop at a point short of the goal without an error, and that point was kept when it was the cheapest. In one example with interacting components it recommended a point whose estimated outcome was 4.92 against a goal of 9.5. A restart that does not reach the goal is now left out. The optimizer also starts from the point of the highest outcome, and uses that point itself when no restart reaches the goal.

* The numerical optimizer now also starts off the diagonal of the bounds, at the corners and at each component's midpoint. All its restarts started on that diagonal, so a near symmetric problem could stop at a costlier point, or at a saddle point that wrongly made a reachable goal look out of reach. An extra start replaces the answer only when it is clearly cheaper, so other results are unchanged.

* Fixed the default cubic cost depending on the units of `unit_costs`. Its slope was a fixed number rather than one in the units of the cost, so restating the costs in thousands changed the shape of the cost curve and the recommendation. The slope now scales with each unit cost, so a unit cost of 1 is unchanged. Results with the cubic cost and other unit costs should be recomputed. A unit cost of 0 no longer makes the cost calculation run forever, and a negative one is refused.

* The `cost` column of the confidence set now keeps 6 significant digits when that is more precise than 2 decimals, so a cost in small units keeps its precision. Costs of 10000 or more are unchanged, and the printed cost IQR still shows 2 decimals.

* Fixed the overall intervention test on center level data. It compared the mean of the row proportions and counted each row as one patient. It now counts each row's `center_sample_size` patients, so it is the same test as for the same data at the patient level. In one example its sign was wrong.

* Fixed the confidence set when an additional covariate has a coefficient that `glm()` cannot estimate, such as one that is constant within each center. The confidence set was empty for a binary outcome and failed for a continuous one. It is now the confidence set of the fit without that term, which the run already warned it would be.

* A center or period whose rows all have a glm weight of 0 is now left out of the outcome model with a warning. It used to stop the run with a message about collinear predictors.

* A factor or character outcome, and an intervention component that is not numeric, are now refused with a clear message. They used to fail with an unclear error, or with a wrong message about collinear predictors.

* Fixed the confidence interval and the confidence set of a continuous outcome with fixed center or time effects. The variance was clustered on the centers and periods that the fixed effects already model, so with few centers the interval was far too narrow and the confidence set often empty, and with both effects the variance could be negative, so no interval was reported. In one example on the logit link the interval was 0.599 to 0.601 with an empty confidence set, and it is now 0.594 to 0.606. The variance is now the one used without fixed effects. Results without fixed effects, and all binary results, are unchanged. Continuous results with fixed center or time effects should be recomputed (#31).

* The coefficient table of `print()`, `summary()` and `lago_report()` now shows each estimate with the standard error the intervals use, and says which one it is. It used to show the `glm()` summary, whose standard errors for a continuous outcome on the logit link, or with fixed effects, were not the ones the intervals used (#30).

* Fixed `lago_optimization()`, `print()`, `summary()` and `lago_report()` failing with "subscript out of bounds" for a binary or continuous logit outcome with fixed center effects and more than one center weight, which includes the default weights. The interval of the weighted average center was correct but lost its `lower` and `upper` names, which the printed output and the Python wrapper read.

* `print()`, `summary()`, `plot()` and `lago_report()` now label the interval and the confidence set with the level they were computed at, from `confidence_set_alpha`. They always said 95%, so an 80% interval was printed as a 95% one.

* Fixed three ways `lago_budget()` could miss the best affordable outcome. When several goals tied on the estimated outcome, as all the goals below the no intervention outcome do, it refined the gap above the lowest of them rather than the highest, leaving budget unspent. It stopped extending its goal grid after 6 extensions. So when the best affordable outcome was far above the observed range, it was not found, and the budget was still reported as not binding. It now extends up to 30 times and warns if that is not enough. With `n_grid = 2` its refinement pass re-ran the same two goals, so nothing was refined, and it now tries goals strictly inside the gap. Other results of `lago_budget()` can also change slightly, within the resolution of its goal grid.

* Fixed the default center weights for center level data using only each center's first row of `center_sample_size`. With several rows per center of different sizes, such as one row per center and period, the weighted average center then depended on the order of the rows and did not match the same data at the patient level. Each center is now weighted by the sum of its `center_sample_size`, which is how `glm()` weights its rows. Results for center level data with several rows per center and the default weights should be recomputed.

* Fixed grid search never trying an upper bound that its step does not reach, which the default steps (a twentieth of the observed range) often do not reach when the bounds differ from the observed range. It then returned a costlier recommendation, or said the goal could not be reached and shrank, when the upper bound would have reached it. The grid of each component now includes its upper bound.

* Fixed the power goal using the control rate at center characteristic value 0, the reference center and the reference period. The next-stage control arm gets no intervention, and its rate was taken as `expit()` of the model intercept, which is the rate only on the logit link when the model has no other terms. On the identity link the intercept is already the rate, so `expit()` of it was wrong even without other terms. With center characteristics or fixed center effects, the power-implied outcome and the recommendation then changed when a characteristic was merely shifted by a constant or the reference center was changed. In one example the raw characteristic gave a power-implied outcome of 0.294, whose true power is 0.215 against a goal of 0.8, while the centered one gave the correct 0.467. The control rate is now the estimated outcome with no intervention at the same planning values as the outcome goal: the center characteristic values, the center weights or named center, and the period. Results with a power goal and center characteristics, fixed center effects or fixed time effects, or on the identity link, should be recomputed.

* Fixed the power goal on center level data counting each row as one patient and summing the proportions as the number of events. It now counts each row's `center_sample_size` patients and its events, as for the same data at the patient level, including the stage-1 cluster sizes used with `icc`. A row without `center_sample_size` is left out, as `glm()` leaves it out. In one example the power-implied outcome was 0.399, with a true power of 0.889 against a goal of 0.8, and is now 0.343, the same as for the patient level data.

* Fixed the confidence interval and the confidence set of the weighted average center on the logit link. The reported outcome is the weighted mean of each center's probability, but its interval was built for `expit()` of the weighted mean linear predictor, a different quantity, so the printed interval could exclude its own estimate, for example an estimated outcome of 0.75 with an interval of 0.764 to 0.835. The interval is now the delta method interval of the weighted mean probability, for binary and continuous outcomes and for individual and center level data. A single named center, and the identity link, are unchanged. Confidence sets for the weighted average center on the logit link should be recomputed.

* Fixed three errors in the shrinking method used when the outcome goal cannot be reached. With interaction terms it read each main component's coefficient by its position in `main_components`, so listing an interaction term before a main component, or the main components in another order than in `intervention_components`, gave a different recommendation for the same model. It also searched for the needed coefficient only up to 100, so an outcome on a large scale, such as a weight in grams, gave a different recommendation than the same outcome in kilograms. When a step of the search landed on the goal exactly, the search also returned the lower end of its bracket instead of that step, which could be far from the needed coefficient. Results that used the shrinking method should be recomputed, most of all those with interaction terms or with a needed coefficient above 100.

* The documentation of `shrinkage_threshold` now says that it is not used. The shrinking method is applied whenever the outcome goal is not estimated to be reachable, which has been the behavior since the threshold rule was removed.

* Fixed the in-browser playground treating a numeric `NaN` period as a missing value when it checks which centers have a row the outcome model can use. The package turns a numeric `period` column into a factor, which keeps `NaN` as a level and its rows in the fit, so with fixed center and time effects the playground could refuse to run, or say a center could not be optimized for, when the package would fit it. When `period` is used as a numeric intervention component or covariate instead, a `NaN` still counts as missing, as it does in `glm()`.

* Three input problems now get clearer refusals. `center_effects_optimization_values = NA_character_` with a `center` column that has missing values but no explicit `NA` level failed with "values in center_weights_for_outcome_goal must sum up to 1", and now says that `NA_character_` is not one of the centers and how to make the missing centers an explicit level with `addNA()`. The three refusals of `center_effects_optimization_values` no longer print a line break and a run of spaces in the middle of the sentence, and the one for an unknown center names the value. A global `options(na.action = "na.pass")` or `"na.fail"` with a missing value in a model variable or its glm weight failed inside `glm()` with an error that did not name the option, and is now refused up front with a message that says to use `"na.omit"` or `"na.exclude"`. Data without missing values still runs under either option.

* Fixed wrong results when two coefficients of the outcome model got the same name. `glm()` names a factor coefficient by joining the column name and the level, so a level labeled with the text `"NA"` and an explicit `NA` level (as from `addNA()`) both became, for example, `centerNA`, and a factor `visit` with level `2` and a numeric column `visit2` both became `visit2`. The package reads coefficients by name. With fixed center effects and `include_confidence_set = FALSE`, the recommendation was then computed for the wrong center, or with the center weights paired with the wrong centers, without any error. With the confidence set, the call failed with a message about unmatched coefficients. Such a fit is now refused with a message that names the coefficient and its columns. This also refuses some fits that gave correct results before, such as a period or covariate with both kinds of `NA` level and no confidence set, so that one rule applies everywhere. Center results from such data computed without the confidence set should be recomputed after renaming one of the levels or columns. When every run of a sweep fails, such as with this refusal or an outcome model that cannot be fitted, `lago_budget()` still returns `feasible = FALSE`, but its warning and printed result now give the error of the first failed run, and the new `run_error` field holds it. The printed result no longer suggests a larger budget in that case.

* The documentation of `get_confidence_set()` now says that `center_weights_for_outcome_goal` has one weight per center of the fitted model, in the order of its center levels, and the documentation of `center_effects_optimization_values` now says that `NA_character_` selects an explicit `NA` center level. The README examples now show the outcome model's actual printed call.

* `lago_sensitivity()` and `lago_budget()` are faster. No input they can sweep changes the outcome model, but every run refitted it, which was most of the time of a sweep. A sweep now fits the model once and reuses it for every run with identical fitting inputs, and a budget search shares one fit across all of its sweeps. A 25 value sensitivity sweep on `BB_data` went from 3.7 to 1.3 seconds, and a default budget search from 6.6 to 2.3 seconds. Results are unchanged, and the warnings and messages from the fit are still raised for every run. A single `lago_optimization()` call always fits its own model.

* `lago_sensitivity()` and `lago_budget()` now show each message about how the inputs were read once per call, instead of once for every run of the sweep. Examples are the note that the `center` column was converted to a factor and the note that `optimization_method` was switched to grid search. A message is hidden only when its text is exactly the same as one already shown. Other messages, such as a note about one run's power goal, and the progress lines shown with `quiet = FALSE` still appear for every run. The note that `default_cost_fxn_type` is ignored when `cost_list_of_vectors` is given is now sent as a message like the other input notes, rather than printed with `print()`, so it no longer shows as `[1] "..."` in the output.

# LAGOtrials 1.1.0

* Fixed two unclear refusals for fixed center and time effects. An all `NA` `center` column was refused with "values in center_weights_for_outcome_goal must sum up to 1", or with a message about the length of `center_weights_for_outcome_goal` when the weights were given. It is now refused with the same kind of message an all `NA` `period` column gets. A center or period whose rows all have zero weight counted as usable, so when no other center or period was left the fit failed with a rank deficiency message that blamed collinear predictors. Such a level no longer counts, and the message names the center or period that still has a positively weighted row, if there is one.

* `get_confidence_set()` now refuses a fitted model with a factor, character or logical predictor that is not treatment coded with its first level as the reference and each coefficient named after its level. Examples are a model fitted with `contr.sum`, `contr.SAS`, a global `options(contrasts = ...)` that sets another coding, the default coding of an ordered factor, or a treatment matrix such as `contr.treatment(3)` whose columns are not named after the levels. The function reads every such predictor that way. For a binary outcome it returned a wrong interval and confidence set without any error. A treatment matrix whose columns are not named after the levels gave a wrong result only for the period, whose dummy is found by its level name, and is now refused for every column so that one rule applies. For a continuous outcome it usually failed with a message about unmatched columns, but it could also return a wrong interval without any error, for example for a factor with levels 0 and 1. The new message names each such column and shows how to refit the model. Models fitted by `lago_optimization()` already use this coding, so its results are unchanged. Results from direct calls with such a model should be recomputed after refitting it.

* Fixed a data column named `weights` replacing the weights of the outcome model. `glm()` looked up its `weights` argument in the data first, so such a column replaced the `weights` argument of `lago_optimization()`, replaced `center_sample_size` for center level data, and weighted the fit even when no weights were given. A row with a missing value in that column was also dropped from the fit. The outcome model now always uses the `weights` argument, or `center_sample_size` for center level data, and a column named `weights` is used only where it is named, such as an additional covariate. This also applies to `lago_sensitivity()` and `lago_budget()`. Results from data with a `weights` column should be recomputed.

* Continuous confidence covariance now uses the actual fitted model's prior weights. Identity covariance uses weighted bread and residual scores, with estimated residual scale divided by the positive weight row count minus the design column count when no clusters are present. Logit covariance retains the existing Gauss Newton bread and HC0 or CR0 sandwich, now with weighted scores. Both clustering dimensions and their intersection use the same fitted weights. Weights are relative observation weights, so a common positive rescaling leaves covariance unchanged for a fixed fit. Zero weight rows contribute nothing, malformed fitted weights are refused, and unclustered identity covariance requires positive residual degrees of freedom. Binary covariance and existing finite unit weight results are unchanged. This extends the existing continuous formulas, not a frequency weight, survey design or general GLM covariance. The existing continuous quasibinomial family variance mismatch is not changed. Results from continuous fits with nonconstant weights should be recomputed.

* Fixed recommendations and confidence output that depended on global contrast options or polynomial coding of ordered covariates. Every categorical predictor in the outcome model now uses treatment contrasts, including factor, ordered factor, character and logical covariates and center characteristics. This overrides explicit contrast attributes without changing input data or global options. Additional categorical covariates are held at their reference level. For a categorical center characteristic with two levels, optimization values 0 and 1 select the reference and other level. Numeric predictors and the existing center and period checks are unchanged. Results from affected runs should be recomputed.

* Fixed wrong results when the `center` or `period` column was not coded with one effect per level. `glm()` fits an ordered factor with polynomial contrasts, and a global `options(contrasts = ...)` such as `contr.sum` changes the coding of every factor, but the package read the resulting coefficients as one effect per center or period. With fixed center effects, a binary outcome or any run without the confidence set returned a wrong recommendation and estimate without any error, and a continuous outcome failed when it computed the confidence set. With fixed time effects an ordered period was refused except at its first level, and under a global coding such as `contr.sum` or `contr.helmert` whether a period was refused depended on its label. A period that was not refused could give a wrong result in the same way. The outcome model now always fits `center` and `period` with treatment contrasts, and an ordered `center` or `period` is converted to an unordered factor with the same levels, with a message. Results from such runs should be recomputed.

* Fixed center or time effects with fewer than two centers (or periods) are now refused with a message that says whether the data has no observed period, only one center (or period), or only one with a row the outcome model can use, and names that level, instead of failing inside `glm()` with "contrasts can be applied only to factors with 2 or more levels". A fit with center or time effects where no row is usable at all is refused with a message that says so.

* Fixed results that used rows the outcome model never fitted. `glm()` drops a row with a missing value in any model variable, but the confidence set was computed over every row, and the center weights covered every center in the data. With a continuous outcome and missing values the confidence set came back empty or wrong, with or without fixed center or time effects, and in some cases the call failed (for example with fixed center effects and a missing covariate, or a missing period with fixed time effects), so such results should be recomputed. A center whose every row has a missing model variable still got a weight: `lago_optimization()` failed when it computed the confidence set, and otherwise (without the confidence set, after shrinking, and in `lago_sensitivity()` and `lago_budget()`) it returned results with mismatched center weights, silently or with at most R's generic "longer object length" warning (none when the number of centers is a multiple of the number fitted), so every run with such a center should be recomputed. The confidence set now uses the rows the model was fitted on, and the center weights now cover only the centers it fitted. A center the model could not fit is left out of the default weights with a warning that names it (the other centers are still sized by all of their rows, and the shrinking target and default grid steps still use every row, since the stage-1 intervention was delivered either way), and naming it in `center_effects_optimization_values`, or giving it a non-zero weight in `center_weights_for_outcome_goal`, is refused with a message that names it. A factor `center` column with unused levels no longer shifts the default weights onto the wrong centers, and an explicit `NA` center level (as from `addNA()`) is kept as a center when another center is left out (a continuous outcome's confidence set still cannot use an explicit `NA` level that a fitted row has in the center or period column, or as a non-reference level of any other factor column, and now says so instead of reporting a missing cluster id or returning an empty set), and a row with a missing center no longer counts as a facility when checking `center_weights_for_outcome_goal`. For center level data, a center with no `center_sample_size` on any row is refused with a message that names it when the default weights are computed, and a center whose size is missing on only some rows is sized by its first observed value.

* Fixed `lago_optimization()` failing for a continuous outcome when the data is a tibble, such as the bundled `BB_data`. With the identity link it failed with "undefined columns selected" when fixed center or time effects were included, and with the logit link it failed with "Not compatible with requested type" with or without them.

* The in-browser playground now supports fixed center effects: tick "Fixed center effects" to adjust the outcome model for each center, matching `lago_optimization()`'s `include_center_effects`, and choose whether to optimize for the size-weighted average center or for one named center (`center_effects_optimization_values`). The center column comes from the data's own `center` column when it has one, used as is, otherwise from a column you pick (`site_name` for `BB_data`), and the shown R code includes the line that builds it on a copy of the data. Fixed center effects replace center characteristics, which the package cannot fit together, and the center column is not offered as a component or an additional covariate. The sensitivity sweep, budget search and Share link carry the setting. A center with no row where all the model columns are observed is left out of the fit, and the page asks for another center if you chose it as the one to optimize for.

* The documentation of `center_effects_optimization_values` now says it is a single center name (a character string), which is what `lago_optimization()` has always required, rather than a numeric vector.

* Fixed an outcome goal or a power goal failing when the outcome column has missing values. The check of the outcome goal's direction compared it against the outcome mean without removing missing values, so any missing outcome stopped `lago_optimization()` with "missing value where TRUE/FALSE needed". The power calculation counted a row with a missing outcome toward its arm's size but made the arm's sum missing, so the default unconditional approach failed the same way and the conditional approach silently returned a power-implied outcome of 0, which dropped the power goal. Both now use only rows with an observed outcome, as the final treatment-versus-control test does, and a power goal now needs a `group` column with both arms present and an observed outcome in each.

* Fixed the in-browser playground misreading a column named like a built-in JavaScript property (for example `constructor` or `toString`). Ticked as an intervention component, it made the generated R code, Run, the sensitivity sweep and the budget search fail, and its bounds and unit cost came up collapsed or blank instead of the column's defaults. As a center characteristic, its held-at value came up blank and was left out of the call.

* The in-browser playground now supports a power goal (binary outcomes): tick "Power goal" to plan for the next stage's statistical power alongside, or instead of, the outcome goal, matching `lago_optimization()`'s `power_goal`, `num_centers_in_next_stage`, `patients_per_center_in_next_stage`, `power_goal_approach`, `icc` and `power_goal_cluster_id`. The treatment arm comes from the data's own `group` column when it has one (used as is, so it must hold only "treatment" and "control" with both present), otherwise from a complete 0/1 column you pick (`pre_post` for `BB_data`, as in the package tests), and the shown R code includes the lines that build it on a copy of the data. Direction must be Maximize, and Run stays disabled with a hint until the power settings are complete. The ICC cluster column list offers only columns with at least two non-missing centers with an observed outcome in each arm, checked when an ICC is entered. The sensitivity sweep can vary the power goal, and the budget search and Share link carry the settings. On `BB_data` the stage-1 data already powers the comparison, so the power goal alone recommends no intervention, and the printed result reports the power-implied outcome.

* The in-browser playground now has an optional "Additional covariates" section: tick any columns (numeric or not, for example a site or arm label) to adjust the outcome model for them, matching `lago_optimization()`'s `additional_covariates`. A column can play only one role at a time (intervention component, center characteristic, or additional covariate). The chosen covariates ride along in the Share link.

* The in-browser playground now supports fixed time effects: when the loaded data has a `period` column (so, an uploaded CSV, since no bundled dataset has one) a "Fixed time effects" checkbox appears with a period value to optimize at, matching `lago_optimization()`'s `include_time_effects` and `time_effect_optimization_value`. (Time effects are not carried in the Share link, which is offered only for bundled datasets.)

* The in-browser playground now has an optional "Interaction terms" section: with two or more intervention components ticked, tick a pair to model their product (for example `coaching_updt × launch_duration`). It sets `include_interaction_terms = TRUE` and `main_components`, appends the `a:b` term to `intervention_components`, and prepends the line that creates the interaction's product column so the shown R code stays copy-pasteable. Bounds, costs and the confidence set stay per main component, and the sensitivity sweep, budget search and Share link inherit the interactions.

* The in-browser playground's "Share link" now carries the whole page, not just the core optimization: the center characteristics and their held-at values, the interaction terms, the additional covariates, a budget you have entered, and the sensitivity sweep's input, range and step count all round-trip through the URL, so an opened link reproduces the full setup. Older or hand-trimmed links that omit these simply keep the page's own defaults for those controls.

* The in-browser playground now has an optional "Center characteristics" section: tick numeric columns (for example a center's birth volume) and set the value each is held at, and the recommendation is computed for a center with those characteristics, matching `lago_optimization()`'s `center_characteristics` and `center_characteristics_optimization_values`. The sensitivity sweep and budget search inherit the same setting. A column can be an intervention component or a center characteristic but not both.

* The in-browser playground now has a "Budget" section that surfaces `lago_budget()`: enter a cost budget and it finds the best outcome reachable within it and the intervention that reaches it (the reverse of setting an outcome goal). It reports the recommendation, its cost and estimated outcome (or that no intervention fits), notes when the budget does not bind, and draws the cost/outcome frontier as an interactive D3 chart with the budget line and the chosen point marked. The budget field auto-fills a sensible default from the current costs and bounds until it is edited.
* Added `lago_budget()`, which answers the reverse of the usual LAGO question: given a fixed cost budget, what is the best outcome reachable, and with which intervention? It searches the outcome goal along the least-cost frontier `lago_optimization()` traces out and returns the most ambitious reachable goal whose recommended cost fits the budget (the lowest reachable outcome within budget for `outcome_goal_intention = "minimize"`), with `print()` and `plot()` methods and a `frontier` of the cost-vs-outcome points searched. Like `lago_sensitivity()`, it reuses a fitted `lago` result's call or takes the arguments directly, and it never computes the confidence set. It reports whether the budget is feasible and whether it binds.
* Fixed the in-browser playground's default configuration not producing a result. It prefilled the first binary column as the outcome, which for `BB_data` is the `pre_post` pre/post period flag: perfectly separable from the coaching components, so the outcome model never converged and Run (and the sensitivity sweep) errored out of the box. Each bundled dataset now starts from a preset that fits (`BB_data` uses the `pp3_oxytocin_mother` outcome with the `coaching_updt` and `launch_duration` components; `mtcars` uses `mpg` with `gear` and `qsec`), and switching datasets or pressing Reset lands on that dataset's runnable preset. Uploaded CSVs keep the generic prefill.
* The in-browser playground now has a "Sensitivity sweep" section that re-runs the recommendation across a range of one input, using `lago_sensitivity()`. Pick the input to vary (the outcome goal or a cost multiplier), a from/to range and a number of steps, and it draws how the recommended cost moves as an interactive D3 line (with each run's per-component recommendation, estimated outcome and status in a table beneath). The confidence set is skipped during the sweep for speed, and a failed run is shown as a gap rather than aborting the sweep.
* The in-browser playground now has a "Share link" button that encodes the whole current setup (bundled dataset, outcome column and type, intervention components, their bounds and costs, the goal and direction) into the page URL and copies it to the clipboard. Opening that link restores the configuration on load, ready to run. Costs are encoded as per-component coefficient vectors, so a curve shaped in the cost designer is preserved along with plain unit costs. Sharing is offered only for the bundled datasets, since an uploaded CSV cannot be carried in a URL.
* The in-browser playground and the cost designer now round-trip. A "Shape costs in the designer" button opens `visualize_cost()` in the browser pre-loaded with the intervention components, bounds and unit costs configured in the playground (the designer reads this from its page URL query and falls back to its built-in example when the query is absent or malformed; a plain `visualize_cost(...)` call is unchanged). In the designer, a "Use these costs in the playground" button sends the shaped cost functions back to the playground tab, which then uses them as `cost_list_of_vectors` for the optimization (shown in the reproducible R code) instead of the linear unit cost; adjusting a component's unit cost reverts it.
* `visualize_cost()` now has a "Cost function form" toggle that switches all components between the linear and cubic cost functions live, instead of the form being fixed for the whole session by `default_cost_fxn_type`. Switching resets each component to that form's initial coefficients and shows the right number of coefficient sliders (2 for linear, 5 for the cubic's degree-4 total cost). The cubic form opens on a visibly curved (but still valid) demo cost rather than the near-linear one `cost_fxn_calculator()` returns for typical unit costs, so the toggle clearly shows a cubic. This also gives the in-browser cost designer a cubic option.
* The total cost curve in `visualize_cost()` can now be reshaped by dragging: each curve carries a draggable handle at several points along it, and dragging one refits the cost function through the new set of points (and updates the sliders), instead of the single right-endpoint handle that only rescaled the whole curve.
* The fitted outcome model's `Call:`, shown in the "Outcome model fit" section of `print()` and `summary()`, now displays the actual model formula instead of the literal word `formula`. This also makes the console output identical across R versions: a change in recent R to how the unrecorded formula deparses had otherwise made the output (and its snapshot tests) version dependent.
* Added a live in-browser demo to the documentation site (`live-demo.html`) that runs the real package client-side with webR (R compiled to WebAssembly), so anyone can try `lago_optimization()` with no installation, plus an interactive playground (`playground.html`) where you pick a bundled dataset or upload a CSV, configure the model with sliders and toggles, and see the recommendation drawn with the package's own D3 charts alongside a copy-pasteable R snippet. A GitHub Actions workflow builds the package to WebAssembly with the rwasm toolchain and publishes it as a small CRAN-like repository alongside the site.
* Added a browser version of `visualize_cost()`: the same Shiny cost-function designer now runs client-side on the documentation site (`/visualize-cost/`), exported with shinylive, so you can shape each component's cost curve and copy the coefficient list with no install. `visualize_cost()` was refactored to build its app object separately from launching it, so the local and in-browser versions share one implementation.
* `lago_report()` now renders an interactive HTML dashboard: the confidence set is a hover-enabled D3 plot (a scatter for two components, a strip for one) with the recommended intervention highlighted, and each intervention component gets interactive total-cost and marginal-cost curves. The report stays a single self-contained offline file (D3 is inlined, no CDN or server) and its API is unchanged; rendering now also uses `jsonlite` (a new Suggests).
* Added an MCP (Model Context Protocol) server to the Python package (`python -m lago.mcp_server`) that exposes `optimize` and `sensitivity` as tools any MCP-aware AI agent can call, plus a `sensitivity()` function in the Python wrapper.
* Added `lago_sensitivity()`, which re-runs an optimization across a sweep of one input (an outcome or power goal, or a `"cost_multiplier"` that scales all costs) and reports how the recommended intervention, its cost, and the estimated outcome move, with `print()` and `plot()` methods.
* The package is now installed and loaded as `LAGOtrials` (call `library(LAGOtrials)`); the previous `LAGO` identifier clashed with an archived CRAN package. Function names, the `lago` result class, and the LAGO method name are unchanged.
* `visualize_cost()` now stores the returned cost list in the `lago_cost_list` option instead of assigning it to the global environment; retrieve it with `getOption("lago_cost_list")`.
* Added tests for the interaction-terms optimization path and the outcome-model fit warnings, and excluded the interactive `visualize_cost()` app from the coverage figure so it reflects the testable R code.
* Added a `CITATION.cff` so the repository can be cited, plus contributing guidelines, a code of conduct and issue/pull-request templates. (#86)
* Added test-coverage reporting, cross-platform (macOS, Windows, Linux) continuous integration, project-status and coverage badges, and a social-preview card. (#83, #84, #85)
* Added a Python wrapper (in `python/`, importable as `lago`) that calls LAGOtrials through `rpy2`. (#81)
* `visualize_cost()` now draws its cost curves client-side with D3, with hover read-outs, a draggable curve endpoint, and invalid-state highlighting. (#80)
* The clustered variance estimator for logit outcomes is now computed by a compiled Rcpp kernel, so `Rcpp` is a new dependency. (#79)
* `lago_optimization()` now refuses an additional covariate whose column is entirely `NA`, naming it. (#77)
* A rank-deficient outcome model is now refused up front with an error naming the aliased terms. (#74, #76)
* Added a warning when `glm()` drops an additional covariate as collinear. (#76)
* Fixed confidence-interval bounds for a binary logit outcome being reported outside `[0, 1]`. (#75)
* Fixed the estimated outcome being scaled by center weights that did not sum to 1. (#72, #75)
* Added a warning when a binary outcome's estimated outcome is reported outside `[0, 1]`. (#75)
* Added a warning when a numeric additional covariate observed away from 0 is held at 0 for the confidence set. (#75)
* Fixed the confidence interval and set being computed on the logit scale for an identity-link binomial model. (#74)
* `center_weights_for_outcome_goal` must now be numeric, finite and non-negative, checked at both entry points. (#73, #74)
* Fixed the numerical optimizer failing with an opaque error when every restart failed, so it now gives an actionable message. (#73)
* Fixed a factor covariate named like "center" or "period" being miscounted as a fixed effect when `get_confidence_set()` is called directly. (#73)
* Fixed `outcome_goal_intention = "minimize"` ignoring the outcome goal on a logit link. (#70)
* Fixed the recommended intervention sometimes falling outside the intervention bounds. (#70)
* Fixed the numerical optimisation returning its most expensive candidate solution instead of the cheapest. (#70)
* Fixed the estimated outcome being wrong when more than one center characteristic was supplied. (#70)
* `link = "probit"` and `link = "log"` are no longer accepted, since only `logit` and `identity` were ever implemented. (#70)
* Fixed `$est_outcome_ci` sometimes reporting the wrong interval instead of the one at the recommended intervention. (#68)
* Fixed the confidence set occasionally dropping one of its qualifying interventions. (#68)
* Fixed `confidence_set_size_percentage` understating the size of the confidence set. (#68)
* A confidence set containing exactly one grid intervention is no longer discarded as empty. (#68)
* Fixed confidence-interval bounds being attached to the wrong interventions under two-way clustering. (#68)
* `get_confidence_set()` now returns `rec_int_ci`, the interval at the recommended intervention (its return shape changed for direct callers). (#68)
* The confidence set, its size, and the estimated-outcome interval can differ from earlier releases wherever the previous values were wrong. (#68)
* Fixed `get_confidence_set()` pairing the prediction matrix with the model coefficients by position instead of by name. (#68)
* Fixed the estimated outcome being reported as 0 for the reference period when fixed time effects were included. (#68)
* Fixed the estimated-outcome interval being computed at the wrong time period. (#68)
* Fixed an additional covariate or center characteristic named like "center" being miscounted as a fixed center effect. (#68)
* Fixed a factor or character additional covariate or center characteristic being paired with the wrong coefficient or erroring. (#68)
* Fixed the confidence set changing when the rows of `data` were reordered. (#68)
* Fixed passing more than one center characteristic to the confidence set failing or duplicating a column. (#68)
* Added runnable `@examples` to every exported function that lacked them. (#67)
* Restyled the `print()` and `summary()` console output with boxed `cli` sections. (#65)
* The fitted outcome model (`$model`) and the estimated-outcome interval (`$est_outcome_ci`) are now returned on the result. (#65)
* Added `lago_report()`, which renders a self-contained HTML report of a result. (#64)
* `lago_optimization()` now returns a `"lago"` object with `print()`, `summary()`, and `plot()` methods. (#61)
* Added an "Optimizing an intervention with LAGO" vignette. (#61)
* Added a hex logo and refreshed the README header. (#56, #61)
* Added a `quiet` argument to `lago_optimization()` that skips progress messages. (#60)
* `visualize_cost()` gained a copy-to-clipboard button, a numeric cost summary, and returns the cost list on close. (#59)
* Expanded the reference documentation for the bundled datasets. (#58)
* Added a GPL-3 `LICENSE` file. (#56)
* Fixed a crash when a single intervention component was used. (#54)
* Added a pkgdown documentation site published to GitHub Pages. (#53)
* The overall intervention-effect test result is now returned in the result object. (#49)
* Added an automated test suite (testthat) and continuous-integration checks. (#45)
* `lago_optimization()` now accepts a standalone power goal, an outcome goal, or both together. (#40)
* Added non-fatal outcome-model fit diagnostics. (#36)
* `visualize_cost()` sliders now support per-component default and custom ranges. (#34)
* Supplying `optimization_grid_search_step_size` now switches the optimization method to grid search automatically. (#32)
* Added an `icc` argument so the power goal can account for within-center clustering. (#29)

## Known limitations

* The confidence set is the set of interventions whose confidence interval
  covers the outcome goal, which is a two sided test and does not depend on
  `outcome_goal_intention`. Under `outcome_goal_intention = "minimize"` it can
  therefore contain interventions whose estimated outcome is above the goal, and
  which cost more than the recommendation, because their interval still reaches
  the goal from above. Read the confidence set as the interventions the data
  cannot distinguish from the goal, not as the interventions that meet it. The
  estimated outcome and its interval, reported as `est_outcome_goal` and
  `est_outcome_ci`, are computed at the recommended intervention and are
  direction independent, so they are unaffected.
