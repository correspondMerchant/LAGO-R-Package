# A sweep reruns lago_optimization() with inputs that never change the outcome
# model, since lago_sensitivity() refuses to sweep the data, the model terms,
# the weights and the family. Refitting the same model was most of a sweep's
# time, so a sweep keeps each fit here and reuses it. The cache is active only
# while a sweep runs, so a plain lago_optimization() call always refits.
.lago_fit_cache <- new.env(parent = emptyenv())
.lago_fit_cache$active <- FALSE
.lago_fit_cache$entries <- list()

# Activate the fit cache. Returns TRUE when this call activated it, so only the
# outermost caller closes it: a nested call, such as lago_budget() running
# lago_sensitivity(), shares the cache of the outermost call.
open_fit_cache <- function() {
  if (isTRUE(.lago_fit_cache$active)) {
    return(FALSE)
  }
  .lago_fit_cache$active <- TRUE
  .lago_fit_cache$entries <- list()
  TRUE
}

# Deactivate the fit cache and drop the fits it holds.
close_fit_cache <- function() {
  .lago_fit_cache$active <- FALSE
  .lago_fit_cache$entries <- list()
  invisible(NULL)
}

# Evaluate expr with the fit cache active.
with_fit_cache <- function(expr) {
  if (open_fit_cache()) on.exit(close_fit_cache(), add = TRUE)
  expr
}

# Fit the outcome model, or reuse the fit an earlier run of the same sweep made
# from identical inputs. The warnings and messages the fit raised are replayed
# on each reuse, so a sweep reports exactly what it reported before. A fit that
# fails is not kept, so every run that needs it fails the same way.
cached_outcome_model_fitting <- function(...) {
  if (!isTRUE(.lago_fit_cache$active)) {
    return(outcome_model_fitting(...))
  }
  args <- list(...)
  # a family object holds fresh closures on every run, so compare it by name
  family <- args$family_object
  args$family_object <- c(family$family, family$link)
  # these global options change how glm() codes factors and drops rows
  key <- list(
    args = args,
    contrasts = getOption("contrasts"),
    na_action = getOption("na.action")
  )
  for (entry in .lago_fit_cache$entries) {
    if (identical(entry$key, key)) {
      for (condition in entry$conditions) {
        if (inherits(condition, "warning")) warning(condition) else message(condition)
      }
      return(entry$value)
    }
  }
  conditions <- list()
  value <- withCallingHandlers(
    outcome_model_fitting(...),
    warning = function(w) conditions[[length(conditions) + 1]] <<- w,
    message = function(m) conditions[[length(conditions) + 1]] <<- m
  )
  .lago_fit_cache$entries[[length(.lago_fit_cache$entries) + 1]] <- list(
    key = key, value = value, conditions = conditions
  )
  value
}
