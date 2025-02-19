# helper function to assign global variables
# to be used in the lago_optimization function
# to avoid passing them as arguments
.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "family_object",
    "lower_outcome_goal",
    "model",
    "step_size_results",
    "rec_int",
    "rec_int_cost",
    "est_outcome_goal",
    "cs",
    "x",
    "y"
  ))
}
