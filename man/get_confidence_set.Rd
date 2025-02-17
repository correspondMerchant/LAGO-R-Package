% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_confidence_set.R
\name{get_confidence_set}
\alias{get_confidence_set}
\title{get_confidence_set}
\usage{
get_confidence_set(
  predictors_data,
  include_center_effects = FALSE,
  center_weights_for_outcome_goal = 1,
  include_time_effects = FALSE,
  additional_covariates = NULL,
  intervention_components,
  include_interaction_terms = FALSE,
  main_components = NULL,
  outcome_data,
  fitted_model,
  link,
  outcome_goal,
  outcome_type,
  intervention_lower_bounds,
  intervention_upper_bounds,
  confidence_set_grid_step_size,
  center_characteristics = NULL,
  center_characteristics_optimization_values = 0,
  confidence_set_alpha = 0.05,
  cluster_id = NULL,
  cost_list_of_vectors,
  rec_int
)
}
\arguments{
\item{predictors_data}{A data.frame. The input data containing
the intervention components and center characteristics.}

\item{include_center_effects}{A boolean. Specifies whether the fixed effects
should be included in the outcome model.}

\item{center_weights_for_outcome_goal}{A numeric vector. Specifies the
weights that will be used for calculating recommended interventions that
satisfy the outcome goal for an (weighted) average center.
The weights need to sum up to 1.}

\item{include_time_effects}{A boolean. Specifies whether the fixed time
effects should be included in the outcome model.}

\item{additional_covariates}{A character vector. The names of the columns in
the dataset that represent additional covariates that need to be included
in the outcome model. This includes interaction terms or any other additional
covariates.}

\item{intervention_components}{A character vector. The names of the columns
in the dataset that represent the intervention components.}

\item{include_interaction_terms}{A boolean. Specifies whether there are
interaction terms in the intervention components.}

\item{main_components}{A character vector. Specifies the main intervention
components in the presence of interaction terms.}

\item{outcome_data}{A vector. The input data containing the outcome
of interest.}

\item{fitted_model}{A glm(). The fitted glm() outcome model.}

\item{link}{A character string. The link function (e.g. "logit", "identity").}

\item{outcome_goal}{A numeric value. Specifies the outcome goal, a desired
probability or mean value.}

\item{outcome_type}{A character string. Specifies the type of the outcome.
Must be either "continuous" for continuous outcomes or "binary" for binary
outcomes.}

\item{intervention_lower_bounds}{A numeric vector. Specifies the lower bounds
of the intervention components.}

\item{intervention_upper_bounds}{A numeric vector. Specifies the upper bounds
of the intervention components.}

\item{confidence_set_grid_step_size}{A numeric vector. Specifies the step
size of the grid search algorithm used in the confidence set calculation.}

\item{center_characteristics}{A character vector. The names of the columns in
the dataset that represent the center characteristics.}

\item{center_characteristics_optimization_values}{A numeric vector. The
values of the center characteristics that will be used for LAGO optimization.}

\item{confidence_set_alpha}{A numeric value. The type I error considered in
the confidence set calculations.}

\item{cluster_id}{A list or NULL. Specifies the columns of data that will be
used as clustering effects when the "outcome_type" is continuous.}

\item{cost_list_of_vectors}{A list of numeric vectors. The cost vectors
used in the LAGO optimization.}

\item{rec_int}{A numeric vector, the recommended interventions calculated
from the optimization step.}
}
\value{
List(
  confidence_set_size_percentage = <number>,
  cs = <data.frame of interventions in the confidence set>
)
}
\description{
Internal function that calculates the confidence set
for the recommended interventions
}
