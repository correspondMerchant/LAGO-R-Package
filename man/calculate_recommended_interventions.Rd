% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_recommended_interventions.R
\name{calculate_recommended_interventions}
\alias{calculate_recommended_interventions}
\title{calculate_recommended_interventions}
\usage{
calculate_recommended_interventions(
  df,
  outcome_name,
  outcome_type,
  glm_family,
  link = "logit",
  include_intercept = TRUE,
  interventions_list,
  center_characteristic_list = NULL,
  center_characteristic_list_for_optimization = NULL,
  intervention_lower_bounds,
  intervention_upper_bounds,
  cost_list_of_lists,
  outcome_goal,
  outcome_goal_optimization = "numerical",
  grid_search_step_size = 0.01,
  include_confidence_set = FALSE,
  confidence_set_step_size = NULL,
  confidence_set_alpha = 0.05
)
}
\arguments{
\item{df}{data.frame, Input data}

\item{outcome_name}{character, Outcome name}

\item{outcome_type}{character, Outcome type, either "continuous" or "binary"}

\item{glm_family}{character, Family for the GLM model
For example: "binomial"
For v1.1, we are only considering binomial family, and quasibinomial(link="logit")}

\item{link}{character, the link option when glm_family is set to quasibinomial.
It has to be one of the following:
logit (default), probit, cauchit, log and cloglog.}

\item{include_intercept}{boolean, Whether the intercept should be included in the
fitted model.}

\item{interventions_list}{character vector, Names of the intervention components
For example: c("component1", "component2")}

\item{center_characteristic_list}{character vector, Names of the center characteristics
For example: c("characteristic1")}

\item{center_characteristic_list_for_optimization}{numeric vector, Given values of
the center characteristics
For example: c(1.74)}

\item{intervention_lower_bounds}{numeric vector, Lower bounds for the intervention components
For example: c(0,0)}

\item{intervention_upper_bounds}{numeric vector, Upper bounds for the intervention components
For example: c(10,20)}

\item{cost_list_of_lists}{list, A nested list structure defining cost functions for the intervention components.
Each sublist represents a component and contains coefficients for its cost function.
The position of each coefficient in the sublist corresponds to the power of x in the polynomial cost function.
For example:
list(c(1, 2), c(4), c(5, 4, 3)) represents:
- First component: cost = 1x + 2x^2
- Second component: cost = 4x
- Third component: cost = 5x + 4x^2 + 3x^3
Empty sublists are not allowed. Each component must have at least one coefficient.}

\item{outcome_goal}{numeric, The outcome goal}

\item{outcome_goal_optimization}{character, Method used behind the scenes for
calculating the recommended interventions. Either "numerical" or "grid_search"
For v1, this does not matter since we work with linear cost functions and
we only have one way of solving for the recommended interventions}

\item{grid_search_step_size}{numeric, step size for the grid search algorithm,
default value is 0.01 for each intervention component.}

\item{include_confidence_set}{boolean, Whether the confidence set should be
calculated for the recommended intervention.}

\item{confidence_set_step_size}{numeric vector, Step size for the grid search
algorithm used in the confidence set calculations.}

\item{confidence_set_alpha}{numeric, Type I error of the confidence set.}
}
\value{
List(recommended interventions, associated cost for the interventions,
estimated outcome mean/probability for the intervention group in the next stage)
}
\description{
Returns the LAGO recommended interventions for the next stage.
The recommended interventions that aim to satisfy the outcome goal and/or the
power goal.
}
\examples{
calculate_recommended_interventions(
  df = infert,
  outcome_name = "case",
  outcome_type = "binary",
  glm_family = "binomial",
  interventions_list = c("age", "parity"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(50, 10),
  cost_list_of_lists = list(c(4), c(1)),
  outcome_goal = 0.5
)

calculate_recommended_interventions(
  df = infert,
  outcome_name = "case",
  outcome_type = "binary",
  glm_family = "binomial",
  interventions_list = c("age", "parity"),
  intervention_lower_bounds = c(0, 0),
  intervention_upper_bounds = c(50, 10),
  cost_list_of_lists = list(c(4), c(1)),
  outcome_goal = 0.5,
  include_confidence_set = TRUE,
  confidence_set_step_size = c(1, 1)
)

}
