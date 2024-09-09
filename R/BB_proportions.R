#' BetterBirth Data (for continuous outcome analysis, using EBP proportions as a continuous outcome).
#'
#' This is the cleaned version of the BetterBirth dataset used in Bing et al.
#'
#' @format A data frame with 7359 rows and 4 variables:
#' \describe{
#'   \item{coaching_updt}{Number of coaching visits accrued (at time of birth)}
#'   \item{launch_duration}{Duration of checklist launch}
#'   \item{birth_volume_100}{Estimated monthly birth volume (divided by 100)}
#'   \item{EBP_proportions}{Essential birth practices performed out of
#'   all possible birth practices measured. We consider this as a continuous outcome.}
#' }
#' @source \url{https://arxiv.org/pdf/2307.06552}
#'
#' @examples
#' data(BB_proportions)
#' head(BB_proportions)
"BB_proportions"
