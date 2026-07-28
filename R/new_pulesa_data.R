#' PULESA hypertension-care data (new model)
#'
#' Clinic-level data from the PULESA study, a cluster-randomized trial of a
#' multi-component intervention to improve hypertension care. Each row is one
#' clinic in one time period. This is the version used to fit the "new" LAGO
#' outcome model, which uses the same clinics and periods as
#' \code{\link{main_pulesa_data}} but a different specification of the
#' intervention (the \code{W1}--\code{W8} components and their interaction
#' terms).
#'
#' @format A data frame with 192 rows and 22 variables:
#' \describe{
#'   \item{Clinic}{Clinic (cluster) identifier}
#'   \item{Period}{Study time period}
#'   \item{AccessBPMachines}{Intervention component: access to blood-pressure machines}
#'   \item{AccessMedicines}{Intervention component: access to medicines}
#'   \item{DeliveryA}{Intervention component: care delivery mode A}
#'   \item{DeliveryB}{Intervention component: care delivery mode B}
#'   \item{HypertensionTraining}{Intervention component: hypertension training}
#'   \item{MI_W5}{Interaction term involving component W5}
#'   \item{MI_W7}{Interaction term involving component W7}
#'   \item{MI_W8}{Interaction term involving component W8}
#'   \item{PerformanceImprovement}{Intervention component: performance improvement}
#'   \item{RemoteMonitoring}{Intervention component: remote monitoring}
#'   \item{Success}{Outcome: number of successes at the clinic-period}
#'   \item{Total_visit}{Number of visits at the clinic-period}
#'   \item{W1}{Intervention component W1}
#'   \item{W2}{Intervention component W2}
#'   \item{W3}{Intervention component W3}
#'   \item{W4}{Intervention component W4}
#'   \item{W5}{Intervention component W5}
#'   \item{W6}{Intervention component W6}
#'   \item{W7}{Intervention component W7}
#'   \item{W8}{Intervention component W8}
#' }
#'
#' @examples
#' data(new_pulesa_data)
#' head(new_pulesa_data)
"new_pulesa_data"
