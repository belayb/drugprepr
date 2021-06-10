#' Example CPRD data.
#'
#' A dataset containing prescription information for two individuals.
#' The dataset is a hypothetical data resembling the real CPRD data.
#'
#' @format A data frame with 18 rows and 9 variables:
#' \describe{
#'   \item{patid}{unique identifier given to a patient in CPRD GOLD}
#'   \item{pracid}{unique identifier given to a practice in CPRD GOLD}
#'   \item{event_date}{Date associated with the event, as entered by the GP}
#'   \item{prodcode}{CPRD unique code for the treatment selected by the GP}
#'   \item{dossageid}{Identifier that allows dosage information on the event to be retrieved from Common Dosages Lookup table}
#'   \item{text}{Prescription instraction for the prescribed product, as entered by the GP}
#'   \item{qty}{Total quantity entered by the GP for the prescribed product}
#'   \item{numdays}{Number of treatment days prescribed for a specific therapy event}
#'   \item{dose_duration}{an estimated prescription duration, as entered by CPRD}
#'   ...
#' }
#' @source \url{https://cprdcw.cprd.com/_docs/CPRD_GOLD_Full_Data_Specification_v2.0.pdf}
"dataset1"
