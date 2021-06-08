#' Example min-max data.
#'
#' A dataset containing minimum and maximum possible values for quantity and
#' number of daily dose for given prescription.
#' The dataset is a hypothetical data.
#'
#' @format A data frame with 2 rows and 5 variables:
#' \describe{
#'   \item{prodcode}{CPRD unique code for the treatment selected by the GP}
#'   \item{qty_max}{maximum possible quantity to be prescribed for the product}
#'   \item{qty_min}{minimum possible quantity to be prescribed for the product}
#'   \item{max_rec_ndd}{maximum possible number of daily dose to be prescribed for the product}
#'   \item{min_rec_ndd}{minimum possible number of daily dose  to be prescribed for the product}
#'   ...
#' }
"min_max_dat"
