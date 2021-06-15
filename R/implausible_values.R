#' Check plausibility of doses and medication quantities
#'
#' This function flags quantity and ndd data that are outside specified acceptable ranges.
#' The acceptable level may be obtained from NICE BNF, or provided manually.
#'
#' @param data a data frame object containing the prescription information
#' @param min_max_dat a data frame object containing minimum and maximum possible values for each drug
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate select
#'
#' @return A data frame with the same structure as the input, plus two columns:
#' \itemize{
#' \item{\code{implausible_qty}}
#' \item{\code{implausible_ndd}}
#' },
#' both logical vectors indicating whether, respectively, the drug quantity or
#' numeric daily dose are outside the plausible ranges given by \code{min_max_dat}.
#' If there is no \code{prodcode} match to \code{min_max_dat}, plausibility is
#' given as \code{NA}.
#'
#' @export
check_implausible <- function(data = NULL, min_max_dat = NULL) {
  if (is.null(data)) {
    stop("\n Data set with information from therapy file of CPRD has to be supplimented")
  }
  if (is.null(min_max_dat)) {
    stop("\n Data set with information including qty_max, qty_min, min_rec_ndd, max_rec_ndd to be supplimented")
  }

  data %>%
    dplyr::left_join(min_max_dat, by = "prodcode") %>%
    dplyr::mutate(
      implausible_qty = (qty > qty_max | qty < qty_min) & !is.na(qty),
      implausible_ndd = (ndd > max_rec_ndd | ndd < min_rec_ndd) & !is.na(ndd)
    ) %>%
    dplyr::select(-c(qty_max, qty_min, max_rec_ndd, min_rec_ndd))
}
