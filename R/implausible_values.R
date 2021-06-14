#' Define Implausible
#'
#' This function flag quantity and ndd informations that are beyond the acceptable levels in the dataset.
#' The acceptable level is obtained from NICE BNF.
#'
#' @param data a data frame object containing the prescription information
#' @param min_max_dat a data frame object containing minimum and maximum possible values for each drug
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @return Dataframe with the same structure as the input.
#'
#' @export

Implausible_values <- function(data = NULL, min_max_dat = NULL) {
  if (is.null(data)) {
    stop("\n Data set with information from therapy file of CPRD has to be supplimented")
  }
  if (is.null(min_max_dat)) {
    stop("\n Data set with information including qty_max, qty_min, min_rec_ndd, max_rec_ndd to be supplimented")
  }

  data %>%
    dplyr::left_join(min_max_dat, by = "prodcode")
    dplyr::mutate(
      implausible_qty = (qty > qty_max | qty < qty_min) & !is.na(qty),
      implausible_ndd = (ndd > max_rec_ndd | ndd < min_rec_ndd) & !is.na(ndd)
    ) %>%
    dplyr::select(-c(qty_max, qty_min, max_rec_ndd, min_rec_ndd))
}
