#' Define Implausible
#'
#' This function flag quantity and ndd informations that are beyond the acceptable levels in the dataset.
#' The acceptable level is obtained from NICE BNF.
#'
#' @param dataset1 a data frame object containing the prescription information
#' @param min_max_dat a data frame object containing minimum and maximum possible values for each drug
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @return Dataframe with the same structure as the input.
#'
#' @export

Implausible_values <- function(dataset1 = NULL, min_max_dat = NULL) {
  if (is.null(dataset1)) {
    stop("\n Data set with information from therapy file of CPRD has to be supplimented")
  }
  if (is.null(min_max_dat)) {
    stop("\n Data set with information including qty_max, qty_min, min_rec_ndd, max_rec_ndd to be supplimented")
  }

  dataset1 <- dplyr::left_join(dataset1, min_max_dat, by = "prodcode")
  dataset1 <- dataset1 %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      implausible_qty = (qty > qty_max | qty < qty_min) & !is.na(qty),
      implausible_ndd = (ndd > max_rec_ndd | ndd < min_rec_ndd) & !is.na(ndd)
    ) %>%
    dplyr::ungroup()

  return(dataset1)
}
