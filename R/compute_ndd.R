#' Compute the number of daily dose (ndd)
#'
#' @description
#' The function calls the doseminer r-package to extract doses from text prescription
#' and compute the (ndd) according to the given decision.
#' The general formula for computing (ndd) is given by (ndd=DF*DN/DI), where (DF) is dose
#' frequency, (DN) is dose number, and (DI) is dose interval. Prescriptions
#' can have a varible dose frequency or dose number such as '2-4 tablets'. In this case, the user can choose
#' minimum or maximum or average of value of DN or DF.
#'
#'
#' @param data a data frame object containing the prescription information.
#' The dataset is must have column names 'dosageid' and 'text'. This dataset is
#' expected to be a result of merging CPRD 'therapy' table with 'commondosage' table.
#' @param decision a character variable for computing number of (ndd).
#' \itemize{
#' \item{"min_min"}{ minimum dose number and minimum dose frequency}
#' \item{"min_mean"}{ minimum dose number and mean dose frequency}
#' \item{"min_max"}{ minimum dose number and maximum dose frequency}
#' \item{"mean_min"}{ mean dose number and minimum dose frequency}
#' \item{"mean_mean"}{ mean dose number and mean dose frequency}
#' \item{"mean_max"}{ mean dose number and maximum dose frequency}
#' \item{"max_min"}{ maximum dose number and minimum dose frequency}
#' \item{"max_mean"}{ maximum dose number and mean dose frequency}
#' \item{"max_max"}{ maximum dose number and maximum dose frequency}
#' }
#'
#' @examples
#' \dontrun{
#' compute_ndd(dataset1, "min_min")
#' }
#'
#' @return Dataframe with the same structure as the input with additional column named ndd
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export

compute_ndd <- function(data = NULL, decision = NULL) {
  # Extract dose and frequency from free text.
  temp_presc <- data %>%
    dplyr::distinct(text) %>%
    dplyr::pull(text) %>%
    doseminer::extract_from_prescription(.) %>%
    tidyr::separate(dose, c('dose1', 'dose2'),
                    sep = '-', convert = TRUE, fill = 'right') %>%
    tidyr::separate(freq, c('freq1', 'freq2'),
                    sep = '-', convert = TRUE, fill = 'right') %>%
    dplyr::mutate(
      dose2 = dplyr::coalesce(dose2, dose1),
      freq2 = dplyr::coalesce(freq2, freq1),
      DF_mean = (freq1 + freq2) / 2,
      DN_mean = (dose1 + dose2) / 2,
      DF_min = freq1, DF_max = freq2,
      DN_min = dose1, DN_max = dose2) %>%
    dplyr::select(text = raw, optional, DF_mean:DN_max) %>%
    dplyr::right_join(data, by = 'text')

  decision <- match.arg(decision, c('min_min', 'min_mean', 'min_max',
                                    'mean_min', 'mean_mean', 'mean_max',
                                    'max_min', 'max_mean', 'max_max'))

  # Decide how to compute numeric daily dose.
  temp_presc %>%
    dplyr::mutate(ndd = switch(decision,
                               min_min  = DN_min * DF_min,
                               min_mean = DN_min * DF_max,
                               min_max  = DN_min * DF_max,
                               mean_min = DN_mean * DF_min,
                               mean_mean = DN_mean * DF_mean,
                               mean_max = DN_mean * DF_max,
                               max_min  = DN_max * DF_min,
                               max_mean = DN_max * DF_mean,
                               max_max  = DN_max * DF_max)) %>%
    dplyr::select(-matches('ndd\\d|^DN|^DF')) # get rid of DN_x, DF_x and ndd# cols
}
