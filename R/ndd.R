#' Compute the numerical daily dose from free text
#'
#' The function calls the \R package \strong{doseminer} to extract dose
#' information from free-text prescribing instructions, then computes the
#' average numerical daily dose according to a given decision rule.
#'
#' The general formula for computing numerical daily dose (ndd) is given by
#' \deqn{\mbox{ndd} = \mbox{DF} \times \mbox{DN} / \mbox{DI},}{ndd = DF * DN / DI,}
#' where
#' \describe{
#' \item{DF}{is dose frequency, the number of dose 'events' per day}
#' \item{DN}{is dose number, or number of units of drug taken during each dose 'event'}
#' \item{DI}{is dose interval, or the number of days between 'dose days', where an interval of 1 means every day}
#' }
#' Prescriptions can have a variable dose frequency or dose number, such as
#' '2-4 tablets up to 3 times per day'. In this case, the user can choose
#' to reduce these ranges to single values by taking the minimum, maximum or
#' average of these endpoints.
#'
#' @param data a data frame containing free-text prescribing instructions in a
#' column called \code{text}.
#' @param decision one of the following strings:
#' \describe{
#' \item{"min_min"}{minimum dose number and minimum dose frequency}
#' \item{"min_mean"}{minimum dose number and mean dose frequency}
#' \item{"min_max"}{minimum dose number and maximum dose frequency}
#' \item{"mean_min"}{mean dose number and minimum dose frequency}
#' \item{"mean_mean"}{mean dose number and mean dose frequency}
#' \item{"mean_max"}{mean dose number and maximum dose frequency}
#' \item{"max_min"}{maximum dose number and minimum dose frequency}
#' \item{"max_mean"}{maximum dose number and mean dose frequency}
#' \item{"max_max"}{maximum dose number and maximum dose frequency}
#' }
#'
#' @examples
#' compute_ndd(dataset1, "min_min")
#'
#' @return
#' A data frame mapping the raw \code{text} to structured dosage information.
#'
#' @import dplyr
#' @export
compute_ndd <- function(data, decision) {
  decision <- match.arg(decision, c('min_min', 'min_mean', 'min_max',
                                    'mean_min', 'mean_mean', 'mean_max',
                                    'max_min', 'max_mean', 'max_max'))

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
    dplyr::select(-matches('ndd\\d|^DN|^DF'))
}
