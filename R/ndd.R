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
#' @importFrom rlang .data
#' @export
compute_ndd <- function(data, decision) {
  decision <- match.arg(decision, c('min_min', 'min_mean', 'min_max',
                                    'mean_min', 'mean_mean', 'mean_max',
                                    'max_min', 'max_mean', 'max_max'))

  # Extract dose and frequency from free text.
  temp_presc <- data %>%
    dplyr::distinct(.data$text) %>%
    dplyr::pull(.data$text) %>%
    doseminer::extract_from_prescription() %>%
    tidyr::separate(.data$dose, c('dose1', 'dose2'),
                    sep = '-', convert = TRUE, fill = 'right') %>%
    tidyr::separate(.data$freq, c('freq1', 'freq2'),
                    sep = '-', convert = TRUE, fill = 'right') %>%
    dplyr::mutate(
      dose2 = dplyr::coalesce(.data$dose2, .data$dose1),
      freq2 = dplyr::coalesce(.data$freq2, .data$freq1),
      DF_mean = (.data$freq1 + .data$freq2) / 2,
      DN_mean = (.data$dose1 + .data$dose2) / 2,
      DF_min = .data$freq1, DF_max = .data$freq2,
      DN_min = .data$dose1, DN_max = .data$dose2) %>%
    dplyr::select(text = .data$raw, .data$optional, .data$DF_mean:.data$DN_max) %>%
    dplyr::right_join(data, by = 'text')

  # Decide how to compute numeric daily dose.
  temp_presc %>%
    dplyr::mutate(ndd = switch(decision,
                               min_min  = .data$DN_min * .data$DF_min,
                               min_mean = .data$DN_min * .data$DF_max,
                               min_max  = .data$DN_min * .data$DF_max,
                               mean_min = .data$DN_mean * .data$DF_min,
                               mean_mean = .data$DN_mean * .data$DF_mean,
                               mean_max = .data$DN_mean * .data$DF_max,
                               max_min  = .data$DN_max * .data$DF_min,
                               max_mean = .data$DN_max * .data$DF_mean,
                               max_max  = .data$DN_max * .data$DF_max)) %>%
    dplyr::select(-matches('ndd\\d|^DN|^DF'))
}
