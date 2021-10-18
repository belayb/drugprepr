#' Run drug preparation algorithm
#'
#' @param data data frame containing prescription data
#' @param plausible_values data frame containing variables \code{prodcode},
#' \code{min_qty}, \code{max_qty}, \code{min_ndd}, \code{max_ndd} describing
#' plausible ranges for values for each drug
#' @param decisions character vector of length 10
#'
#' @examples
#' plausible_values <- data.frame(
#'   prodcode = c('a', 'b', 'c'),
#'   min_qty = 0,
#'   max_qty = c(50, 100, 200),
#'   min_ndd = 0,
#'   max_ndd = c(10, 20, 30)
#' )
#' drug_prep(example_therapy,
#'           plausible_values,
#'           decisions = c('a', 'a', 'a', 'a', 'a',
#'                         'c', 'a', 'a', 'a', 'a'))
#'
#' @import dplyr
#' @export
drug_prep <- function(data,
                      plausible_values,
                      decisions = rep('a', 10)) {
  data <- dplyr::left_join(data, plausible_values, by = 'prodcode')
  # Implausible quantities
  data <- decision_1(data, decisions[1]) %>% dplyr::select(-min_qty, -max_qty)
  # Missing quantities
  data <- decision_2(data, decisions[2])
  # Implausible numerical daily doses
  data <- decision_3(data, decisions[3]) %>% dplyr::select(-min_ndd, -max_ndd)
  # Missing numerical daily doses
  data <- decision_4(data, decisions[4])
  # Choose method for calculating prescription duration (note switched order)
  data <- decision_6(data, decisions[6])
  # Truncate/delete overly long prescription durations
  data <- decision_5(data, decisions[5])
  # Impute missing prescription durations
  data <- decision_7(data, decisions[7])
  # Disambiguate prescriptions with the same start_date
  data <- decision_8(data, decisions[8])
  # Compute stop_date from duration
  data <- transform(data, stop_date = start_date + duration)
  # Disambiguate overlapping prescription intervals
  data <- decision_9(data, decisions[9])
  # Close short gaps between successive prescriptions
  data <- decision_10(data, decisions[10])
  return(data)
}

decision_1 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', 'b', paste0(rep(letters[3:7], each = 3), 1:3)))
  impute_qty(data,
             method = get_implausible_method(decision),
             where = function(q) q < data[['min_qty']] | q > data[['max_qty']],
             group = get_decision_group(decision),
             replace_with = NA_real_)
}

decision_2 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', paste0(rep(letters[2:6], each = 3), 1:3)))
  impute_qty(data,
             method = get_missing_method(decision),
             where = is.na,
             group = get_decision_group(decision))
}

decision_3 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', 'b', paste0(rep(letters[3:7], each = 3), 1:3)))
  impute_ndd(data,
             method = get_implausible_method(decision),
             where = function(n) n < data[['min_ndd']] | n > data[['max_ndd']],
             group = get_decision_group(decision),
             replace_with = NA_real_)
}

decision_4 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', paste0(rep(letters[2:6], each = 3), 1:3)))
  impute_ndd(data,
             method = get_missing_method(decision),
             where = is.na,
             group = get_decision_group(decision))
}

decision_5 <- function(data, decision = 'a') {
  decision <- match.arg(decision, c('a',
                                    paste(rep(c('b', 'c'), each = 3),
                                          c(6, 12, 24), sep = '_')))
  if (decision == 'a') return(data)
  clean_duration(data,
                 max_months = as.integer(substring(decision, 3)),
                 method = switch(substring(decision, 2, 2),
                                 b = 'remove',
                                 c = 'truncate'))
}

decision_6 <- function(data, decision = 'c') {
  decision <- match.arg(decision, letters[1:3])
  transform(data, duration = switch(decision,
                                    a = numdays,
                                    b = dose_duration,
                                    c = qty / ndd))
}

decision_7 <- function(data, decision = 'a') {
  decision <- match.arg(decision, letters[1:4])
  out <- impute_duration(data,
                         method = switch(decision,
                                         a = 'replace',
                                         b = 'mean',
                                         c = 'mean',
                                         d = 'mean'),
                         group = switch(decision,
                                        b = 'patid',
                                        c = 'population',
                                        d = 'patid'))
  if (decision != 'd') return(out)
  # If any individuals have no non-missing values, the previous
  # step will return duration of NaN. Run one more iteration to fill in those:
  impute_duration(out, method = 'mean', group = 'population')
}

#' @import dplyr
decision_8 <- function(data, decision = 'a') {
  decision <- match.arg(decision, letters[1:5])
  if (decision == 'a') return(data)
  impute_duration(data,
                  method = switch(decision,
                                  b = 'mean',
                                  c = 'min',
                                  d = 'max',
                                  e = 'sum'),
                  where = function(x) length(x) > 1,
                  group = c('patid', 'start_date')) %>%
    dplyr::group_by(prodcode, patid, start_date) %>%
    dplyr::slice(1L) %>% # remove duplicates
    dplyr::ungroup()
}

#' @import dplyr
decision_9 <- function(data, decision = 'a') {
  decision <- match.arg(decision, c('a', 'b'))
  if (decision == 'a') return(data)
  # else:
  isolate_overlaps(data) %>%
    dplyr::group_by(patid, prodcode) %>%
    dplyr::arrange(start_date) %>%
    dplyr::group_modify(~ shift_interval(.x) %>%
                          dplyr::select(-patid, -prodcode),
                        .keep = TRUE) %>%
    dplyr::ungroup()
}

decision_10 <- function(data, decision = 'a') {
  decision <- match.arg(decision, c('a', 'b_15', 'b_30', 'b_60'))
  min_gap <- if (decision == 'a') 0 else as.integer(substring(decision, 3))
  close_small_gaps(data, min_gap)
}

get_decision_group <- function(decision_string) {
  switch(substring(decision_string, 2, 2),
         'population',
         '1' = 'patid',
         '2' = 'pracid',
         '3' = 'population')
}

get_implausible_method <- function(decision_string) {
  switch(substring(decision_string, 1, 1),
         a = 'ignore',
         b = 'replace',
         c = 'mean',
         d = 'median',
         e = 'mode',
         stop('Decision not yet implemented'))
}

get_missing_method <- function(decision_string) {
  switch(substring(decision_string, 1, 1),
         a = 'ignore',
         b = 'mean',
         c = 'median',
         d = 'mode',
         stop('Decision not yet implemented'))
}


