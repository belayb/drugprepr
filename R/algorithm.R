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
#' @family \code{\link{drug_prep}} decision functions
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

#' Decision 1: impute implausible total quantities
#'
#' A light wrapper around \code{\link{impute_qty}}.

#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#' \item{"a"}{do nothing; leave implausible values as-is}
#' \item{"b"}{set implausible values to missing}
#' \item{"c1"}{set to mean for individual's prescriptions for that drug}
#' \item{"c2"}{set to mean for practice's prescriptions for that drug}
#' \item{"c3"}{set to mean for populations's prescriptions for that drug}
#' \item{"d1"}{set to median for individual's prescriptions for that drug}
#' \item{"d2"}{set to median for practice's prescriptions for that drug}
#' \item{"d3"}{set to median for population's prescriptions for that drug}
#' \item{"e1"}{set to mode for individual's prescriptions for that drug}
#' \item{"e2"}{set to mode for practice's prescriptions for that drug}
#' \item{"e3"}{set to mode for population's prescriptions for that drug}
#' \item{"f1"}{use value of individual's next prescription}
#' \item{"f2"}{use value of practice's next prescription}
#' \item{"f3"}{use value of population's next prescription}
#' \item{"g1"}{use value of individual's previous prescription}
#' \item{"g2"}{use value of practice's previous prescription}
#' \item{"g3"}{use value of population's previous prescription}
#' }
#'
#' @note Decisions \code{f} and \code{g} are not yet implemented.
#'
#' @family \code{\link{drug_prep}} decision functions
#'
decision_1 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', 'b', paste0(rep(letters[3:7], each = 3), 1:3)))
  impute_qty(data,
             method = get_implausible_method(decision),
             where = function(q) q < data[['min_qty']] | q > data[['max_qty']],
             group = get_decision_group(decision),
             replace_with = NA_real_)
}

#' Decision 2: impute missing total quantities
#'
#' A light wrapper around \code{\link{impute_qty}}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#' \item{"a"}{Leave as missing (implicitly drop this prescription)}
#' \item{"b1"}{set to mean for individual's prescriptions for that drug}
#' \item{"b2"}{set to mean for practice's prescriptions for that drug}
#' \item{"b3"}{set to mean for populations's prescriptions for that drug}
#' \item{"c1"}{set to median for individual's prescriptions for that drug}
#' \item{"c2"}{set to median for practice's prescriptions for that drug}
#' \item{"c3"}{set to median for population's prescriptions for that drug}
#' \item{"d1"}{set to mode for individual's prescriptions for that drug}
#' \item{"d2"}{set to mode for practice's prescriptions for that drug}
#' \item{"d3"}{set to mode for population's prescriptions for that drug}
#' \item{"e1"}{use value of individual's next prescription}
#' \item{"e2"}{use value of practice's next prescription}
#' \item{"e3"}{use value of population's next prescription}
#' \item{"f1"}{use value of individual's previous prescription}
#' \item{"f2"}{use value of practice's previous prescription}
#' \item{"f3"}{use value of population's previous prescription}
#' }
#'
#' @note Decisions \code{e} and \code{f} are not yet implemented.
#'
#' @family \code{\link{drug_prep}} decision functions
#'
decision_2 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', paste0(rep(letters[2:6], each = 3), 1:3)))
  impute_qty(data,
             method = get_missing_method(decision),
             where = is.na,
             group = get_decision_group(decision))
}

#' Decision 3: impute implausible daily doses
#'
#' A light wrapper around \code{\link{impute_ndd}}.
#'
#' @inheritParams decision_1
#'
#' @note Decisions \code{f} and \code{g} are not yet implemented.
#'
#' @family \code{\link{drug_prep}} decision functions
#'
decision_3 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', 'b', paste0(rep(letters[3:7], each = 3), 1:3)))
  impute_ndd(data,
             method = get_implausible_method(decision),
             where = function(n) n < data[['min_ndd']] | n > data[['max_ndd']],
             group = get_decision_group(decision),
             replace_with = NA_real_)
}

#' Decision 4: impute missing daily doses
#'
#' A light wrapper around \code{\link{impute_ndd}}.
#'
#' @inheritParams decision_2
#'
#' @note Decisions \code{e} and \code{f} are not yet implemented.
#'
#' @family \code{\link{drug_prep}} decision functions
#'
decision_4 <- function(data, decision = 'a') {
  decision <- match.arg(decision,
                        c('a', paste0(rep(letters[2:6], each = 3), 1:3)))
  impute_ndd(data,
             method = get_missing_method(decision),
             where = is.na,
             group = get_decision_group(decision))
}

#' Decision 5: impute implausible prescription durations
#'
#' A light wrapper around \code{\link{clean_duration}}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#'   \item{"a"}{leave duration as-is}
#' 		\item{"b_6"}{set to missing if > 6 months}
#' 		\item{"b_12"}{set to missing if > 12 months}
#' 		\item{"b_24"}{set to missing if > 24 months}
#' 		\item{"c_6"}{set to 6 months if > 6 months}
#' 		\item{"c_12"}{set to 12 months if > 12 months}
#' 		\item{"c_24"}{set to 24 months if > 24 months}
#' }
#'
#' @family \code{\link{drug_prep}} decision functions
#'
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

#' Decision 6: choose method of calculating prescription duration
#'
#' This is just shorthand for defining a column equal to one of the specified
#' formulae. If the column(s) corresponding to \code{decision} are missing, an
#' error will be thrown.
#' If you have already calculated or obtained the column \code{duration} from
#' elsewhere, this step is not necessary.
#'
#' @note This step actually takes place \emph{before} \code{\link{decision_5}}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#' \item{"a"}{\code{numdays}}
#' 	 \item{"b"}{\code{dose_duration}}
#' 	 \item{"c"}{\code{qty / ndd}}
#' }
#'
#' @family \code{\link{drug_prep}} decision functions
#'
decision_6 <- function(data, decision = 'c') {
  decision <- match.arg(decision, letters[1:3])
  transform(data, duration = switch(decision,
                                    a = numdays,
                                    b = dose_duration,
                                    c = qty / ndd))
}

#' Decision 7: impute missing prescription durations
#'
#' A light wrapper around \code{\link{impute_duration}}.
#'
#' @param data a data frame
#' @param decision one  of the following strings:
#' \describe{
#' \item{"a"}{Leave missing durations as-is (implicitly drop the prescription)}
#' \item{"b"}{Use mean prescription duration for that drug, for that individual}
#' \item{"c"}{Use mean prescription duration for that drug, for the population}
#' \item{"d"}{Use individual mean duration; if not available use population mean}
#' }
#'
#' @family \code{\link{drug_prep}} decision functions
#'
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

#' Decision 8: disambiguate prescriptions with the same start date
#'
#' A light wrapper around \code{\link{impute_duration}}, followed by removing
#' duplicate rows with the same combination of \code{prodcode}, \code{patid}
#' and \code{start_date}.
#'
#' @param data a data frame
#' @param decision one of the following strings
#' \describe{
#' \item{"a"}{do nothing}
#'  \item{"b"}{replace with a prescription of duration equal to the mean}
#'  \item{"c"}{choose the shortest prescription}
#'  \item{"d"}{choose longest prescription}
#'  \item{"e"}{replace with a prescription of duration equal to the sum}
#' }
#'
#' @family \code{\link{drug_prep}} decision functions
#'
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

#' Decision 9: handle overlapping prescription periods
#'
#' In situations where one prescription starts before another (for the same
#' patient and drug) finishes, this function will either implicitly sum the
#' doses (i.e. do nothing) or it will divide the intervals into non-overlapping
#' subsets, shifting these sub-intervals forward in time until there is no
#' overlap.
#'
#' The underlying algorithm for shifting overlapping intervals is implemented
#' by the internal function \code{shift_interval}.
#'
#' @param data a data frame
#' @param decision one of the following strings:
##' \describe{
##'  \item{"a"}{allow overlapping prescriptions (implicitly sum doses)}
##'  \item{"b"}{move later prescription to next available time that this product is not prescribed}
##' }
#'
#' @family \code{\link{drug_prep}} decision functions
#'
#' @import dplyr
decision_9 <- function(data, decision = 'a') {
  decision <- match.arg(decision, c('a', 'b'))
  if (decision == 'a') {
    return(data)
  } else isolate_overlaps(data) %>%
    dplyr::group_by(patid, prodcode) %>%
    dplyr::arrange(start_date) %>%
    dplyr::group_modify(~ shift_interval(.x) %>%
                          dplyr::select(-patid, -prodcode),
                        .keep = TRUE) %>%
    dplyr::ungroup()
}

#' Decision 10: close small gaps between successive prescriptions
#'
#' Where one prescription (for the same drug and patient) starts only a short
#' time after the previous finishes, this function can close the gap, as if
#' the prescription was continuous over the entire period.
#'
#' The underlying function is called \code{close_small_gaps}
#'
#' @param data a data frame
#' @param decision one of the following strings:
#' \describe{
#'  \item{"a"}{do nothing}
#'  \item{"b"}{change stop date of first prescription to start date of next if gap is \eqn{\leq 15} days}
#'  \item{"c"}{change stop date of first prescription to start date of next if gap is \eqn{\leq 30} days}
#'  \item{"d"}{change stop date of first prescription to start date of next if gap is \eqn{\leq 60} days}
#' }
#'
#' @family \code{\link{drug_prep}} decision functions
#'
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

