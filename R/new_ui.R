#' Impute missing or implausible values
#'
#' This is a workhorse function used by \code{\link{impute_ndd}},
#' \code{\link{impute_qty}} and others.
#'
#' The argument \code{where} indicates which values are to be imputed.
#' It can be specified as either a vector or as a function. Thus you can
#' specify, for example, \code{\link{is.na}} to impute all missing values, or
#' you can pass in a vector, if it depends on something else rather than just
#' the current values of the variable to imputed.
#' This design may change in future. In particular, if we want to impute
#' implausible values and impute missing values separately, it's important that
#' these steps are independent.
#'
#' @import dplyr
#' @param data A data frame containing columns \code{prodcode}, \code{pracid}, \code{patid}
#' @param variable Unquoted name of the column in \code{dataset} to be imputed
#' @param method Method for imputing the values. See details.
#' @param where Logical vector, or function applied to \code{variable} returning such a vector, indicating which elements to impute. Defaults to \code{\link[base:NA]{is.na}}
#' @param group Level of structure for imputation. Defaults to whole study population.
#' @param ... Extra arguments, currently ignored
#' @param replace_with if the method 'replace' is selected, which value should be inserted?
#'
#' \itemize{
#' \item \code{ignore}. Do nothing, leaving input unchanged.
#' \item \code{mean}. Replace values with the mean by \code{group}
#' \item \code{median}. Replace values with the median by \code{group}
#' \item \code{mode}. Replace values with the most common value by \code{group}
#' \item \code{replace}. Replace values with \code{replace_with}, which defaults to \code{NA} (i.e. mark as missing)
#' \item \code{min}. Replace with minimum value.
#' \item \code{max}. Replace with maximum value.
#' \item \code{sum}. Replace with sum of values.
#' }
#'
#' @return A data frame of the same structure as \code{data}, with values imputed
#' @export
impute <- function(data,
                   variable,
                   method = c('ignore', 'mean', 'median', 'mode', 'replace',
                              'min', 'max', 'sum'),
                   where = is.na,
                   group,
                   ...,
                   replace_with = NA_real_) {

  if (length(replace_with) != 1)
    stop('replace_with must be a scalar value')

  if (!is.numeric(replace_with))
    stop('replace_with should be numeric, but input was ', class(replace_with))

  where_fn <- if (is.function(where)) where else function(x) where
  method <- match.arg(method)

  impute_fun <- switch(method,
                       'ignore' = identity,
                       'replace' = function(x) replace(x, TRUE, replace_with),
                       'mean' = function(x) mean(x, na.rm = TRUE),
                       'median' = function(x) median(x, na.rm = TRUE),
                       'mode' = get_mode,
                       'min' = function(x) min(x, na.rm = TRUE),
                       'max' = function(x) max(x, na.rm = TRUE),
                       'sum' = function(x) sum(x, na.rm = TRUE))

  group_vars <- if (method %in% c('ignore', 'replace')) NULL else
    c('prodcode', if (group[1] == 'population') NULL else
      match.arg(group, colnames(data), several.ok = TRUE))

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate("{{variable}}" := ifelse(no   = {{ variable }},
                                           test = rep_len(where_fn({{ variable }}), length({{ variable }})),
                                           yes  = impute_fun({{ variable }}))) %>%
    dplyr::ungroup()
}

#' Find implausible entries

#' Replace implausible or missing prescription quantities
#'
#' @inheritParams impute
#'
#' @examples
#' impute_qty(example_therapy, 'mean')
#'
#' @export
impute_qty <- function(data,
                       method,
                       where = is.na,
                       group,
                       ...) {
  impute(data, qty, method, where, group, ...)
}

#' Replace implausible or missing numerical daily doses (NDD)
#'
#' @inheritParams impute
#'
#' @examples
#' impute_ndd(example_therapy, 'mean')
#'
#' @export
impute_ndd <- function(data,
                       method,
                       where = is.na,
                       group,
                       ...) {
  impute(data, ndd, method, where, group, ...)
}


#' Replace missing or implausible prescription durations
#'
#' Instead of replacing missing stop dates, we impute the durations and then
#' infer the stop dates from there.
#'
#' We can fix clashing start dates by setting \code{group} to \code{start_date}
#' and \code{patid}, i.e. average over groups with more than one member;
#' any metric should return the original values if the group size is one.
#'
#' @inheritParams impute
#'
#' @examples
#' example_duration <- transform(example_therapy, duration = qty / ndd)
#' impute_duration(example_duration, method = 'mean', group = 'patid')
#'
#' @export
impute_duration <- function(data,
                            method,
                            where = is.na,
                            group = c('patid', 'start_date'),
                            ...) {
  impute(data, duration, method, where, group, ...)
}

#' Clean implausibly-long prescription durations
#'
#' Given a prescription length limit, truncate any prescriptions that appear to
#' be longer than this, or mark them as missing.
#'
#' The method 'truncate' causes any duration longer than \code{max_months} to
#' be replaced with the value of \code{max_months} (albeit converted to days).
#' The method 'remove' causes such durations to be replaced with \code{NA}.
#' There is no explicit 'ignore' method, but if you want to 'do nothing', simply
#' set \code{max_months} to an arbitrarily high number.
#' By default, the maximum is infinite, so nothing should happen.
#' (Of course, you could also just \emph{not} run the function...)
#'
#' @note Currently the variable name is hard-coded as 'duration', but in
#' principle this could be parametrised for datasets where the column has a
#' different name.
#'
#' @param data A data frame containing a column called \code{duration}
#' @param max_months The maximum plausible prescription length in months
#' @param method Either 'truncate' or 'remove'. See details
#'
#' @examples
#' long_presc <- data.frame(duration = c(100, 300, 400, 800))
#' clean_duration(long_presc, 6)
#' clean_duration(long_presc, 12, 'remove')
#'
#' @export
clean_duration <- function(data,
                           max_months = Inf,
                           method = c('truncate', 'remove')) {

  stopifnot(length(max_months) == 1 & is.numeric(max_months))
  method <- match.arg(method)
  max_days <- round(max_months * 365 / 12)
  impute(data, duration,
         method = 'replace',
         where = function(x) x > max_days,
         replace_with = switch(method, truncate = max_days, remove = NA_real_))
}

fix_start_clash <- function(data) {

}

#' New way of separating overlapping prescriptions
#'
#' Run this function and then you can either simply discard overlapping
#' intervals or shift them around using an appropriate algorithm.
#'
#' The older implementation used \code{isolateoverlaps} from the
#' \code{intervalaverage} package and \code{Overlap} from the \code{DescTools}
#' package. Here we refactor it using functions from \code{tidyverse} instead.
#'
#' @param data A data frame including variables \code{patid}, \code{start_date},
#' \code{stop_date} and \code{prodcode}
#'
#' @return A data frame of \code{patid}, \code{prodcode}, \code{start_date} and
#' \code{stop_date}, where intervals are either exactly overlapping or mutually
#' non-overlapping (but not partially overlapping), such that the union of such
#' intervals is equivalent to those originally provided in \code{data}
#'
#' @examples
#' set.seed(1)
#' overlapping_data <- data.frame(
#'   rowid = 1:20,
#'   patid = 1:2,
#'   prodcode = 'a',
#'   start_date = Sys.Date() + c(round(rexp(19, 1/7)), -20),
#'   qty = rpois(20, 64),
#'   ndd = sample(seq(.5, 12, by = .5), 20, replace = TRUE),
#'   stringsAsFactors = FALSE
#' )
#' overlapping_data <- transform(overlapping_data,
#'   stop_date = start_date + qty / ndd
#' )
#' isolate_overlaps(overlapping_data)
#'
#' @note
#' This function currently doesn't use any keys except \code{patid} and
#' \code{prodcode}. It may be desirable to add a row ID, for matching each
#' partial interval back to the original interval from which it was derived.
#' This may be relevant to models using weighted dosages.
#'
#' @seealso
#' \code{\link[intervalaverage]{isolateoverlaps}},
#' \code{\link[data.table]{foverlaps}}
#'
#' @import dplyr tidyr
#' @importFrom sqldf sqldf
#' @export
isolate_overlaps <- function(data) {
  data %>%
    # melt
    tidyr::pivot_longer(c(start_date, stop_date),
                        names_to = 'date_name',
                        values_to = 'date_value') %>%
    # setorderv
    dplyr::arrange(patid, prodcode, date_value, date_name) %>%
    dplyr::group_by(patid, prodcode) %>%
    # shift
    dplyr::transmute(date_value,
                     is_end_var = date_name == 'stop_date',
                     end_next_var = dplyr::lead(is_end_var),
                     value_next_var = dplyr::lead(date_value)) %>%
    dplyr::filter(!is.na(end_next_var)) %>%
    # temp
    dplyr::transmute(start_date = dplyr::if_else(!is_end_var,
                                                 date_value,
                                                 date_value + 1L),
                     stop_date = dplyr::if_else(!end_next_var,
                                                value_next_var - 1L,
                                                value_next_var)) %>%
    dplyr::filter(stop_date >= start_date) -> temp

  # foverlaps / overlap join
  out <- sqldf::sqldf(
    'SELECT x.patid, x.prodcode, --x.rowid, y.rowid yrowid,
            y.start_date, y.stop_date
     FROM data x
     JOIN temp y
     ON (x.start_date BETWEEN y.start_date AND y.stop_date) OR
        (x.stop_date BETWEEN y.start_date AND y.stop_date) OR
        (x.start_date < y.start_date AND x.stop_date > y.stop_date) OR
        (x.start_date > y.start_date AND x.stop_date < y.stop_date)
     WHERE x.patid = y.patid AND x.prodcode = y.prodcode'
  )

   return(out)
}

#' Close small gaps between successive prescriptions
#'
#' Given a series of prescriptions in \code{data}, if one prescription
#' (for the same patient and drug) starts \eqn{\leq} \code{min_gap} days
#' after the previous one finishes, we extend the length of the previous
#' prescription to cover the gap.
#'
#' @param data A data frame containing columns \code{prodcode}, \code{patid},
#' \code{start_date} and \code{stop_date}
#' @param min_gap Size of largest gaps to close. Default is zero, i.e. do nothing
#'
#' @return The input data frame \code{data}, possibly with some of the
#' \code{stop_date}s changed.
#'
#' @import dplyr
#'
#' @examples
#' gappy_data <- data.frame(
#'   patid = 1,
#'   prodcode = 'a',
#'   start_date = Sys.Date() + (0:6) * 7,
#'   stop_date = Sys.Date() + (0:6) * 7 + 4
#' )
#' close_small_gaps(gappy_data)
#' close_small_gaps(gappy_data, 7)
#'
#' @export
close_small_gaps <- function(data, min_gap = 0L) {
  if ((min_gap <- round(min_gap)) < 0)
    stop('min_gap must non-negative')
  if (length(min_gap) != 1)
    stop('min_gap must be a single value')
  if ('next_start_date' %in% colnames(data))
    warning('`next_start_date` is a reserved variable name but already exists')

  data %>%
    dplyr::group_by(patid, prodcode) %>%
    dplyr::mutate(
      next_start_date = dplyr::lead(start_date, order_by = start_date),
      gap = difftime(next_start_date, stop_date, units = 'days'),
      stop_date = dplyr::if_else(gap < min_gap & gap >= 0,
                                 next_start_date,
                                 stop_date,
                                 missing = stop_date)
    ) %>%
    dplyr::select(-next_start_date, -gap) %>%
    dplyr::ungroup()
}
