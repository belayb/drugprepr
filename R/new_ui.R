
# 1. Implausible total quantity
# 2. Missing total quantity
# 3. Implausible daily dose
# 4. Missing daily dose
# 5. Clean duration
# 6. Select stop date type
# 7. Missing stop date
# 8. Multiple prescriptions with same start date
# 9. Overlapping prescriptions
# 10. Gaps between prescriptions

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
#' }
#'
#' @return A data frame of the same structure as \code{data}, with values imputed
impute <- function(data,
                   variable,
                   method = c('ignore', 'mean', 'median', 'mode', 'replace'),
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
  # group <- match.arg(group)

  impute_fun <- switch(method,
                       'ignore' = identity,
                       'replace' = function(x) replace(x, TRUE, replace_with),
                       'mean' = function(x) mean(x, na.rm = TRUE),
                       'median' = function(x) median(x, na.rm = TRUE),
                       'mode' = get_mode)

  group_vars <- if (method %in% c('ignore', 'replace')) NULL else
    c('prodcode', if (all(group == 'population')) NULL else group)

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
                       method = c('ignore', 'mean', 'median', 'mode', 'replace'),
                       where = is.na,
                       group = c('population', 'patid', 'pracid'),
                       ...) {
  impute(data, qty, method, where, match.arg(group), ...)
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
                       method = c('ignore', 'mean', 'median', 'mode', 'replace'),
                       where = is.na,
                       group = c('population', 'patid', 'pracid'),
                       ...) {
  impute(data, ndd, method, where, match.arg(group), ...)
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
                            method = c('ignore', 'mean', 'median', 'mode', 'replace'),
                            where = is.na,
                            group = c('patid', 'start_date'),
                            ...) {
  impute(data, duration, method, where, group, ...)
}

#' Example electronic prescription dataset
#'
#' Based on a hypothetical 'therapy' file from the Clinical Practical Research
#' Datalink (CPRD), a UK database of primary care records.
#'
#' @note This dataset is now generated deterministically, so it will not vary
#' between sessions.
#'
#' @export
example_therapy <- data.frame(
  patid = rep(1:10, each = 3),
  pracid = rep(c('x', 'y'), each = 3),
  prodcode = c("a", "b", "b", "a", "a", "a", "a", "a", "b", "a", "b", "a",
               "a", "a", "b", "b", "b", "a", "a", "a", "b", "b", "b", "a", "a",
               "b", "b", "a", "a", "a"),
  start_date = as.Date('2021-10-01') +
    c(310L, 118L, 49L, 34L, 153L, 166L, 179L, 328L, 262L, 271L, 25L,
      154L, 114L, 212L, 329L, 263L, 156L, 66L, 317L, 279L, 47L, 29L,
      235L, 309L, 159L, 92L, 42L, 253L, 127L, 312L),
  qty = c(50L, 49L, 56L, 37L, NA, 56L, 51L, 39L, 51L, 55L, 41L, 47L,
          46L, 43L, NA, 44L, 50L, 50L, NA, 47L, 46L, 62L, 52L, 40L, 49L,
          NA, 38L, 57L, 49L, NA),
  ndd = c(6, 7.5, 2, 7, 1.5, 7, 8, 4.5, 6.5, 3, 2, 3, 8, 5.5, 1.5, 6,
          NA, 5, 6.5, 6.5, 6, 7, 6.5, 3.5, 5.5, 5, 3.5, 0.5, 3, 2),
  stringsAsFactors = FALSE
)

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
