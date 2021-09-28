
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
#' @import dplyr
#' @param data A data frame containing columns \code{prodcode}, \code{pracid}, \code{patid}
#' @param variable Unquoted name of the column in \code{dataset} to be imputed
#' @param method Method for imputing the values. See details.
#' @param where Function applied to \code{variable} that returns a logical vector indicating which elements to impute. Defaults to \code{\link[base:NA]{is.na}}
#' @param group Level of structure for imputation. Defaults to whole study population.
#' @param ... Extra arguments, currently ignored
#'
#' @details
#' Possible values for \code{fun} are
#' \itemize{
#' \item \code{ignore}. Do nothing, leaving input unchanged.
#' \item \code{mean}. Replace values with the mean by \code{group}
#' \item \code{median}. Replace values with the median by \code{group}
#' \item \code{mode}. Replace values with the most common value by \code{group}
#' \item \code{missing}. Replace values with \code{NA}, to mark as missing.
#' }
#'
#' @return A data frame of the same structure as \code{data}, with values imputed
impute <- function(data,
                   variable,
                   method = c('ignore', 'mean', 'median', 'mode', 'missing'),
                   where = is.na,
                   group = c('population', 'patid', 'pracid'),
                   ...) {

  where <- match.fun(where)
  method <- match.arg(method)
  group <- match.arg(group)

  impute_fun <- switch(method,
                       'ignore' = identity, # slightly confusing because 'ignore' might imply deleting these outliers, but the function actually leaves them as-is
                       'missing' = function(x) replace(x, TRUE, NA), # rather than 'missing', should be a verb maybe
                       'mean' = function(x) mean(x, na.rm = TRUE),
                       'median' = function(x) median(x, na.rm = TRUE),
                       'mode' = get_mode)
  group_vars <- c('prodcode', if (group == 'population') NULL else group)

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate("{{variable}}" := ifelse(no   = {{ variable }},
                                           test = rep_len(where({{ variable }}), length({{ variable }})),
                                           yes  = impute_fun({{ variable }})))
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
                       method = c('ignore', 'mean', 'median', 'mode', 'missing'),
                       where = is.na,
                       group = c('population', 'patid', 'pracid'),
                       ...) {
  impute(data, qty, method, where, group, ...)
}

#' Replace implausible or missing numerical daily doses (NDD)
#'
#' @inheritParams impute_ndd
#'
#' @examples
#' impute_ndd(example_therapy, 'mean')
#'
#' @export
impute_ndd <- function(data,
                       method = c('ignore', 'mean', 'median', 'mode', 'missing'),
                       where = is.na,
                       group = c('population', 'patid', 'pracid'),
                       ...) {
  impute(data, ndd, method, where, group, ...)
}

############ Utilities ############

#' Example electronic prescription dataset
#'
#' Based on a hypothetical 'therapy' file from the Clinical Practical Research
#' Datalink (CPRD), a UK database of primary care records.
#'
#' @note This dataset is generated nondeterministically, so it may vary between
#' builds (or maybe even sessions?) of the \code{drugprepr} package.
#'
#' @export
example_therapy <- data.frame(
  patid = rep(1:10, each = 3),
  pracid = rep(c('x', 'y'), each = 3),
  prodcode = sample(letters[1:2], 30, replace = TRUE),
  qty = sample(c(rpois(25, 50), rep(NA, 5))),
  ndd = sample(c(seq(0.5, 8, by = .5), NA), 30, replace = TRUE),
  stringsAsFactors = FALSE
)

#' Do values fall outside a specified 'plausible' range?
#'
#' A utility function for indicating if elements of a vector are implausible.
#'
#' Though the function \code{\link[dplyr]{between}} already exists, it is not vectorised over the bounds.
#'
#' @param x numeric vector
#' @param lower minimum plausible value
#' @param upper maximum plausible value
#' @param closed logical. If \code{TRUE}, values exactly equal to \code{lower} or \code{upper} are also considered implausible
outside_range <- function(x, lower, upper, open = TRUE) {
  if (closed) {
    x < lower | x > upper
  } else x <= lower | x >= upper
}

