
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
#' @param dataset A data frame containing columns \code{prodcode}, \code{pracid}, \code{patid}
#' @param variable Unquoted name of the column in \code{dataset} to be imputed
#' @param which_fun Function applied to \code{variable} that returns a logical vector indicating which elements to impute. Defaults to \code{\link[base:NA]{is.na}}
#' @param fun Method for imputing the values. See details.
#' @param group Level of structure for imputation. Defaults to whole study population.
#' @param ... Not used
#'
#' @examples
#' iris2 <- iris
#' iris2[sample(nrow(iris), 50), 'Petal.Length'] <- NA
#' colnames(iris2)[colnames(iris2) == 'Species'] <- 'prodcode'
#' iris2$pracid <- as.numeric(as.numeric(iris2$prodcode) < 2)
#' impute(iris2, Petal.Length, fun = 'median')
#' impute(iris2, Petal.Length, fun = 'mean', group = 'population')
#' impute(iris2, Petal.Length, fun = 'mean', group = 'pracid')
#'
#' @return A data frame of the same structure as \code{dataset}, with values imputed
impute <- function(data,
                   variable,
                   which_fun = is.na,
                   fun = c('ignore', 'mean', 'median', 'mode', 'missing'),
                   group = c('population', 'patid', 'pracid'),
                   ...) {

  which_fun <- match.fun(which_fun)
  fun <- match.arg(fun)
  group <- match.arg(group)

  impute_fun <- switch(fun,
                       'ignore' = identity, # slightly confusing because 'ignore' might imply deleting these outliers, but the function actually leaves them as-is
                       'missing' = function(x) NA, # rather than 'missing', should be a verb maybe
                       'mean' = function(x) mean(x, na.rm = TRUE),
                       'median' = function(x) median(x, na.rm = TRUE),
                       'mode' = get_mode)
  group_vars <- c('prodcode', if (group == 'population') NULL else group)

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate("{{variable}}" := ifelse(which_fun({{ variable }}),
                                           impute_fun({{ variable }}),
                                           {{ variable }}))
}

#' Find implausible entries

#' Replace implausible or missing prescription quantities
#'
#' @inheritParams impute
#'
#' @export
impute_qty <- function(data,
                       which_fun,
                       fun = c('ignore', 'mean', 'median', 'mode', 'missing'),
                       group = c('population', 'patid', 'pracid'),
                       ...) {
  impute(data, qty, which_fun, fun, group, ...)
}

#' Replace implausible or missing numerical daily doses (NDD)
#'
#' @inheritParams impute_ndd
#'
#' @export
impute_ndd <- function(data,
                       which_fun,
                       fun = c('ignore', 'mean', 'median', 'mode', 'missing'),
                       group = c('population', 'patid', 'pracid'),
                       ...) {
  impute(data, ndd, which_fun, fun, group, ...)
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
#' Though the function \code{%within%} already exists, it is not vectorised.
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

