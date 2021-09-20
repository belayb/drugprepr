
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
#' @param dataset A data frame containing columns \code{prodcode}, \code{pracid}, \code{patid}, \code{qty}
#' @return A data frame of the same structure as \code{dataset}, with values imputed
impute <- function(dataset,
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

  dataset %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::mutate("{{variable}}" := ifelse(which_fun({{ variable }}),
                                           impute_fun({{ variable }}),
                                           {{ variable }}))
}

iris2 <- iris
iris2[sample(nrow(iris), 50), 'Petal.Length'] <- NA
colnames(iris2)[colnames(iris2) == 'Species'] <- 'prodcode'
iris2$pracid <- as.numeric(as.numeric(iris2$prodcode) < 2)
impute(iris2, Petal.Length, fun = 'median')
impute(iris2, Petal.Length, fun = 'mean', group = 'population')
impute(iris2, Petal.Length, fun = 'mean', group = 'pracid')
