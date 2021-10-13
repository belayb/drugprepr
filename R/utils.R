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
    c(20L, 65L, 46L, 3L, 33L, 10L, 28L, 33L, 47L, 10L, 56L, 19L,
      13L, 35L, 17L, 138L, 7L, 15L, 18L, 12L, 27L, 12L, 19L, 31L, 46L,
      32L, 39L, 73L, 17L, 2L),
  qty = c(50L, 49L, 56L, 37L, NA, 56L, 51L, 39L, 51L, 55L, 41L, 47L,
          46L, 43L, NA, 44L, 50L, 50L, NA, 47L, 46L, 62L, 52L, 40L, 49L,
          NA, 38L, 57L, 49L, NA),
  ndd = c(6, 7.5, 2, 7, 1.5, 7, 8, 4.5, 6.5, 3, 2, 3, 8, 5.5, 1.5, 6,
          NA, 5, 6.5, 6.5, 6, 7, 6.5, 3.5, 5.5, 5, 3.5, 0.5, 3, 2),
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
#' @param open logical. If \code{TRUE}, values exactly equal to \code{lower} or \code{upper} are also considered implausible
outside_range <- function(x, lower, upper, open = TRUE) {
  if (closed) {
    x < lower | x > upper
  } else x <= lower | x >= upper
}

#' Get the mode (most common value) of a vector
#'
#' @param v a vector
#' @param na.rm Logical. If \code{TRUE} (the default), find mode of non-\code{NA} values
get_mode <- function(v, na.rm = TRUE) {
  # Returns the most common value. If multiple: whichever appears first.
  if (na.rm) v <- v[!is.na(v)]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
