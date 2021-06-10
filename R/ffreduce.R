#' Utility function to sequentially apply function lists
#'
#' This function accepts the list of functions and decisons corresponding to each
#' functions and sequentially apply them to the data. That is the resulting data of
#' function x will be the input for function x+1
#'
#' @param value value
#' @param function_list list of decison nodes for processing
#' @param decision decision vector
#' @return Dataframe with the same structure as the input
#'
#'
ffreduce <- function(value, function_list, decision) {
  k <- length(function_list)
  if (k > 1) {
    for (i in 1:(k - 1L)) {
      value <- function_list[[i]](value, decision)
    }
  }
  value <- withVisible(function_list[[k]](value, decision))
  if (value[["visible"]]) value[["value"]] else invisible(value[["value"]])
}
