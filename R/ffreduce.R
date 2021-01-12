#' freduce
#' modified version of reduce function in r
#' @param value, function_list, decision
#' @return value
#'
ffreduce <- function(value, function_list,decision)
{
  k <- length(function_list)
  if (k > 1) {
    for (i in 1:(k - 1L)) {
      value <- function_list[[i]](value,decision)
    }
  }
  value <- withVisible(function_list[[k]](value,decision))
  if (value[["visible"]]) value[["value"]] else invisible(value[["value"]])
}
