#' Compute the number of daily dose (ndd)
#'
#' @description
#' The function calls the doseminer r-package to extract doses from text prescription
#' and compute the (ndd) according to the given decision.
#' The general formula for computing (ndd) is given by (ndd=DF*DN/DI), where (DF) is dose
#' frequency, (DN) is dose number, and (DI) is dose interval. Prescriptions
#' can have a varible dose frequency or dose number such as '2-4 tablets'.
#'
#'
#' @param dataset1 a data frame object containing the prescription information.
#' The dataset is must have column names 'dosageid' and 'text'. This dataset is
#' expected to be a result of merging CPRD 'therapy' table with 'commondosage' table.
#' @param decision a character variable for computing number of (ndd).
##' \itemize{
##' \item{"min_min"}{ minimum dose number and minimum dose frequency}
##' \item{"min_mean"}{ minimum dose number and mean dose frequency}
##' \item{"min_max"}{ minimum dose number and maximum dose frequency}
##' \item{"mean_min"}{ mean dose number and minimum dose frequency}
##' \item{"mean_mean"}{ mean dose number and mean dose frequency}
##' \item{"mean_max"}{ mean dose number and maximum dose frequency}
##' \item{"max_min"}{ maximum dose number and minimum dose frequency}
##' \item{"max_mean"}{ maximum dose number and mean dose frequency}
##' \item{"max_max"}{ maximum dose number and maximum dose frequency}
##' }
##'
#'
#' @return Dataframe with the same structure as the input
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export

compute_ndd <- function(dataset1 = NULL, decision = NULL) {
  freq_2 <- freq_1 <- dose_2 <- dose_1 <- DF_mean <- DF_min <- DF_max <- DN_mean <- DN_min <- DN_max <- optional <- NULL
  # crate a dataset from dataset1 with unique dossageid
  temp_presc <- dataset1[
    !duplicated(dataset1[, c("dossageid")]),
    c("dossageid", "text")
  ]
  temp_presc <- cbind(temp_presc[, "dossageid"], doseminer::extract_from_prescription(temp_presc$text) %>%
    splitstackshape::cSplit(c("freq", "dose"), sep = "-", direction = "wide") %>%
    dplyr::rowwise() %>% dplyr::mutate(
      freq_2 = ifelse(is.na(freq_2), freq_1, freq_2),
      dose_2 = ifelse(is.na(dose_2), dose_1, dose_2),
      DF_mean = mean(c(freq_1, freq_2), na.rm = T),
      DN_mean = mean(c(dose_1, dose_2), na.rm = T),
      DF_min = freq_1, DF_max = freq_2,
      DN_min = dose_1, DN_max = dose_2
    ) %>%
    dplyr::select(c(optional, DF_mean:DN_max)))
  names(temp_presc)[1] <- "dossageid"
  dataset1 <- dplyr::left_join(dataset1, temp_presc, by = "dossageid")

  if (decision == "min_min") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_min * DF_min)
  }
  else if (decision == "min_mean") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_min * DF_mean)
  }
  else if (decision == "min_max") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_min * DF_max)
  }
  else if (decision == "mean_min") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_mean * DF_min)
  }
  else if (decision == "mean_mean") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_mean * DF_mean)
  }
  else if (decision == "mean_max") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_mean * DF_max)
  }
  else if (decision == "max_min") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_max * DF_min)
  }
  else if (decision == "max_mean") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_max * DF_mean)
  }
  else if (decision == "max_max") {
    dataset1 <- dataset1 %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd = DN_max * DF_max)
  }
  dataset1 <- dataset1 %>%
    dplyr::ungroup() %>% # NB 'rowwise()' is often very inefficient
    dplyr::select(-matches("ndd\\d|^DN|^DF")) # get rid of DN_x, DF_x and ndd# cols

  return(dataset1)
}
