get_mode <- function(v) {
  # Returns the most common value. If multiple: whichever appears first.
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Implausible  quantity
#'
#' This function accepts a data frame containing prescription information and
#' replace the implusible quntity by one of the following options.
#' The user must provide a data input with the following column names:
#' patid, prodcode, qty (quantity), implausible_qty and pracid (if practice level
#' information is used for replacement). The implausible_qty column will take value 1
#' if the given qty in the data is not a plausible value and 0 otherwise. If this column
#' not provided then only decision 1a (do nothing) can be chosen.
#'
#' @param data a data frame containing prescription information.
#' @param decision a character specifying the decison to consider for processing.
#' \itemize{
#' \item{"1a"}{use implausible value}
#' \item{"1b"}{set to missing}
#' \item{"1c1"}{set to mean for individual's prescriptions for that drug}
#' \item{"1c2"}{set to mean for practice's prescriptions for that drug}
#' \item{"1c3"}{set to mean for populations's prescriptions for that drug}
#' \item{"1d1"}{set to median for individual's prescriptions for that drug}
#' \item{"1d2"}{set to median for practice's prescriptions for that drug}
#' \item{"1d3"}{set to median for population's prescriptions for that drug}
#' \item{"1e1"}{set to mode for individual's prescriptions for that drug}
#' \item{"1e2"}{set to mode for practice's prescriptions for that drug}
#' \item{"1e3"}{set to mode for population's prescriptions for that drug}
#' \item{"1f1"}{use value of individual's next prescription}
#' \item{"1f2"}{use value of practice's next prescription}
#' \item{"1f3"}{use value of population's next prescription}
#' \item{"1g1"}{use value of individual's previous prescription}
#' \item{"1g2"}{use value of practice's previous prescription}
#' \item{"1g3"}{use value of population's previous prescription}
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr across all_of group_by mutate
#'
#' @return Dataframe with the same structure as the input
#'
#' @export
dec1_implausible_qty <- function(data = NULL, decision) {
  message("Started executing dec1:implausible_qty")

  decision_group <- switch(
    substring(decision[1], 3),
    '1' = c('prodcode', 'patid'),
    '2' = c('prodcode', 'pracid'),
    '3' = 'prodcode',
    NULL
  )

  decision_fun <- switch(
    substring(decision[1], 1, 2),
    '1a' = identity,
    '1b' = function(x) NA,
    '1c' = function(x) mean(x, na.rm = TRUE),
    '1d' = function(x) median(x, na.rm = TRUE),
    '1e' = get_mode,
    stop(paste('Decision rule', decision, 'is not yet implemented'))
  )

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(decision_group))) %>%
    dplyr::mutate(qty = ifelse(implausible_qty, decision_fun(qty), qty)) %>%
    dplyr::ungroup()
}

#' Missing quantity
#'
#' This function accepts a data frame containing prescription information and
#' replace the missing quntity by one of the following options. The input data
#' must have the following columns: patid, prodcode, pracid (if practice level
#' information is used for replacement) and qty.
#'
#' @param data a data frame containing prescription information
#'
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"2a"}{Leave as missing (implicitly drop this prescription)}
##' \item{"2b1"}{set to mean for individual's prescriptions for that drug}
##' \item{"2b2"}{set to mean for practice's prescriptions for that drug}
##' \item{"2b3"}{set to mean for populations's prescriptions for that drug}
##' \item{"2c1"}{set to median for individual's prescriptions for that drug}
##' \item{"2c2"}{set to median for practice's prescriptions for that drug}
##' \item{"2c3"}{set to median for population's prescriptions for that drug}
##' \item{"2d1"}{set to mode for individual's prescriptions for that drug}
##' \item{"2d2"}{set to mode for practice's prescriptions for that drug}
##' \item{"2d3"}{set to mode for population's prescriptions for that drug}
##' \item{"2e1"}{use value of individual's next prescription}
##' \item{"2e2"}{use value of practice's next prescription}
##' \item{"2e3"}{use value of population's next prescription}
##' \item{"2f1"}{use value of individual's previous prescription}
##' \item{"2f2"}{use value of practice}'s previous prescription
##' \item{"2f3"}{use value of population's previous prescription}
##' }
#' @return Dataframe with the same structure as the input
#' @importFrom stats median


#' @export
dec2_missing_qty <- function(data = NULL, decision) {
  message("Started executing dec2:missing_qty")

  decision_group <- switch(
    substring(decision[2], 3),
    '1' = c('prodcode', 'patid'),
    '2' = c('prodcode', 'pracid'),
    '3' = 'prodcode',
    NULL
  )

  decision_fun <- switch(
    substring(decision[2], 1, 2),
    '2a' = identity,
    '2b' = function(x) NA,
    '2c' = function(x) mean(x, na.rm = TRUE),
    '2d' = function(x) median(x, na.rm = TRUE),
    '2e' = get_mode,
    stop(paste('Decision rule', decision, 'is not yet implemented'))
  )

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(decision_group))) %>%
    dplyr::mutate(qty = ifelse(is.na(qty), decision_fun(qty), qty)) %>%
    dplyr::ungroup()
}

#' Implusible ndd
#'
#' This function accepts a data frame containing prescription information
#' and replace the implusible ndd by one of the following options.
#' The user must provide a data input with the following column names:
#' patid, prodcode, ndd, implausible_ndd and pracid (if practice level
#' information is used for replacement). The implausible_ndd column will take value 1
#' if the given ndd in the data is not a plausible value and 0 otherwise. If this column
#' not provided then only decision 1a (do nothing) can be chosen.
#'
#' @param data a data frame containing prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"3a"}{use implausible value}
##' \item{"3b"}{set to missing}
##' \item{"3c1"}{set to mean for individual's prescriptions for that drug}
##' \item{"3c2"}{set to mean for practice's prescriptions for that drug}
##' \item{"3c3"}{set to mean for populations's prescriptions for that drug}
##' \item{"3d1"}{set to median for individual's prescriptions for that drug}
##' \item{"3d2"}{set to median for practice's prescriptions for that drug}
##' \item{"3d3"}{set to median for population's prescriptions for that drug}
##' \item{"3e1"}{set to mode for individual's prescriptions for that drug}
##' \item{"3e2"}{set to mode for practice's prescriptions for that drug}
##' \item{"3e3"}{set to mode for population's prescriptions for that drug}
##' \item{"3f1"}{use value of individual's next prescription}
##' \item{"3f2"}{use value of practice's next prescription}
##' \item{"3f3"}{use value of population's next prescription}
##' \item{"3g1"}{use value of individual's previous prescription}
##' \item{"3g2"}{use value of practice's previous prescription}
##' \item{"3g3"}{use value of population's previous prescription}
##' }
##'
#' @return Dataframe with the same structure as the input

#' @importFrom stats median
#'
#' @export
dec3_implausible_ndd <- function(data = NULL, decision) {
  message("Started executing dec3:implausible_ndd")

  decision_group <- switch(
    substring(decision[3], 3),
    '1' = c('prodcode', 'patid'),
    '2' = c('prodcode', 'pracid'),
    '3' = 'prodcode',
    NULL
  )

  decision_fun <- switch(
    substring(decision[3], 1, 2),
    '3a' = identity,
    '3b' = function(x) NA,
    '3c' = function(x) mean(x, na.rm = TRUE),
    '3d' = function(x) median(x, na.rm = TRUE),
    '3e' = get_mode,
    stop(paste('Decision rule', decision, 'is not yet implemented'))
  )

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(decision_group))) %>%
    dplyr::mutate(ndd = ifelse(implausible_ndd, decision_fun(ndd), ndd)) %>%
    dplyr::ungroup()
}

#' Missing ndd
#'
#' This function accepts a data frame containing prescription information and
#' replace the missing ndd by one of the following options. The input data
#' must have the following columns: patid, prodcode, pracid (if practice level
#' information is used for replacement) and ndd.
#'
#' @param data a data frame containing prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"4a"}{Leave as missing (implicitly drop this prescription)}
##' \item{"4b1"}{set to mean for individual's prescriptions for that drug}
##' \item{"4b2"}{set to mean for practice's prescriptions for that drug}
##' \item{"4b3"}{set to mean for populations's prescriptions for that drug}
##' \item{"4c1"}{set to median for individual's prescriptions for that drug}
##' \item{"4c2"}{set to median for practice's prescriptions for that drug}
##' \item{"4c3"}{set to median for population's prescriptions for that drug}
##' \item{"4d1"}{set to mode for individual's prescriptions for that drug}
##' \item{"4d2"}{set to mode for practice's prescriptions for that drug}
##' \item{"4d3"}{set to mode for population's prescriptions for that drug}
##' \item{"4e1"}{use value of individual's next prescription}
##' \item{"4e2"}{use value of practice's next prescription}
##' \item{"4e3"}{use value of population's next prescription}
##' \item{"4f1"}{use value of individual's previous prescription}
##' \item{"4f2"}{use value of practice's previous prescription}
##' \item{"4f3"}{use value of population's previous prescription}
##' }
##'
#' @return Dataframe with the same structure as the input

#' @importFrom stats median
#'
#' @export
dec4_missing_ndd <- function(data = NULL, decision) {
  message("Started executing dec4:missing_ndd")

  decision_group <- switch(
    substring(decision[4], 3),
    '1' = c('prodcode', 'patid'),
    '2' = c('prodcode', 'pracid'),
    '3' = 'prodcode',
    NULL
  )

  decision_fun <- switch(
    substring(decision[4], 1, 2),
    '4a' = identity,
    '4b' = function(x) NA,
    '4c' = function(x) mean(x, na.rm = TRUE),
    '4d' = function(x) median(x, na.rm = TRUE),
    '4e' = get_mode,
    stop(paste('Decision rule', decision, 'is not yet implemented'))
  )

  data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(decision_group))) %>%
    dplyr::mutate(ndd = ifelse(is.na(ndd), decision_fun(ndd), ndd)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-implausible_qty, -implausible_ndd, -optional)
}

#' Clean duration
#'
#' This function accepts a data frame containing prescription information
#' and compute duration and replace duration by one of the following
#' options if they are longer than clinically plusible duration. The
#' input dataset must have the following columns: qty, ndd, numdays and
#' dose_duration.
#'
#' @param data a data frame containing prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##'   \item{"5a"}{leave duration as it is}
##' 		\item{"5b_6"}{set to missing if > 6 months}
##' 		\item{"5b_12"}{set to missing if > 12 months}
##' 		\item{"5b_24"}{set to missing if > 24 months}
##' 		\item{"5c_6"}{set to 6 months if > 6 months}
##' 		\item{"5c_12"}{set to 12 months if > 12 months}
##' 		\item{"5c_24"}{set to 24 months if > 24 months}
##' }
##'
#' @return Dataframe with the same structure as the input
#'
#' @export
dec5_clean_duration <- function(data = NULL, decision) {
  message("Started executing dec5:clean duration")

  data$new_duration <- round(data$qty / data$ndd)

  if (decision[5] == "5a") {
    # do nothing
    data <- data
  }
  else if (decision[5] == "5b_6") {
    data$new_duration[data$new_duration > 183] <- NA
    data$numdays[data$numdays > 183] <- NA
    data$dose_duration[data$dose_duration > 183] <- NA
  }
  else if (decision[5] == "5b_12") {
    data$new_duration[data$new_duration > 365] <- NA
    data$numdays[data$numdays > 365] <- NA
    data$dose_duration[data$dose_duration > 365] <- NA
  }
  else if (decision[5] == "5b_24") {
    data$new_duration[data$new_duration > 730] <- NA
    data$numdays[data$numdays > 730] <- NA
    data$dose_duration[data$dose_duration > 730] <- NA
  }
  else if (decision[5] == "5c_6") {
    data$new_duration[data$new_duration > 183] <- 183
    data$numdays[data$numdays > 183] <- 183
    data$dose_duration[data$dose_duration > 183] <- 183
  }
  else if (decision[5] == "5c_12") {
    data$new_duration[data$new_duration > 365] <- 365
    data$numdays[data$numdays > 365] <- 365
    data$dose_duration[data$dose_duration > 183] <- 183
  }
  else if (decision[5] == "5c_24") {
    data$new_duration[data$new_duration > 730] <- 730
    data$numdays[data$numdays > 730] <- 730
    data$dose_duration[data$dose_duration > 730] <- 730
  }
  return(data)
}

#' Select stop date
#'
#' This function accepts a data frame containing prescription information already processed by
#' dec5_clean_duration, and select which stop date to use from one of the following options.
#' This step can only be called after dec5_clean_duration is executed.  The input dataset
#' must have the following columns: event_date, numdays, dose_duration, new_duration (column
#' name created by dec5_clean_duration).
#'
#' @param data a data frame containing prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"6a"}{stop1 (start + numdays)}
##' 	\item{"6b"}{stop2 (start + dose_duration)}
##' 	\item{"6c"}{stop3 (start + qty/ndd)}
##' 				}
##'
#' @return Dataframe with the same structure as the input
#'
#' @export
dec6_select_stop_date <- function(data = NULL, decision) {
  message("Started executing dec6:select stop date")

  data$start <- data$event_date
  if (decision[6] == "6a") {
    data$real_stop <- data$start + data$numdays
  }
  else if (decision[6] == "6b") {
    data$real_stop <- data$start + data$dose_duration
  }
  else if (decision[6] == "6c") {
    data$real_stop <- data$start + data$new_duration
  }
  data <- data %>%
    dplyr::select(-c(event_date))
  return(data)
  # i will add the three other decisons latter
}

#' Missing stop date
#'
#' This function identify missing stop dates and perform the following tasks depending on
#' the given decision. This function can only be called after dec6_select_stop_date is
#' executed. The input dataset must have the following columns: patid, prodcode, real_stop,
#' and new_duration.
#'
#' @param data a data frame containing prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"7a"}{Leave as missing, drop prescription}
##' 	\item{"7b"}{Use mean for that drug for that individual}
##' 	\item{"7c"}{Use mean for that drug for all individuals}
##' 	\item{"7d"}{Use individual mean for that drug but if not available use population mean}
##'
##' }
#' @return Dataframe with the same structure as the input
#'
#' @export
dec7_missing_stop_date <- function(data = NULL, decision) {
  message("Started executing dec7:dealing with missing stop date")

  # patid<-prodcode<-real_stop<-start<-new_duration<-NULL

  if (decision[7] == "7a") {
    # do nothing
    data <- data[!is.na(data$real_stop), ]
  }
  else if (decision[7] == "7b") {
    data <- data %>%
      dplyr::group_by(patid, prodcode) %>%
      dplyr::mutate(real_stop = ifelse(is.na(real_stop), start + mean(new_duration, na.rm = T), real_stop))
    data <- data[!is.na(data$real_stop), ]
  }
  else if (decision[7] == "7c") {
    data <- data %>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(real_stop = ifelse(is.na(real_stop), start + mean(new_duration, na.rm = T), real_stop))
    data <- data[!is.na(data$real_stop), ]
  }
  return(data)
  # i will add decison 7d latter
}

#' Handle multiple prescriptions for same product on same day
#'
#' In situations where rows of identical prescription for the same individual at the same time
#' is found in the data, this function removes the duplicated prescriptions by choosing one of the
#' following options. The input data must have the following columns: patid,prodcode,start, real_stop,
#' and ndd. It's adviced that this function is called after dec6_select_stop_date is executed.
#'
#' @param data a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"8a"}{do nothing}
##'  \item{"8b"}{use mean ndd and mean length}
##'  \item{"8c"}{choose shortest prescription}
##'  \item{"8d"}{choose longest prescription}
##'  \item{"8e"}{sum durations}
##' }
##'
#' @return Dataframe with the same structure as the input
#'
#' @export
dec8_multipleprescription_same_start_date <- function(data = NULL, decision) {
  message("Started executing dec8:idealing with multiple prescription")

  max_ndd<-min_ndd<-min_stop<-max_stop<-mean_stop<-mean_ndd<-NULL

  if (decision[8] == "8a") {
    # do nothing
    data <- data
  }
  else if (decision[8] == "8b") {
    data <- data %>%
      dplyr::group_by(patid, prodcode, start) %>%
      dplyr::mutate(
        mean_stop = mean(as.numeric(real_stop - start), na.rm = T),
        mean_ndd = mean(ndd, na.rm = T),
        real_stop = start + mean_stop,
        ndd = mean_ndd
      )
    data <- data[!(duplicated(data[, c("patid", "prodcode", "start")])), ]
    data <- data %>% dplyr::select(-c(mean_stop, mean_ndd))
  }
  else if (decision[8] == "8c") {
    data <- data %>%
      dplyr::group_by(patid, prodcode, start) %>%
      dplyr::mutate(
        min_stop = min(as.numeric(real_stop - start), na.rm = T),
        min_ndd = min(ndd, na.rm = T),
        real_stop = start + min_stop,
        ndd = min_ndd
      )
    data <- data[!(duplicated(data[, c("patid", "prodcode", "start")])), ]
    data <- data %>% dplyr::select(-c(min_stop, min_ndd))
  }
  else if (decision[8] == "8d") {
    data <- data %>%
      dplyr::group_by(patid, prodcode, start) %>%
      dplyr::mutate(
        max_stop = max(as.numeric(real_stop - start), na.rm = T),
        max_ndd = max(ndd, na.rm = T),
        real_stop = start + max_stop,
        ndd = max_ndd
      )
    data <- data[!(duplicated(data[, c("patid", "prodcode", "start")])), ]
    data <- data %>% dplyr::select(-c(max_stop, max_ndd))
  }

  else if (decision[8] == "8e") {
    data <- data %>%
      dplyr::group_by(patid, prodcode, start) %>%
      dplyr::mutate(
        sum_duration = sum(new_duration, na.rm = T),
        real_stop = start + sum_duration,
        new_duration = sum_duration
      )
    data <- data[!(duplicated(data[, c("patid", "prodcode", "start")])), ]
    data <- data %>% dplyr::select(-c(sum_duration))
  }
  return(data)
}

#' shift_interval
#'
#' Internal function used by dec9
#'
#' @param x a data frame containg prescription start and stop dates
#' @return Dataframe with the same structure as the input
#'
#' @export

shift_interval <- function(x) {
  x$id <- seq_along(x$patid)
  i <- 0
  y <- x
  overlap <- TRUE
  while (overlap) {
    i <- i + 1
    overlaps <- y %>%
      dplyr::mutate(dummy = 1) %>%
      dplyr::full_join(., ., by = "dummy") %>%
      dplyr::filter(id.x < id.y) %>%
      dplyr::mutate(overlap = DescTools::Overlap(
        cbind(start.x, end.x),
        cbind(start.y, end.y)
      )) %>%
      dplyr::filter(overlap > 0)
    y <- overlaps %>%
      dplyr::select(id = id.y, overlap) %>%
      dplyr::slice(1) %>%
      dplyr::right_join(y, by = "id") %>%
      dplyr::mutate(
        overlap = replace(overlap, is.na(overlap), 0),
        start = start + overlap,
        end = end + overlap
      ) %>%
      dplyr::select(-overlap)
    overlap <- nrow(overlaps)
  }
  list(y)
  return(y)
}
#' Handle overlapping prescriptions
#'
#' In situations where one prescription start before the end date of previous prescription
#' of the same type (prodcode) for an individual, this function either moves the start date
#' of the prescription to the end date of the previous prescription or sums the ndd of the two
#' prescrption at the time overlap depending on the users decision.
#' The input data must have the following columns: patid,prodcode,start, real_stop,
#' and ndd.
#'
#' @param data a data frame containing prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##'  \item{"9a"}{do nothing, allow prescriptions to overlap (implicity sum doses)}
##'  \item{"9b"}{move later prescription to next available time that this product is not prescribed}
##' }
##'
#' @return Dataframe with the same structure as the input.
#'
#' @export
dec9_overlaping_prescription <- function(data = NULL, decision) {
  message("Started executing dec9:dealing with overlaping prescription")

  # patid<-start<-prodcode<-real_stop<-ndd<-id.x<-id.y<-start.x<-start.y<-end.x<-end.y<-end<-NULL

  if (decision[9] == "9a") {
    # This function uses an expermental date format
    # becuse Isolate overlap accepts integer or date
    # in an intger format. Care needed
    data$start <- data.table::as.IDate(data$start)
    data$real_stop <- data.table::as.IDate(data$real_stop)
    # Isolateoverlap creates two new column named start and stop while resevring the orginal
    # to aviod naming confilict with interval_vars_out we hold the new data under the name rstart and rreal_stop
    data <- intervalaverage::isolateoverlaps(data.table::setDT(data),
      interval_vars = c("start", "real_stop"),
      group_vars = c("patid", "prodcode"),
      interval_vars_out = c("rstart", "rreal_stop")
    )
    data <- as.data.frame(data)
    # do nothing- sum overlaping doses and remove duplicate
    data <- data %>%
      dplyr::group_by(patid, prodcode, rstart) %>%
      dplyr::mutate(sum_ndd = sum(ndd, na.rm = T))
    data$ndd <- data$sum_ndd # do we need to check for implusibility
    data <- data[!(duplicated(data[, c("patid", "prodcode", "rstart")])), ]
    #data$real_stop <- as.POSIXct(data$rreal_stop)
    data <- data %>%
     dplyr::ungroup() %>%
     dplyr::select(-c(start, real_stop, sum_ndd))
    data <- reshape::rename(data, c("rstart" = "start", "rreal_stop" = "real_stop"))
  }
  else if (decision[9] == "9b") {
    data.table::setDT(data)
    data$patid_prodcode <- paste0(data$patid, data$prodcode)
    data <- reshape::rename(data, c("start" = "start", "real_stop" = "end"))
    data <- data %>%
      dplyr::group_by(patid_prodcode) %>%
      dplyr::arrange(start)
    data <- do.call(rbind, lapply(
      1:length(unique(data$patid_prodcode)),
      function(x) {
        shift_interval(data[data$patid_prodcode == unique(data$patid_prodcode)[x], ])
      }
    ))
    data <- reshape::rename(data, c("start" = "start", "end" = "real_stop"))
  }
  return(data)
}

#' Handle sequential prescriptions with short gaps
#'
#' This function provides option for filling gaps between prescription of the same type
#' for an individual depending on the choosen decison.
#'
#' @param data a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##'  \item{"10a"}{do nothing}
##'  \item{"10b_15"}{change stop date of first prescription to start date of next if gap is <= 15 days}
##'  \item{"10b_30"}{change stop date of first prescription to start date of next if gap is <= 30 days}
##'  \item{"10b_60"}{change stop date of first prescription to start date of next if gap is <= 60 days}
##' }
##'
#' @return Dataframe with the same structure as the input
#'
#' @export
dec10_gap_bn_prescription <- function(data = NULL, decision) {
  message("Started executing dec10:dealing with short gaps between presecriptions")

  # patid<-start<-prodcode<-gap_to_next<-real_stop<-NULL

  data <- data %>%
    dplyr::group_by(patid, prodcode) %>%
    dplyr::arrange(start)

  data <- data %>%
    dplyr::group_by(patid, prodcode) %>%
    dplyr::mutate(
      gap_to_next =
        dplyr::case_when(
          dplyr::n_distinct(start) > 1 & start != dplyr::last(start) ~ as.numeric(dplyr::lead(start) - real_stop),
          TRUE ~ 100000
        )
    )

  if (decision[10] == "10a") {
    # do nothing
    data <- data
  }
  else if (decision[10] == "10b") {
    data <- data %>%
      dplyr::group_by(patid, prodcode) %>%
      dplyr::mutate(real_stop = replace(real_stop, gap_to_next < 16 & gap_to_next>1, dplyr::lead(start)))
  }
  else if (decision[10] == "10c") {
    data <- data %>%
      dplyr::group_by(patid, prodcode) %>%
      dplyr::mutate(real_stop = replace(real_stop, gap_to_next < 31 & gap_to_next>1, dplyr::lead(start)))
  }
  else if (decision[10] == "10d") {
    data <- data %>%
      dplyr::group_by(patid, prodcode) %>%
      dplyr::mutate(real_stop = replace(real_stop, gap_to_next < 61 & gap_to_next>1, dplyr::lead(start)))
  }

  return(data)
}


#' run drug prep
#'
#' Utility function to run all decisions sequentially.
#' If you have implusible values (i.e., implusible quantity and ndd) to take action,
#' run first the implusible_value function.
#' If not, you need to choose 'do nothing' for dec1 and dec3.
#'
#' @param data a data frame containing prescription information
#'
#' @param decisions a character vector containing list of decison to be taken. See dec1-dec10
#' for possible values to specify under the decisions argument.
#'
#' @examples
#' dd1<-compute_ndd(data,"min_min")
#' dd1<-Implausible_values(dd1,min_max_dat)
#' dd1<-run.drugPREP(dd1,c("1b","2b1","3b","4b1","5b_6","6c","7a","8d","9a","10b"))
#'
#' @return data.frame
#'
#' @export
run.drugPREP <- function(data = NULL, decisions = NULL) {
  # check if necessary columns are in the input data
  must_names <- c("patid","pracid","implausible_qty","implausible_ndd", "event_date", "prodcode", "qty", "ndd", "numdays", "dose_duration") # u need pracid -remove it for now
  stopifnot(must_names %in% names(data))

  data %>%
    dec1_implausible_qty(decisions) %>%
    dec2_missing_qty(decisions) %>%
    dec3_implausible_ndd(decisions) %>%
    dec4_missing_ndd(decisions) %>%
    dec5_clean_duration(decisions) %>%
    dec6_select_stop_date(decisions) %>%
    dec7_missing_stop_date(decisions) %>%
    dec8_multipleprescription_same_start_date(decisions) %>%
    dec9_overlaping_prescription(decisions) %>%
    dec10_gap_bn_prescription(decisions)
}
