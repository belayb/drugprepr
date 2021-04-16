#' implausible  quantity
#'
#' This function acepts a data frame containg prodcode, implausible indicator and
#' replace the implusible quntity by one of the following options.
#' options are  1a  use implausible value
#'              1b  set to missing
#'              1c1  set to mean for individual's prescriptions for that drug
#'              1c2  set to mean for practice's prescriptions for that drug
#'              1c3  set to mean for populations's prescriptions for that drug
#'              1d1  set to median for individual's prescriptions for that drug
#'              1d2  set to median for practice's prescriptions for that drug
#'              1d3  set to median for population's prescriptions for that drug
#'              1e1  set to mode for individual's prescriptions for that drug
#'              1e2  set to mode for practice's prescriptions for that drug
#'              1e3  set to mode for population's prescriptions for that drug
#'              1f1  use value of individual's next prescription
#'              1f2  use value of practice's next prescription
#'              1f3  use value of population's next prescription
#'              1g1  use value of individual's previous prescription
#'              1g2  use value of practice's previous prescription
#'              1g3  use value of population's previous prescription
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec1_implausible_qty<-function(dataset1=NULL, decision)
{
  mod_fun <- function(x) unique(x)[which.max(table(x))]

  if(decision[1]=="1a")
  {
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[1]=="1b"){
    dataset1<-dataset1%>%
      dplyr::rowwise() %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, NA,qty))%>%
      dplyr::ungroup()
  }
  else if (decision[1]=="1c1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, mean(qty,na.rm=T),qty))

  }
  else if (decision[1]=="1c2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, mean(qty,na.rm = T),qty))

  }
  else if (decision[1]=="1c3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, mean(qty,na.rm = T),qty))

  }
  else if (decision[1]=="1d1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, median(qty,na.rm=T),qty))

  }
  else if (decision[1]=="1d2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, median(qty,na.rm=T),qty))

  }
  else if (decision[1]=="1d3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, median(qty,na.rm=T),qty))

  }
  else if(decision[1]=="1e1")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, mod_fun(qty,na.rm=T),qty))
  }
  else if(decision[1]=="1e2")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(pracid, prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, mod_fun(qty,na.rm=T),qty))
  }
  else if(decision[1]=="1e3")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty=ifelse(implausible_qty==1, mod_fun(qty,na.rm=T),qty))
  }
  #other possible decison to be added
  return(dataset1)
}

#' Missing  quantity
#'
#' This function acepts a data frame containg prodcode, and quantity and
#' replace the missing quntity by one of the following options.
#' options are  2a  Leave as missing (implicitly drop this prescription)
#'              2b1  set to mean for individual's prescriptions for that drug
#'              2b2  set to mean for practice's prescriptions for that drug
#'              2b3  set to mean for populations's prescriptions for that drug
#'              2c1  set to median for individual's prescriptions for that drug
#'              2c2  set to median for practice's prescriptions for that drug
#'              2c3  set to median for population's prescriptions for that drug
#'              2d1  set to mode for individual's prescriptions for that drug
#'              2d2  set to mode for practice's prescriptions for that drug
#'              2d3  set to mode for population's prescriptions for that drug
#'              2e1  use value of individual's next prescription
#'              2e2  use value of practice's next prescription
#'              2e3  use value of population's next prescription
#'              2f1  use value of individual's previous prescription
#'              2f2  use value of practice's previous prescription
#'              2f3  use value of population's previous prescription
#' @param dataset, decsion
#' @return dataset
#' @export
dec2_missing_qty<-function(dataset1=NULL, decision )
{
  mod_fun <- function(x) unique(x)[which.max(table(x))]

  if(decision[2]=="2a")
  {
    #do nothing
    dataset1<-dataset1

  }
  else if (decision[2]=="2b1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), mean(qty,na.rm=T),qty))
  }
  else if (decision[2]=="2b2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), mean(qty,na.rm=T),qty))
  }
  else if (decision[2]=="2b3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), mean(qty,na.rm=T),qty))
  }

  else if (decision[2]=="2c1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), median(qty,na.rm=T),qty))
  }
  else if (decision[2]=="2c2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), median(qty,na.rm=T),qty))
  }
  else if (decision[2]=="2c3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), median(qty,na.rm=T),qty))
  }

  else if (decision[2]=="2d1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), mod_fun(qty,na.rm=T),qty))
  }
  else if (decision[2]=="2d2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), mod_fun(qty,na.rm=T),qty))
  }

  else if (decision[2]=="2d3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty=ifelse(is.na(qty), mod_fun(qty,na.rm=T),qty))
  }
  return(dataset1)
}

#' Implusible  ndd
#'
#' This function acepts a data frame containg prodcode, ndd, and implusible ndd indicator and
#' replace the implusible ndd by one of the following options.
#' options are  3a  use implausible value
#'              3b  set to missing
#'              3c1  set to mean for individual's prescriptions for that drug
#'              3c2  set to mean for practice's prescriptions for that drug
#'              3c3  set to mean for populations's prescriptions for that drug
#'              3d1  set to median for individual's prescriptions for that drug
#'              3d2  set to median for practice's prescriptions for that drug
#'              3d3  set to median for population's prescriptions for that drug
#'              3e1  set to mode for individual's prescriptions for that drug
#'              3e2  set to mode for practice's prescriptions for that drug
#'              3e3  set to mode for population's prescriptions for that drug
#'              3f1  use value of individual's next prescription
#'              3f2  use value of practice's next prescription
#'              3f3  use value of population's next prescription
#'              3g1  use value of individual's previous prescription
#'              3g2  use value of practice's previous prescription
#'              3g3  use value of population's previous prescription
#' @param dataset, decsion
#' @return dataset
#' @export
dec3_implausible_ndd<-function(dataset1=NULL, decision )
{
  mod_fun <- function(x) unique(x)[which.max(table(x))]

  if(decision[3]=="3a")
  {
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[3]=="3b"){
    dataset1<-dataset1%>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, NA,ndd))
  }
  else if (decision[3]=="3c1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, mean(ndd,na.rm=T),ndd))
  }
  else if (decision[3]=="3c2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, mean(ndd,na.rm=T),ndd))
  }
  else if (decision[3]=="3c3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, mean(ndd,na.rm=T),ndd))
  }
  else if (decision[3]=="3d1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, median(ndd,na.rm=T),ndd))
  }
  else if (decision[3]=="3d2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, median(ndd,na.rm=T),ndd))
  }
  else if (decision[3]=="3d3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, median(ndd,na.rm=T),ndd))
  }

  else if(decision[3]=="3e1")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, mod_fun(ndd,na.rm=T),ndd))
  }

  else if(decision[3]=="3e2")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, mod_fun(ndd,na.rm=T),ndd))
  }

  else if(decision[3]=="3e3")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd=ifelse(implausible_ndd==1, mod_fun(ndd,na.rm=T),ndd))
  }
  return(dataset1)

}

#' Missing  ndd
#'
#' This function acepts a data frame containg prodcode, and ndd and
#' replace the missing ndd by one of the following options.
#' options are  4a  Leave as missing (implicitly drop this prescription)
#'              4b1  set to mean for individual's prescriptions for that drug
#'              4b2  set to mean for practice's prescriptions for that drug
#'              4b3  set to mean for populations's prescriptions for that drug
#'              4c1  set to median for individual's prescriptions for that drug
#'              4c2  set to median for practice's prescriptions for that drug
#'              4c3  set to median for population's prescriptions for that drug
#'              4d1  set to mode for individual's prescriptions for that drug
#'              4d2  set to mode for practice's prescriptions for that drug
#'              4d3  set to mode for population's prescriptions for that drug
#'              4e1  use value of individual's next prescription
#'              4e2  use value of practice's next prescription
#'              4e3  use value of population's next prescription
#'              4f1  use value of individual's previous prescription
#'              4f2  use value of practice's previous prescription
#'              4f3  use value of population's previous prescription
#' @param dataset, decsion
#' @return dataset
#' @export
dec4_missing_ndd<-function(dataset1=NULL, decision )
{
  mod_fun <- function(x) unique(x)[which.max(table(x))]

  if(decision[4]=="4a"){
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[4]=="4b1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), mean(ndd,na.rm=T),ndd))
  }

  else if (decision[4]=="4b2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), mean(ndd,na.rm=T),ndd))
  }
  else if (decision[4]=="4b3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), mean(ndd,na.rm=T),ndd))
  }
  else if (decision[4]=="4c1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), median(ndd,na.rm=T),ndd))
  }

  else if (decision[4]=="4c2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), median(ndd,na.rm=T),ndd))
  }
  else if (decision[4]=="4c3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), median(ndd,na.rm=T),ndd))
  }
  else if (decision[4]=="4d1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), mod_fun(ndd,na.rm=T),ndd))
  }
  else if (decision[4]=="4d2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), mod_fun(ndd,na.rm=T),ndd))
  }
  else if (decision[4]=="4d3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd=ifelse(is.na(ndd), mod_fun(ndd,na.rm=T),ndd))
  }
  return(dataset1)

}

#' Clean  duration
#'
#' This function acepts a data frame containg prodcode, qty, and ndd and
#' compute duration and replace duration by  by one of the following options if they are longer than clinically plusible.
#' options are	5a	leave duration as it is
#'		5b_6	set to missing if > 6 months
#'		5b_12	set to missing if > 12 months
#'		5b_24	set to missing if > 24 months
#'		5c_6	set to 6 months if > 6 months
#'		5c_12	set to 12 months if > 12 months
#'		5c_24	set to 24 months if > 24 months
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec5_clean_duration<-function(dataset1, decision)
{
  dataset1$new_duration<-round(dataset1$qty/dataset1$ndd)

  if(decision[5]=="5a"){
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[4]=="5b_6"){
    dataset1$new_duration[dataset1$new_duration>183]<-NA
    dataset1$numdays[dataset1$numdays>183]<-NA
    dataset1$dose_duration[dataset1$dose_duration>183]<-NA

  }
  else if (decision[4]=="5b_12"){
    dataset1$new_duration[dataset1$new_duration>365]<-NA
    dataset1$numdays[dataset1$numdays>365]<-NA
    dataset1$dose_duration[dataset1$dose_duration>365]<-NA

  }
  else if (decision[4]=="5b_24"){
    dataset1$new_duration[dataset1$new_duration>730]<-NA
    dataset1$numdays[dataset1$numdays>730]<-NA
    dataset1$dose_duration[dataset1$dose_duration>730]<-NA

  }
  else if (decision[4]=="5c_6"){
    dataset1$new_duration[dataset1$new_duration>183]<-183
    dataset1$numdays[dataset1$numdays>183]<-183
    dataset1$dose_duration[dataset1$dose_duration>183]<-183

  }
  else if (decision[4]=="5c_12"){
    dataset1$new_duration[dataset1$new_duration>365]<-365
    dataset1$numdays[dataset1$numdays>365]<-365
    dataset1$dose_duration[dataset1$dose_duration>183]<-183

  }
  else if (decision[4]=="5c_24"){
    dataset1$new_duration[dataset1$new_duration>730]<-730
    dataset1$numdays[dataset1$numdays>730]<-730
    dataset1$dose_duration[dataset1$dose_duration>730]<-730

  }
  return(dataset1)
}

#' Stop date
#' Select which stop date to use
#' options are	6a	stop1 (start + numdays)
#'		6b	stop2 (start + dose_duration)
#'		6c	stop3 (start + qty/ndd)
#'			If only one stop available, use it
#'			if two available and equal, use that date
#'			if two available and unequal (but within x days), use mean
#'			if three available and unequal, use mean of closest 2 if within x days
#'				6d_15	x = 15
#' 			6d_30	x = 30
#'				6d_60	x = 60
#'				6d_90	x = 90
#'				6e 	x = something very big, like 9,999,999
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec6_select_stop_date<-function(dataset1, decision)
{
  dataset1$start<-as.IDate(dataset1$event_date)
  if(decision[6]=="6a"){
    dataset1$real_stop<-dataset1$start+dataset1$numdays
  }
  else if (decision[6]=="6b"){
    dataset1$real_stop<-dataset1$start+dataset1$dose_duration

  }
  else if (decision[6]=="6c"){
    dataset1$real_stop<-dataset1$start+dataset1$new_duration

  }
  return(dataset1)
  #i will add the three other decisons latter
}

#' Handle missing stop dates
#' options are	7a	Leave as missing, drop prescription
#'		7b	Use mean for that drug for that individual
#'		7c	Use mean for that drug for all individuals
#'		7d 	Use individual mean for that drug but if not available use population mean
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec7_missing_stop_date<-function(dataset1, decision)
{
  if(decision[7]=="7a"){
    #do nothing
    dataset1<-dataset1[!is.na(dataset1$real_stop),]
  }
  else if (decision[7]=="7b"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=ifelse(is.na(real_stop), start+mean(new_duration,na.rm=T),real_stop))
    dataset1<-dataset1[!is.na(dataset1$real_stop),]
  }
  else if (decision[7]=="7c"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(real_stop=ifelse(is.na(real_stop), start+mean(new_duration,na.rm=T),real_stop))
    dataset1<-dataset1[!is.na(dataset1$real_stop),]
  }
  return(dataset1)
  #i will add decison 7d latter
}

#' Handle multiple prescriptions for same product on same day
#' will always drop if textid == 0 and there is another, non-zero textid
#' options are	8a	Do nothing: implicitly sum doses as in dec9 below
#'		8b	use mean ndd and mean length
#'		8c	choose prescription with smallest ndd
#'		8d	choose prescription with largest ndd
#'		8e	choose shortest prescription
#'		8f	choose longest prescription
#' 	8g	sum durations
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec8_multipleprescription_same_start_date<-function(dataset1, decision)
{
  if(decision[8]=="8a"){
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[8]=="8b"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(mean_stop=mean(real_stop[!is.na(real_stop)]), mean_ndd=mean(ndd, na.rm=T))#not sure what this decison is doing mean of stop date don't make sence
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1$real_stop<-dataset1$mean_stop
    dataset1$ndd<-dataset1$mean_ndd
    dataset1<-dataset1[,-c("mean_stop","mean_ndd")]
  }
  else if (decision[8]=="8c"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(min_ndd=min(ndd[!is.na(ndd)]))
    dataset1<-dataset1[dataset1$ndd==min_ndd,]
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1<-dataset1[,-c("min_ndd")]

  }
  else if (decision[8]=="8d"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(max_ndd=max(ndd[!is.na(ndd)]))
    dataset1<-dataset1[dataset1$ndd==max_ndd,]
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1<-dataset1[,-c("max_ndd")]

  }
  else if (decision[8]=="8e"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(min_stop=min(real_stop[!is.na(real_stop)]))
    dataset1<-dataset1[dataset1$real_stop==min_stop,]
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1<-dataset1[,-c("min_stop")]

  }
  else if (decision[8]=="8f"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(max_stop=max(real_stop[!is.na(real_stop)]))
    dataset1<-dataset1[dataset1$real_stop==max_stop,]
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1<-dataset1[,-c("max_stop")]

  }
  else if (decision[8]=="8g"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(sum_duration=sum(new_duration[!is.na(new_duration)]))
    dataset1$real_stop<-dataset1$start+dataset1$sum_duration
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1<-dataset1[,-c("sum_duration")]

  }
  return(dataset1)
}

#' Handle overlapping prescriptions
#' options are:	9a  do nothing, allow prescriptions to overlap (implicity sum doses)
#'              9b  move later prescription to next available time that this product is not prescribed
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec9_overlaping_prescription<-function(dataset1, decision)
{

  # do the slicing first by patid and prodcode
  dataset1<-data.table::setDT(dataset1)[order(patid, start, prodcode)]

  if(decision[9]=="9a"){
    dataset1$start<-as.IDate(dataset1$start)#Isolateoverlap creates two new column named start and stop while resevring the orginal
    dataset1$real_stop<-as.IDate(dataset1$real_stop)#Isolateoverlap creates two new column named start and stop while resevring the orginal

    dataset1 <- dataset1 %>%
      rename(rstart = start, rreal_stop=real_stop)#to aviod naming confilict with interval_vars_out

    dataset1 <- intervalaverage::isolateoverlaps(dataset1,interval_vars=c("rstart","rreal_stop"),group_vars=c("patid","prodcode"), interval_vars_out = c("start", "real_stop"))
    #do nothing- sum overlaping doses and remove duplicate
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(sum_ndd=sum(ndd, na.rm=T))
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1$ndd<-dataset1$sum_ndd # do we need to check for implusibility
    #dataset1<-dataset1[,-c("sum_ndd")]
  }
  else if (decision[8]=="8b"){


    shift_interval <- function(x) {
      i <- 0
      y <- x
      overlap <- TRUE
      while (overlap) {

        i <- i + 1
        overlaps <- y %>%
          dplyr::mutate(dummy = 1) %>%
          full_join(., ., by = 'dummy') %>%
          filter(id.x < id.y) %>%
          mutate(overlap = DescTools::Overlap(cbind(start.x, end.x),
                                              cbind(start.y, end.y))) %>%
          dplyr::filter(overlap > 0)
        y <- overlaps %>%
          select(id = id.y, overlap) %>%
          slice(1) %>%
          right_join(y, by = 'id') %>%
          dplyr::mutate(overlap = replace(overlap, is.na(overlap), 0),
                        start = start + overlap,
                        end = end + overlap) %>%
          select(-overlap)
        overlap <- nrow(overlaps)
      }
      list(y)
      return(y)
    }

    dataset1<-data.table::setDT(dataset1)[order(patid, start, prodcode)]
    dataset1$patid_prodcode<-paste0(dataset1$patid,dataset1$prodcode)
    dataset1<-reshape:::rename(dataset1, c("start"="start", "real_stop"="end"))
    dataset1<-cbind(dataset1,do.call(rbind,lapply(1:length(unique(dataset1$patid_prodcode)), function(x) shift_interval(dataset1[dataset1$patid_prodcode==unique(dataset1$patid_prodcode)[x],]))))
  }
  return(dataset1)

}

#' dec10: Handle sequential prescriptions with short gaps
#' options are	10a	Do nothing
#'			change stop date of first prescription to start date of next if gap is <=
#'			10b_15  15 days
#'			10b_30  30 days
#'			10b_60  60 days
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec10_gap_bn_prescription<-function(dataset1, decision)
{
  dataset1<-data.table::setDT(dataset1)
  dataset1<-dataset1[order(patid,start, prodcode)]

  dataset1<-dataset1%>%
    dplyr::group_by(patid,prodcode) %>%
    dplyr::mutate(gap_to_next=lead(start)-real_stop)

  if(decision[10]=="10a"){
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[10]=="10b"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=replace(real_stop,gap_to_next<16,lead(start)))
  }
  else if (decision[10]=="10c"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=replace(real_stop,gap_to_next<31,lead(start)))
  }
  else if (decision[10]=="10d"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=replace(real_stop,gap_to_next<61,lead(start)))
  }

  return(dataset1)
}

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



#' run drug prep
#' outer function to run all decisons
#' If you have implusible values to take action, run first the implusible_value function
#' else you need to choose 'do nothing' for dec1 and dec3
#'
#' @param dataset, decisons,
#' @return dataset
#' @export
run.drugPREP<-function(dataset1=NULL, decisions=NULL)
{
  #check if nessary columns are in the input data
  must_names<-c("patid","event_date","prodcode","qty","ndd","numdays","dose_duration")#u need pracid -remove it for now
  stopifnot(must_names%in%names(dataset1))

  function_list=list(dec1_implausible_qty, dec2_missing_qty,dec3_implausible_ndd,dec4_missing_ndd,
                     dec5_clean_duration, dec6_select_stop_date,dec7_missing_stop_date,
                     dec8_multipleprescription_same_start_date,dec9_overlaping_prescription,
                     dec10_gap_bn_prescription)
  decision=decisions
  run_decisions<-ffreduce(dataset1, function_list, decision)
  return(run_decisions)
}

