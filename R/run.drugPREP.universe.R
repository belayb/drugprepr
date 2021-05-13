#' implausible  quantity
#'
#' This function acepts a data frame containg prodcode, implausible indicator and replace the implusible quntity by one of the following options.
#'
#'
#'
#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"1a"}{use implausible value}
##' \item{"1b"{set to missing}
##' \item{"1c1"}{set to mean for individual's prescriptions for that drug}
##' \item{"1c2"}{set to mean for practice's prescriptions for that drug}
##' \item{"1c3"}{set to mean for populations's prescriptions for that drug}
##' \item{"1d1"}{set to median for individual's prescriptions for that drug}
##' \item{"1d2"}{set to median for practice's prescriptions for that drug}
##' \item{"1d3"}{set to median for population's prescriptions for that drug}
##' \item{"1e1"}{set to mode for individual's prescriptions for that drug}
##' \item{"1e2"}{set to mode for practice's prescriptions for that drug}
##' \item{"1e3"}{set to mode for population's prescriptions for that drug}
##' \item{"1f1"}{use value of individual's next prescription}
##' \item{"1f2"}{use value of practice's next prescription}
##' \item{"1f3"}{use value of population's next prescription}
##' \item{"1g1"}{use value of individual's previous prescription}
##' \item{"1g2"}{use value of practice's previous prescription}
##' \item{"1g3"}{use value of population's previous prescription}
##' }
##'
#' @importFrom stats median

#' @return data.frame
#' @export
dec1_implausible_qty<-function(dataset1=NULL, decision)
{
  #implausible_qty<-qty<-patid<-prodcode<-pracid<-NULL

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
#' This function acepts a data frame containg prodcode, and quantity and replace the missing quntity by one of the following options.
#'
#' @param dataset1 a data frame containg prescription information
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

#' @return data.frame

#' @importFrom stats median


#' @export
dec2_missing_qty<-function(dataset1=NULL, decision )
{
  #qty<-patid<-prodcode<-pracid<-NULL

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
#' This function acepts a data frame containg prodcode, ndd, and implusible ndd indicator and replace the implusible ndd by one of the following options.
#'
#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"3a"}{use implausible value}
##' \item{"3b"{set to missing}
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
#' @return data.frame

#' @importFrom stats median
#'
#' @export
dec3_implausible_ndd<-function(dataset1=NULL, decision )
{

  #implausible_ndd<-ndd<-patid<-prodcode<-pracid<-NULL


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
#'
#' @param dataset1 a data frame containg prescription information
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
##' \item{"4f2"}{use value of practice}'s previous prescription
##' \item{"4f3"}{use value of population's previous prescription}
##' }
##'
#' @return data.frame

#' @importFrom stats median
#'
#' @export
dec4_missing_ndd<-function(dataset1=NULL, decision )
{
  #ndd<-patid<-prodcode<-pracid<-NULL

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
#' This function acepts a data frame containg prodcode, qty, and ndd and compute duration and replace duration by  by one of the following options if they are longer than clinically plusible.
#'
#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##'   \item{"5a"}{leave duration as it is}
##'		\item{"5b_6"}{set to missing if > 6 months}
##'		\item{"5b_12"}{set to missing if > 12 months}
##'		\item{"5b_24"}{set to missing if > 24 months}
##'		\item{"5c_6"}{set to 6 months if > 6 months}
##'		\item{"5c_12"}{set to 12 months if > 12 months}
##'		\item{"5c_24"}{set to 24 months if > 24 months}
##' }
##'
#' @return data.frame
#'
#' @export
dec5_clean_duration<-function(dataset1=NULL, decision)
{
  dataset1$new_duration<-round(dataset1$qty/dataset1$ndd)

  if(decision[5]=="5a"){
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[5]=="5b_6"){
    dataset1$new_duration[dataset1$new_duration>183]<-NA
    dataset1$numdays[dataset1$numdays>183]<-NA
    dataset1$dose_duration[dataset1$dose_duration>183]<-NA

  }
  else if (decision[5]=="5b_12"){
    dataset1$new_duration[dataset1$new_duration>365]<-NA
    dataset1$numdays[dataset1$numdays>365]<-NA
    dataset1$dose_duration[dataset1$dose_duration>365]<-NA

  }
  else if (decision[5]=="5b_24"){
    dataset1$new_duration[dataset1$new_duration>730]<-NA
    dataset1$numdays[dataset1$numdays>730]<-NA
    dataset1$dose_duration[dataset1$dose_duration>730]<-NA

  }
  else if (decision[5]=="5c_6"){
    dataset1$new_duration[dataset1$new_duration>183]<-183
    dataset1$numdays[dataset1$numdays>183]<-183
    dataset1$dose_duration[dataset1$dose_duration>183]<-183

  }
  else if (decision[5]=="5c_12"){
    dataset1$new_duration[dataset1$new_duration>365]<-365
    dataset1$numdays[dataset1$numdays>365]<-365
    dataset1$dose_duration[dataset1$dose_duration>183]<-183

  }
  else if (decision[5]=="5c_24"){
    dataset1$new_duration[dataset1$new_duration>730]<-730
    dataset1$numdays[dataset1$numdays>730]<-730
    dataset1$dose_duration[dataset1$dose_duration>730]<-730

  }
  return(dataset1)
}

#' Stop date
#' Select which stop date to use
#'
#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"6a"}{stop1 (start + numdays)}
##'	\item{"6b"}{stop2 (start + dose_duration)}
##'	\item{"6c"}{stop3 (start + qty/ndd)}
##'				}
##'
#' @return data.frame
#'
#' @export
dec6_select_stop_date<-function(dataset1=NULL, decision)
{
  dataset1$start<-data.table::as.IDate(dataset1$event_date)
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
#'
#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##'\itemize{
##' \item{"7a"}{Leave as missing, drop prescription}
##'	\item{"7b"}{Use mean for that drug for that individual}
##'	\item{"7c"}{Use mean for that drug for all individuals}
##'	\item{"7d"}{Use individual mean for that drug but if not available use population mean}
##'
##' }
#' @return data.frame
#'
#' @export
dec7_missing_stop_date<-function(dataset1=NULL, decision)
{
  #patid<-prodcode<-real_stop<-start<-new_duration<-NULL

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
#'
#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##' \item{"8a"}{do nothing}
##'  \item{"8b"}{use mean ndd and mean length}
##'  \item{"8c"}{choose prescription with smallest ndd}
##'  \item{"8d"}{choose prescription with largest ndd}
##'  \item{"8e"}{choose shortest prescription}
##'  \item{"8f"}{choose longest prescription}
##'  \item{"8g"}{sum durations}
##' }
##'
#' @return data.frame
#'
#' @export
dec8_multipleprescription_same_start_date<-function(dataset1=NULL, decision)
{
  #patid<-prodcode<-start<-real_stop<-ndd<-max_ndd<-min_ndd<-min_stop<-max_stop<-new_duration<-NULL

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
#'
#'

#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##'  \item{"9a"}{do nothing, allow prescriptions to overlap (implicity sum doses)}
##'  \item{"9b"}{move later prescription to next available time that this product is not prescribed}
##' }
##'
#' @return data.frame
#'
#' @export
dec9_overlaping_prescription<-function(dataset1=NULL, decision)
{

  #patid<-start<-prodcode<-real_stop<-ndd<-id.x<-id.y<-start.x<-start.y<-end.x<-end.y<-end<-NULL

  if(decision[9]=="9a"){
    dataset1$start<-data.table::as.IDate(dataset1$start)#Isolateoverlap creates two new column named start and stop while resevring the orginal
    dataset1$real_stop<-data.table::as.IDate(dataset1$real_stop)#Isolateoverlap creates two new column named start and stop while resevring the orginal

    dataset1 <- dataset1 %>%
      reshape::rename(rstart = start, rreal_stop=real_stop)#to aviod naming confilict with interval_vars_out

    dataset1 <- intervalaverage::isolateoverlaps(dataset1,interval_vars=c("rstart","rreal_stop"),group_vars=c("patid","prodcode"), interval_vars_out = c("start", "real_stop"))
    #do nothing- sum overlaping doses and remove duplicate
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(sum_ndd=sum(ndd, na.rm=T))
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1$ndd<-dataset1$sum_ndd # do we need to check for implusibility
    #dataset1<-dataset1[,-c("sum_ndd")]
    dataset1$real_stop<-as.POSIXct(dataset1$real_stop)

  }
  else if (decision[9]=="9b"){


    shift_interval <- function(x) {
      i <- 0
      y <- x
      overlap <- TRUE
      while (overlap) {

        i <- i + 1
        overlaps <- y %>%
          dplyr::mutate(dummy = 1) %>%
          dplyr::full_join(., ., by = 'dummy') %>%
          dplyr::filter(id.x < id.y) %>%
          dplyr::mutate(overlap = DescTools::Overlap(cbind(start.x, end.x),
                                              cbind(start.y, end.y))) %>%
          dplyr::filter(overlap > 0)
        y <- overlaps %>%
          dplyr::select(id = id.y, overlap) %>%
          dplyr::slice(1) %>%
          dplyr::right_join(y, by = 'id') %>%
          dplyr::mutate(overlap = replace(overlap, is.na(overlap), 0),
                        start = start + overlap,
                        end = end + overlap) %>%
          dplyr::select(-overlap)
        overlap <- nrow(overlaps)
      }
      list(y)
      return(y)
    }
    data.table::setDT(dataset1)
    dataset1$patid_prodcode<-paste0(dataset1$patid,dataset1$prodcode)
    dataset1<-reshape::rename(dataset1, c("start"="start", "real_stop"="end"))
    dataset1<-cbind(dataset1,do.call(rbind,lapply(1:length(unique(dataset1$patid_prodcode)), function(x) shift_interval(dataset1[dataset1$patid_prodcode==unique(dataset1$patid_prodcode)[x],]))))
  dataset1$real_stop<-as.POSIXct(dataset1$real_stop)
    }
  return(dataset1)

}

#' dec10: Handle sequential prescriptions with short gaps
#'

#' @param dataset1 a data frame containg prescription information
#' @param decision a character specifying the decison to consider for processing
##' \itemize{
##'  \item{"10a"}{do nothing}
##'  \item{"10b_15"}{change stop date of first prescription to start date of next if gap is <= 15 days}
##'  \item{"10b_30"}{change stop date of first prescription to start date of next if gap is <= 30 days}
##'  \item{"10b_60"}{change stop date of first prescription to start date of next if gap is <= 60 days}
##' }
##'
#' @return data.frame
#'
#' @export
dec10_gap_bn_prescription<-function(dataset1=NULL, decision)
{
  #patid<-start<-prodcode<-gap_to_next<-real_stop<-NULL

  dataset1<-data.table::setDT(dataset1)
  dataset1<-dataset1[order(patid,start, prodcode)]

  dataset1<-dataset1%>%
    dplyr::group_by(patid,prodcode) %>%
    dplyr::mutate(gap_to_next=dplyr::lead(start)-real_stop)

  if(decision[10]=="10a"){
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[10]=="10b"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=replace(real_stop,gap_to_next<16,dplyr::lead(start)))
  }
  else if (decision[10]=="10c"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=replace(real_stop,gap_to_next<31,dplyr::lead(start)))
  }
  else if (decision[10]=="10d"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=replace(real_stop,gap_to_next<61,dplyr::lead(start)))
  }

  return(dataset1)
}

#' freduce
#' modified version of reduce function in r
#'
#' @param value value
#' @param function_list list of decison nodes for processing
#' @param decision decision vector
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
#'
#' outer function to run all decisons. If you have implusible values to take action, run first the implusible_value function, else you need to choose 'do nothing' for dec1 and dec3
#'
#' @param dataset1 a data frame containing prescription information
#'
#' @param decisions a character vector containing list of decison to be taken
#'
#' @importFrom rlang .data

#' @return data.frame
#'
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
  decisions=decisions
  run_decisions<-ffreduce(dataset1, function_list, decisions)
  return(run_decisions)
}

