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
