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
