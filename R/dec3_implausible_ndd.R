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
  if(decision[3]=="3a")
  {
    #do nothing
    dataset1<-dataset1%>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd_new=ndd)%>%
      dplyr::ungroup()
  }
  else if (decision[3]=="3b"){
    dataset1<-dataset1%>%
      dplyr::rowwise() %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, NA,ndd_new))
  }
  else if (decision[3]=="3c1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MIPndd,ndd))
  }
  else if (decision[3]=="3c2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MPPndd,ndd))
  }
  else if (decision[3]=="3c3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MPndd,ndd))
  }
  else if (decision[3]=="3d1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MdIPndd,ndd))
  }
  else if (decision[3]=="3d2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(MdPPndd=median(ndd, na.rm = T), ndd_new=ifelse(implausible_ndd==1, MdPPndd,ndd))
  }
  else if (decision[3]=="3d3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MdPndd,ndd))
  }

  else if(decision[3]=="3e1")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MoIPndd,ndd))
  }

  else if(decision[3]=="3e2")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MoPPndd,ndd))
  }

  else if(decision[3]=="3e3")
  {

    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(ndd_new=ifelse(implausible_ndd==1, MoPndd,ndd))
  }
  return(dataset1)

}
