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
  if(decision[2]=="2a")
  {
    #do nothing
    dataset1<-dataset1

  }
  else if (decision[2]=="2b1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MIPQ,qty_new))
  }
  else if (decision[2]=="2b2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MPPQ,qty_new))
  }
  else if (decision[2]=="2b3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MPQ,qty_new))
  }

  else if (decision[2]=="2c1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MdIPQ,qty_new))
  }
  else if (decision[2]=="2c2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MdPPQ,qty_new))
  }
  else if (decision[2]=="2c3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MdPQ,qty_new))
  }

  else if (decision[2]=="2d1"){
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MoIPQ,qty_new))
  }
  else if (decision[2]=="2d2"){
    dataset1<-dataset1%>%
      dplyr::group_by(pracid,prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MoPPQ,qty_new))
  }

  else if (decision[2]=="2d3"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(qty_new=ifelse(is.na(qty_new), MoPQ,qty_new))
  }
  return(dataset1)
}
