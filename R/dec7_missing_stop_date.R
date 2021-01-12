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
      dplyr::mutate(Indmean_new_duration=mean(new_duration[!is.na(new_duration)]))
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode) %>%
      dplyr::mutate(real_stop=ifelse(is.na(real_stop), start+Indmean_new_duration,real_stop))
    dataset1<-dataset1[!is.na(dataset1$real_stop),]
  }
  else if (decision[7]=="7c"){
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(Prodmean_new_duration=mean(new_duration[!is.na(new_duration)]))
    dataset1<-dataset1%>%
      dplyr::group_by(prodcode) %>%
      dplyr::mutate(real_stop=ifelse(is.na(real_stop), start+Prodmean_new_duration,real_stop))
    dataset1<-dataset1[!is.na(dataset1$real_stop),]
  }
  return(dataset1)
  #i will add decison 7d latter
}
