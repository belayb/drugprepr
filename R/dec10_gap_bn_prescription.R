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
  dataset1<-setDT(dataset1)
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
