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
  dataset1<-dataset1[order(patid, event_date, prodcode)]

  dataset1 <- intervalaverage::isolateoverlaps(dataset1,interval_vars=c("start","real_stop"),group_vars=c("patid","prodcode"))
  #may need checking the column names. Isolateoverlap creates two new column named start and stop while resevring the orginal
  if(decision[9]=="9a"){
    #do nothing- sum overlaping doses and remove duplicate
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(sum_ndd=sum(ndd, na.rm=T))
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1$ndd<-dataset1$sum_ndd # do we need to check for implusibility
    dataset1<-dataset1[,-c("sum_ndd")]
  }
  else if (decision[8]=="8b"){

    #shift to next avaliable time - do recursive algorthim

  }



  return(dataset1)
}
