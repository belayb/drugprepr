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
  dataset1<-setDT(dataset1)[order(patid, start, prodcode)]

  if(decision[9]=="9a"){
    dataset1$rstart<-dataset1$start#Isolateoverlap creates two new column named start and stop while resevring the orginal
    dataset1 <- intervalaverage::isolateoverlaps(dataset1,interval_vars=c("rstart","real_stop"),group_vars=c("patid","prodcode"))
    #do nothing- sum overlaping doses and remove duplicate
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(sum_ndd=sum(ndd, na.rm=T))
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1$ndd<-dataset1$sum_ndd # do we need to check for implusibility
    dataset1<-dataset1[,-c("sum_ndd")]
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

    dataset1<-setDT(dataset1)[order(patid, start, prodcode)]
    dataset1$patid_prodcode<-paste0(dataset1$patid,dataset1$prodcode)
    dataset1<-reshape:::rename(dataset1, c("start"="start", "real_stop"="end"))
    dataset1<-cbind(dataset1,do.call(rbind,lapply(1:length(unique(dataset1$patid_prodcode)), function(x) shift_interval(dataset1[dataset1$patid_prodcode==unique(dataset1$patid_prodcode)[x],]))))
    }
  return(dataset1)

}
