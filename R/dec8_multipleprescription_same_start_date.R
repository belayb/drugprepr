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
      dplyr::mutate(mean_stop=mean(real_stop[!is.na(real_stop)]), mean_ndd=mean(ndd, na.rm=T))
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
