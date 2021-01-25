#' Stop date
#' Select which stop date to use
#' options are	6a	stop1 (start + numdays)
#'		6b	stop2 (start + dose_duration)
#'		6c	stop3 (start + qty/ndd)
#'			If only one stop available, use it
#'			if two available and equal, use that date
#'			if two available and unequal (but within x days), use mean
#'			if three available and unequal, use mean of closest 2 if within x days
#'				6d_15	x = 15
#' 			6d_30	x = 30
#'				6d_60	x = 60
#'				6d_90	x = 90
#'				6e 	x = something very big, like 9,999,999
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec6_select_stop_date<-function(dataset1, decision)
{
  dataset1$start<-dataset1$event_date
  if(decision[6]=="6a"){
    #do nothing
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
