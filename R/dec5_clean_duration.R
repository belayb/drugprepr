#' Clean  duration
#'
#' This function acepts a data frame containg prodcode, qty, and ndd and
#' compute duration and replace duration by  by one of the following options if they are longer than clinically plusible.
#' options are	5a	leave duration as it is
#'		5b_6	set to missing if > 6 months
#'		5b_12	set to missing if > 12 months
#'		5b_24	set to missing if > 24 months
#'		5c_6	set to 6 months if > 6 months
#'		5c_12	set to 12 months if > 12 months
#'		5c_24	set to 24 months if > 24 months
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec5_clean_duration<-function(dataset1, decision)
{
  dataset1$new_duration<-round(dataset1$qty/dataset1$ndd)

  if(decision[5]=="5a"){
    #do nothing
    dataset1<-dataset1
  }
  else if (decision[4]=="5b_6"){
    dataset1$new_duration[dataset1$new_duration>183]<-NA
    dataset1$numdays[dataset1$numdays>183]<-NA
    dataset1$dose_duration[dataset1$dose_duration>183]<-NA

  }
  else if (decision[4]=="5b_12"){
    dataset1$new_duration[dataset1$new_duration>365]<-NA
    dataset1$numdays[dataset1$numdays>365]<-NA
    dataset1$dose_duration[dataset1$dose_duration>365]<-NA

  }
  else if (decision[4]=="5b_24"){
    dataset1$new_duration[dataset1$new_duration>730]<-NA
    dataset1$numdays[dataset1$numdays>730]<-NA
    dataset1$dose_duration[dataset1$dose_duration>730]<-NA

  }
  else if (decision[4]=="5c_6"){
    dataset1$new_duration[dataset1$new_duration>183]<-183
    dataset1$numdays[dataset1$numdays>183]<-183
    dataset1$dose_duration[dataset1$dose_duration>183]<-183

  }
  else if (decision[4]=="5c_12"){
    dataset1$new_duration[dataset1$new_duration>365]<-365
    dataset1$numdays[dataset1$numdays>365]<-365
    dataset1$dose_duration[dataset1$dose_duration>183]<-183

  }
  else if (decision[4]=="5c_24"){
    dataset1$new_duration[dataset1$new_duration>730]<-730
    dataset1$numdays[dataset1$numdays>730]<-730
    dataset1$dose_duration[dataset1$dose_duration>730]<-730

  }
  return(dataset1)
}
