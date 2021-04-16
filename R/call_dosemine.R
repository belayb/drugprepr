#' Compute ndd
#' This function calls the dosemining r package
#' and compute the ndd according to the suplemented decision
#'
#' @param dataset, decison,
#' @return dataset
#' @export
#'
call_dosemine<-function(dataset1=NULL, decision=NULL)
{

  # crate a dataset from dataset1 with unique dossageid
  temp_presc<-dataset1[!duplicated(dataset1[,c("dossageid")]),
                                           c("dossageid","text")]
  temp_presc<-cbind(temp_presc[,"dossageid"],doseminer::extract_from_prescription(temp_presc$text)%>%
                      splitstackshape::cSplit(c("freq","dose"), sep="-", direction = "wide")%>%
                      rowwise() %>% mutate(freq_2=ifelse(is.na(freq_2),freq_1, freq_2),
                                           dose_2=ifelse(is.na(dose_2),dose_1, dose_2),
                                           DF_mean=mean(c(freq_1, freq_2), na.rm=T),
                                           DN_mean=mean(c(dose_1, dose_2), na.rm=T),
                                           DF_min=freq_1,DF_max=freq_2,
                                           DN_min=dose_1,DN_max= dose_2) %>%
                      select(c(optional,DF_mean:DN_max)))
  names(temp_presc)[1]<-"dossageid"
  dataset1<-left_join(dataset1,temp_presc,by="dossageid")
  dataset1<-dataset1%>%rowwise()%>%mutate(ndd1=DN_min*DF_min, ndd2=DN_min*DF_mean, ndd3=DN_min*DF_max, ndd4=DN_mean*DF_min, ndd5=DN_mean*DF_mean,
                                         ndd6=DN_mean*DF_max, ndd7=DN_max*DF_min, ndd8=DN_max*DF_mean, ndd9=DN_max*DF_max)#you have to move this computation to the ifelse section

  if(decision=="min_min"){
    dataset1$ndd<-dataset1$ndd1
  }
  else if (decision=="min_mean"){
    dataset1$ndd<-dataset1$ndd2
  }
  else if (decision=="min_max"){
    dataset1$ndd<-dataset1$ndd3
  }
  else if (decision=="mean_min"){
    dataset1$ndd<-dataset1$ndd4
  }
  else if (decision=="mean_mean"){
    dataset1$ndd<-dataset1$ndd5
  }

  else if (decision=="mean_max"){
    dataset1$ndd<-dataset1$ndd6
  }
  else if (decision=="max_min"){
    dataset1$ndd<-dataset1$ndd7
  }

  else if (decision=="max_mean"){
    dataset1$ndd<-dataset1$ndd8
  }
  else if (decision=="max_max"){
    dataset1$ndd<-dataset1$ndd8
  }

  return(dataset1)

}

