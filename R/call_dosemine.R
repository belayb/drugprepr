#' Compute ndd
#' This function calls the dosemining r package
#' and compute the ndd according to the suplemented decision
#'
#' @param dataset, decisons,
#' @return dataset
#' @export
#'
call_dosemine<-function(dataset1=NULL, decisions=NULL)
{

  # crate a dataset from dataset1 with unique dossageid
  temp_presc<-dataset1[!duplicated(dataset1[,c(dossageid)]),
                                           c("dossageid","text","dose_number","dose_unit","dose_frequency",
                                             "dose_interval","choice_of_dose","dose_max_average", "change_dose")]
  temp_presc<-doseminer::extract_from_prescription(temp_presc)
  dataset1<-left_join(dataset1,temp_presc,by="dossageid")

  return(dataset1)

}
