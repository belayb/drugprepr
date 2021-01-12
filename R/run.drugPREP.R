#' run drug prep
#' outer function to run all decisons
#' If you have implusible values to take action, run first the implusible_value function
#' else you need to choose 'do nothing' for dec1 and dec3
#'
#' @param dataset, decisons,
#' @return dataset
#' @export
run.drugPREP<-function(dataset1=NULL, decisions=NULL)
{
  #check if nessary columns are in the input data
  must_names<-c("patid","pracid","event_date","prodcode","qty","ndd","numdays","dose_duration")
  stopifnot(must_names%in%names(dataset1))

  function_list=list(dec1_impausible_qty, dec2_missing_qty,dec3_implausible_ndd,dec4_missing_ndd,
                     dec5_clean_duration, dec6_select_stop_date, dec7_missing_stop_date,
                     dec8_multipleprescription_same_start_date, dec9_overlaping_prescription,
                     dec10_gap_bn_prescription)
  decision=decision
  run_decisions<-ffreduce(dataset1, function_list, decision)
  return(run_decisions)
}
