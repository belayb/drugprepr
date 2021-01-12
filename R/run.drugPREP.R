#' run drug prep
#' outer function to run all decisons
#' some of the computation inside this function will be taken out
#'
#' @param dataset, decisons,
#' @return dataset
#' @export
run.drugPREP<-function(dataset1=NULL, decisions=NULL)
{
  #check if nessary columns are in the input data
  must_names<-c("patid","pracid","prodcode","qty")# update this with all names
  stopifnot(must_names%in%names(dataset1))

  mod_fun <- function(x) unique(x)[which.max(table(x))]
  dataset1<-dataset1%>%
    dplyr::rowwise() %>%
    dplyr::mutate(implausible_qty=(qty>qty_max|qty<qty_min)&!is.na(qty),
           implausible_ndd=(ndd>max_rec_ndd|ndd<min_rec_ndd)&!is.na(ndd))%>%
    dplyr::ungroup()

  dataset1<-dataset1%>%
    dplyr::group_by(patid,prodcode) %>%
    dplyr::mutate(MIPQ=mean(qty[!is.na(qty)]),MdIPQ=median(qty[!is.na(qty)]),
           MoIPQ=mod_fun(qty[!is.na(qty)]),MIPndd=mean(ndd[!is.na(ndd)]),
           MdIPndd=median(ndd[!is.na(ndd)]),MoIPndd=mod_fun(ndd[!is.na(ndd)]))

  dataset1<-dataset1%>%
    dplyr::group_by(pracid,prodcode) %>%
    dplyr::mutate(MPPQ=mean(qty[!is.na(qty)]),MdPPQ=median(qty[!is.na(qty)]),
           MoPPQ=mod_fun(qty[!is.na(qty)]),MPPndd=mean(ndd[!is.na(ndd)]),
           MdPPndd=median(ndd[!is.na(ndd)]),MoPPndd=mod_fun(ndd[!is.na(ndd)]))

  dataset1<-dataset1%>%
    dplyr::group_by(prodcode) %>%
    dplyr::mutate(MPQ=mean(qty[!is.na(qty)]),MdPQ=median(qty[!is.na(qty)]),
           MoPQ=mod_fun(qty[!is.na(qty)]),MPndd=mean(ndd[!is.na(ndd)]),
           MdPndd=median(ndd[!is.na(ndd)]),MoPndd=mod_fun(ndd[!is.na(ndd)]))

  function_list=list(dec1_impausible_qty, dec2_missing_qty,dec3_implausible_ndd,dec4_missing_ndd,
                     dec5_clean_duration, dec6_select_stop_date, dec7_missing_stop_date,
                     dec8_multipleprescription_same_start_date, dec9_overlaping_prescription,
                     dec10_gap_bn_prescription)
  decision=decision
  run_decisions<-ffreduce(dataset1, function_list, decision)
  return(run_decisions)
}
