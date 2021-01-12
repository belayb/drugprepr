#' Define implusible
#' ideally create a shiny that will be populated by the user with min max values
#' That is data set Immp_val will be generated from dataset1 by removing duplicate productnames
#' and will have column Prdductcode, productname, qty_min, qty_max, ndd_min, ndd_max
#' The last 4 columns will be populated by the user

#' @param dataset
#' @return dataset,decision
#' @export
Implusible_values<-function(dataset1=NULL,min_max_dat=NULL)
{
  if(is.null(dataset1)) {stop("\n Data set with information from therapy file of CPRD has to be supplimented")}
  if(is.null(min_max_dat)) {stop("\n Data set with information including qty_max, qty_min, min_rec_ndd, max_rec_ndd to be supplimented")}

  dataset1<-left_join(dataset1,min_max_dat, by="prdoductname")
  dataset1<-dataset1%>%
    dplyr::rowwise() %>%
    dplyr::mutate(implausible_qty=(qty>qty_max|qty<qty_min)&!is.na(qty),
                  implausible_ndd=(ndd>max_rec_ndd|ndd<min_rec_ndd)&!is.na(ndd))%>%
    dplyr::ungroup()

  return(dataset1)
}
