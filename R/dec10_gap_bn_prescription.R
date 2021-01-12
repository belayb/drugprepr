#' dec10: Handle sequential prescriptions with short gaps
#' options are	10a	Do nothing
#'			change stop date of first prescription to start date of next if gap is <=
#'			10b_15  15 days
#'			10b_30  30 days
#'			10b_60  60 days
#'
#' @param dataset, decsion
#' @return dataset
#' @export
dec10_gap_bn_prescription<-function(dataset1, decision)
{
  return(dataset1)
}
