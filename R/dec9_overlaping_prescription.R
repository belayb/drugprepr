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
  dataset1$rstart<-dataset1$start#Isolateoverlap creates two new column named start and stop while resevring the orginal
  dataset1 <- intervalaverage::isolateoverlaps(dataset1,interval_vars=c("rstart","real_stop"),group_vars=c("patid","prodcode"))
  if(decision[9]=="9a"){
    #do nothing- sum overlaping doses and remove duplicate
    dataset1<-dataset1%>%
      dplyr::group_by(patid,prodcode,start) %>%
      dplyr::mutate(sum_ndd=sum(ndd, na.rm=T))
    dataset1<-dataset1[ !(duplicated(dataset1[, c("patid","prodcode","start")])),]
    dataset1$ndd<-dataset1$sum_ndd # do we need to check for implusibility
    dataset1<-dataset1[,-c("sum_ndd")]
  }
  else if (decision[8]=="8b"){

    #The method followed in stata is so heavy - it converts the data to a big data where each row corrsponds to a day
    # Start simple - play with this data
    #artificial data to play with - x2
    x2 <- data.table(patid=rep(1:2,each=5),
                     exposure_start=rep(c(1L,5L,10L,26L,31L),times=2),
                     exposure_end=rep(c(7L,14L,18L,30L,40L),times=2),
                     exposure_value=c(round(runif(10, min = 1, max = 10),digits=0)))

    x2z <- isolateoverlaps(x2,interval_vars=c("exposure_start","exposure_end"),group_vars=c("patid"))
    x2z

    # Approch one----if there is space enough to accomodate the overlap do it if not ignore it for now
    # Step-1: Isolate
    # Step-2: Generate a row indictor by patid, prodcode, start
    # Step-3: compuate new duration stop-start
    # Step-4: compute the gap between each conscutative rows by patid
    # Step-5: If gap  less than the duration of the first overlap and have a prodcode similar to the previous one insert
    # do the above step for all overlaps
    x2z<-x2z[, presc_id := rowid(patid,start)]
    x2z<-x2z[, new_duration := end-start]
    x2z<-x2z[, gap := end-shift(start, 1, type="lead"), by=patid]



  }



  return(dataset1)
}
