#' Compute year the study begins
#' 
#' Compute the start year of each study, in case it is missing
beginYear <- function(dat=study_info){
  starts <- dat$start
  ind <- which(is.na(starts))
  starts[ind] <- with(dat[ind,], round(pubdate - pmax(maxfollowup, esrdfu..m.,na.rm=T)/12-1))
  ind <- which(is.na(starts)) # just arms of 118
  starts[ind] <- starts[min(ind)-1]
  return(starts)
}
