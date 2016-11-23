#' Creating data for import into JAGS
#' 
#' This function inputs data from the survival studies, including IPD data and 
#' followup data, and converts it into the R dump() format for import into
#' JAGS 
#' 
#' @param ipd This is a list of IPD data, one per study. Each element is
#'            itself a list comprising 2 elements, t.d (death) and t.cens (censored) times
#' 
#' @param  follow A data.frame that contains information from studies that 
#'                contains only followup time and number of events, in addition
#'                to some study characteristics (not currently implemented)
#' @param  info A data.frame that contains information about each study (study_info for this study)
#' @return A list in R dump() format using all the input data that is amenable for
#'         processing by JAGS
#' 
datForJags <- function(ipd, follow=NULL, info=study_info){
  # out <- follow[,c('number','maxfollowup','Events','Developed',
  #                  'lags')]
  dat.jags <- as.data.frame(gen_jagsdata(ipd,info))
  # names(out)[c(1,4)] <- c('n','geog2')
  # out$isCensored2 <- as.integer(rep(1,nrow(out)))
  # out$Y <- rep(NA, nrow(out))
  #   out$yr2 <- as.factor(out$yr2)
  # out$geog2 <- as.factor(out$geog2)
  
  # dat <- as.list(out)
  # dat$N2 <- nrow(out)
  names(dat.jags) <- c('td','tcens','trunc','isCensored1','geog1')
  # dat.jags <- dat.jags[,-6]
  #   dat.jags$yr1 <- as.factor(dat.jags$yr1)
  dat.jags$geog1 <- as.factor(dat.jags$geog1)
  
  # dat <- c(dat, as.list(dat.jags))
  dat <- as.list(dat.jags)
  dat$N1 <- nrow(dat.jags)
  for(i in 1:length(dat)){
    if(is.factor(dat[[i]])) dat[[i]] <- as.numeric(dat[[i]])
  }
  # dat$J <- length(unique(c(dat$geog1,dat$geog2)))
  dat$J <- length(unique(c(dat$geog1)))
  #   dat$K <- length(unique(c(dat$yr1,dat$yr2)))
  return(dat)
}
