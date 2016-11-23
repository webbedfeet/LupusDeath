#' Converting a list of IPD into JAGS-compatible data
#' 
#' @param ipd The list of IPDs
#' @param info Other data, matched on some index with the names of the IPD list, that will be included
#'            in the JAGS dataset
gen_jagsdata <- function(ipd, info){
  data.jags <- list(td=NULL, tcens=NULL, trunc=NULL, isCensored=NULL, geog=NULL)
  data.jags <- data.frame(data.jags)
  n <- length(ipd)
  for(i in 1:n){
    dat = ipd[[i]]
    pubID = names(ipd)[i]
    study.info <- subset(info, pubID = pubID)
    t.d <- c(dat$d.times, rep(NA,length(dat$cens.times)))
    trunc <- rep(ifelse(is.na(study.info$Lag), 0, study.info$Lag), length(t.d))
    t.cens = c(rep(1000, length(dat$d.times)), dat$cens.times) # Need 1000 to be bigger than any times in ipd
    t.d <- t.d + trunc
    t.cens <- t.cens+trunc
    is.censored <- as.integer(is.na(t.d))
    geog = rep(study.info$Developed, length(t.d))
    bl <- data.frame(td=t.d, tcens=t.cens, trunc=trunc, isCensored=is.censored, geog=geog)
    data.jags <- rbind(data.jags, bl)
  }
  return(as.list(data.jags))
}
