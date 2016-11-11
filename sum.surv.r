# Functions for summary survival data -------------------------------------
setwd("P:/FranWork/LupusDeath")
fn.env <- new.env()
source('P:/FranWork/LupusDeath/truncFunctions.R', local=fn.env)

## Determining the appropriate weibull parameters
weib.param <- function(summData, Lag=0){
  yr <- summData$Year; p <- summData$Prob
  if(length(yr)==1){
    yr <- c(0.1,yr); p <- c(0.999,p)
  }
  p = pmax(0.001, pmin(0.999,p))
  if(Lag==0){ # Use Weibull plot method
    #ind <- (p>0 & p<1);yr <- yr[ind];p <- p[ind]
    m <- lm(log(-log(p))~log(yr))
    k <- m$coef[2]
    lambda <- exp(-m$coef[1]/k)
  } else { # Use optimization
    obj <- function(params){sum((log(1-p) - log(ptrunc(as.numeric(yr+Lag),'weibull',a = as.numeric(Lag),shape=params[1],scale=params[2])))^2)}
    ests <- optim(c(1.5,1.5),obj, control=list( maxit=2000))
    k <- ests$par[1]
    lambda <- ests$par[2]
  }
  return(data.frame(shape=k,scale=lambda))
}

#try
summDat<-read.csv(file="try.csv", header=TRUE, sep=",")
weib.param(summDat[10:11,1:2],Lag=2.6)

rweibull(107, -.000000000000003, scale =0)

