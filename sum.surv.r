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
    ests <- optim(c(1.5,1.5),obj, control=list( maxit=2000))#c(1.5,1.5) is  the bets set of param 
    k <- ests$par[1]
    lambda <- ests$par[2]
  }
  return(data.frame(shape=k,scale=lambda))
}

#try
#summDat<-read.csv("P:/FranWork/LupusDeath/Papers/Prop_surv_lag.csv")
#dim(summDat)
#head(summDat)
#weib.param(summDat[47:49,3:4],Lag=0)# Error in log(yr) : non-numeric argument to mathematical function 
#load('data/rda/summary_survival.rda')
#names(summary_survival)<-c('armID','pubID','number','Lag','Year','Prop')
#wb<-as.data.frame(matrix(999,61,2))
#names(wb)<-c("shape","scale")






#write a function to fill wb
#wb[25,]=wb[26,]=wb[27,]=wb[28,]<-weib.param(summary_survival[25:28,5:6],Lag=0)
#wb[47,]=wb[48,]=wb[49,]<-weib.param(summary_survival[47:49,5:6],Lag=0)
#wb[59,]=wb[60,]<-weib.param(summary_survival[59:60,5:6],Lag=6.95)
#wb[22,]<-weib.param(summary_survival[22,5:6],Lag=0)
#problem: most parameters are 1.5
# Error in model.frame.default(formula = log(-log(p)) ~ log(yr), drop.unused.levels = TRUE) : 
#variable lengths differ (found for 'log(yr)') 




#try simulation
#rweibull(70, 1.5, scale =1.5)


