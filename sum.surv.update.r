# Functions for summary survival data -------------------------------------
setwd("P:/FranWork/LupusDeath")
fn.env <- new.env()
source('P:/FranWork/LupusDeath/truncFunctions.R', local=fn.env)


## Determining the appropriate weibull parameters  
weibull_estimation <- function(summData){  
yr <- summData$year  
p <- summData$surv_perc  
Lag <- unique(summData$Lag)  
 
  if(all(p > 1)) p <- p/100 # convert to probabilities  
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
  
qtrunc <- function(p, spec, a = -Inf, b = Inf, ...)
{
  tt <- p
  G <- get(paste("p", spec, sep = ""), mode = "function")
  Gin <- get(paste("q", spec, sep = ""), mode = "function")
  tt <- Gin(G(a, ...) + p*(G(b, ...) - G(a, ...)), ...)
  return(tt)
}
#try 
#summDat<-read.csv(file="try.csv", header=TRUE, sep=",") 
#weibull_estimation(summDat[10:11,]) 

#load rda
load('data/rda/summary_survival.rda')

#create wb param storage
wb<-as.data.frame(matrix(999,61,2))
names(wb)<-c("shape","scale")

#wb[50,]=wb[51,]=wb[52,]=wb[53,]<-weibull_estimation(summary_survival[50:53,4:6])
#wb[59,]=wb[60,]=wb[61,]<-weibull_estimation(summary_survival[59:61,4:6])
#wb[44,]=wb[45,]<-weibull_estimation(summary_survival[44:46,4:6])
#wb[58,]<-weibull_estimation(summary_survival[58,4:6])

#combine summmary and estimated wb param
summDat<-cbind(summary_survival,wb)
write.csv(summDat,file="summary_survival.csv")


#=====sim weibull data================
rweibull(185, 0.03457368, 2.797611)

#==============create lifetime and censortime=====
censtime<-runif(185*.113,0,216/12)#length=20
lifetime<-rweibull(185*.113,0.03457368, 2.797611)#20
dat1<-cbind(lifetime,censtime)
sort(lifetime)
hist(lifetime)
censtime
sort(censtime)
hist(censtime)
