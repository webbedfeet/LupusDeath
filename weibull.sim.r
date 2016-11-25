rweibull(107, -.000000000000003, scale =0)

#Example: 2.2; 3+; 8.4; 7.5+. censored at 3 and 7.5
stime <- c(2.2, 3, 8.4, 7.5)
status <- c(1,0,1,0)
Surv(stime, status)
#To simulate the (randomly right) censored observations, we need to 
#simulate a lifetime vector and, independently, 
#a termination time (=censoring time) vector. Then we observe
#whichever comes first (ztimes) and related indicator (status).
lifetimes <- rexp( 25, rate = 0.2)
censtimes <- 5 + 5*runif(25)
ztimes <- pmin(lifetimes, censtimes)
status <- as.numeric(censtimes > lifetimes)
survfit(Surv(stime,status)~x,data)
summary(survfit(Surv(stime,status)))


#T <- rweibull(3, shape=.5, scale=1)
#censoring_time <- 0.88
#T_censored <- min(censoring_time, T)


set.seed(0775)  
t    = rweibull(3, shape=.5, scale=1)
t      # [1] 0.7433678 1.1325749 0.2784812
c    = rweibull(3, shape=.5, scale=1.5)
c      # [1] 3.3242417 2.8866217 0.9779436
time = pmin(t, c)
time   # [1] 0.7433678 1.1325749 0.2784812
cens = ifelse(c<t, 1, 0)
cens   # [1] 0 0 0