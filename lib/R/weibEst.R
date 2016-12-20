weibEst <- function(d){
  d <- d %>% filter(Time > 0, Time <=15)
  m <- lm(log(-log(Prob))~log(Time), data=d[-1,])
  out = c(shape = m$coef[2], scale = exp(-m$coef[1]/m$coef[2]))
  names(out) <- c('shape','scale')
  return(out)
}