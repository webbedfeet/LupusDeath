## Running moving average Bayesian analysis

source('lib/reload.R')
source('preamble.R')
reload()
load('data/rda/study_info.rda')
load('data/rda/KM2IPD.rda')
load('data/rda/fig_metadata.rda')
load('data/rda/window_membership.rda')


fullmodelcts.bugs <- "
model{
# Prior model & estimated quantities
for(j in 1:J){
#beta[j] ~ dnorm(0.0,0.0001);
nu[j] ~ dgamma(1.0,0.0001);
lambda[j] ~ dgamma(1.0,0.0001);
pr5[j] <- 1-pweib(5.0, nu[j],lambda[j]);
pr10[j] <- 1-pweib(10.0, nu[j],lambda[j]);
pr15[j] <- 1-pweib(15.0, nu[j],lambda[j]);
}
# Likelihood for IPD data
for(i in 1:N1){
isCensored1[i] ~ dinterval(td[i], tcens[i]);
td[i] ~ dweib(nu[geog1[i]], lambda[geog1[i]])T(trunc[i],);
}
# Likelihood for followup data
for(i in 1:N2){
p[i] <- pweib(maxfollowup[i], nu[geog2[i]], lambda[geog2[i]])-pweib(lags[i],nu[geog2[i]], lambda[geog2[i]]);
isCensored2[i] ~ dinterval(Y[i], Events[i]);
Y[i] ~ dbinom(p[i], n[i]);
}
}
"
writeLines(fullmodelcts.bugs, con='fullmodelcts.bug')

fullmodelcts2.bugs <- "
model{
# Prior model & estimated quantities
for(j in 1:J){
beta[j] ~ dnorm(0.0,0.0001);
nu[j] ~ dgamma(1.0,0.0001);
lambda[j] <- exp(beta[j]);
pr5[j] <- 1-pweib(5.0, nu[j],lambda[j]);
pr10[j] <- 1-pweib(10.0, nu[j],lambda[j]);
pr15[j] <- 1-pweib(15.0, nu[j],lambda[j]);
}
# Likelihood for IPD data
for(i in 1:N1){
isCensored1[i] ~ dinterval(td[i], tcens[i]);
td[i] ~ dweib(nu[geog1[i]], lambda[geog1[i]])T(trunc[i],);
}
# Likelihood for followup data
# for(i in 1:N2){
# p[i] <- pweib(maxfollowup[i], nu[geog2[i]], lambda[geog2[i]])-pweib(lags[i],nu[geog2[i]], lambda[geog2[i]]);
# isCensored2[i] ~ dinterval(Y[i], Events[i]);
# Y[i] ~ dbinom(p[i], n[i]);
# }
}
"
writeLines(fullmodelcts2.bugs, con='fullmodelcts2.bug')


ipds <- KM2IPD[setdiff(names(KM2IPD), fig_metadata$ids[fig_metadata$male.only=='Yes'])]
createDatasets(membership, ipds, outdir='adult')
