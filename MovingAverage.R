## Running moving average Bayesian analysis

source('lib/reload.R')
source('preamble.R')
reload()
load('data/rda/final_study_info.rda')
load('data/rda/fig_metadata.rda')
load('data/rda/window_membership.rda')
load('data/rda/KM2IPD.rda')
load('data/rda/summaries2IPD.rda')
load('data/rda/followup_data.rda')


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
p[i] <- pweib(maxfollowup[i]+Lag[i], nu[geog2[i]], lambda[geog2[i]])-pweib(Lag[i],nu[geog2[i]], lambda[geog2[i]]);
isCensored2[i] ~ dinterval(Y[i], Events[i]);
Y[i] ~ dbinom(p[i], n[i]);
}
}
"
writeLines(fullmodelcts.bugs, con='fullmodelcts.bug')

# Ignore follow-up data
fullmodelcts2.bugs <- "
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
# for(i in 1:N2){
# p[i] <- pweib(maxfollowup[i], nu[geog2[i]], lambda[geog2[i]])-pweib(lags[i],nu[geog2[i]], lambda[geog2[i]]);
# isCensored2[i] ~ dinterval(Y[i], Events[i]);
# Y[i] ~ dbinom(p[i], n[i]);
# }
}
"
writeLines(fullmodelcts2.bugs, con='fullmodelcts2.bug')

## Which are male-only studies
fig_metadata <- fig_metadata %>%
  mutate(male.only = ifelse(ids %in% study_info$pubID[study_info$male.only=='Y'],'Yes','No'))

ipds <- KM2IPD[setdiff(names(KM2IPD), fig_metadata$ids[fig_metadata$male.only=='Yes'])]
ipds <- c(ipds,
          summaries2IPD[setdiff(names(summaries2IPD), study_info$pubID[study_info$male.only=='Y'])])
## Use study_info since summaries2IPD contains data from spreadsheet, not just graphical
createDatasets(membership, ipds, outdir='adult')
