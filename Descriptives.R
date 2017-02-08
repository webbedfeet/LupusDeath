# Descriptives

source('lib/reload.R')
reload()
source('preamble.R')

info <- study_info %>% filter(male.only=='N') %>% filter(armID==pubID)

info %>% summarise(perc_female = weighted.mean(female, number, na.rm=T),
                   age = weighted.mean(as.numeric(str_extract(Age, '^[0-9\\.]+')),number, na.rm=T),
                   fup = weighted.mean(f.up.months, number, na.rm=T))

info %>% group_by(Developed) %>% summarise(age = weighted.mean(as.numeric(str_extract(Age,'^[0-9]+')),number, na.rm=T))

## Summary survival from 2008-2014

target = seq(2008,2014)
# Generate study_years from DataMunging.R
studies_in_target <- sapply(study_years, function(x) length(intersect(x, target))>0)
studies_in_target <- names(studies_in_target)[studies_in_target]
# studies_in_target <- setdiff(studies_in_target, 'Lopez_2012')

load('data/rda/KM2IPD.rda')
load('data/rda/summaries2IPD.rda')
load('data/rda/followup_data.rda')

ipd <- c(KM2IPD, summaries2IPD)
members <- intersect(names(ipd), studies_in_target)
ipd1 <- ipd[members]
fup1 <- fup_data %>% filter(pubID %in% studies_in_target)
d <- datForJags(ipd1, fup1, info=study_info)
dump('d', file='data/mcmc/summary.txt')

# Inception cohorts
study_in_target_inception=info %>% filter(pubID %in% studies_in_target) %>%
  filter(inception==1) %>% select(pubID) %>% as_vector()
members = intersect(names(ipd), study_in_target_inception)
ipd2 = ipd[members]
fup2 = fup_data %>% filter(pubID %in% study_in_target_inception)
d = datForJags(ipd2, fup2, info = study_info)
dump('d', file='data/mcmc/summary_inception.txt')
