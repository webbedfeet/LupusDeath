# Follow-up data

source('preamble.R')
source('lib/reload.R')
reload()
load('data/rda/final_study_info.rda')
load('data/rda/KM2IPD.rda')
load('data/rda/summaries2IPD.rda')

ids <- setdiff(study_info$pubID, c(names(KM2IPD), names(summaries2IPD)))

fup_data <- study_info %>% filter(pubID %in% ids) %>% filter(male.only=='N') %>% 
  select(pubID, max.f.up, number, deaths, f.up.months, mean.median) %>% 
  mutate(n.death = round(number*deaths/100)) %>% 
  filter(!is.na(n.death) & !is.na(f.up.months))
