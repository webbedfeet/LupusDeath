# Empirical survival ------------------------------------------------------

source('lib/reload.R'); reload()
load('data/rda/final_study_info.rda')
load('data/rda/KM_digitized.rda')
load('data/rda/KM2IPD.rda')
load('data/rda/window_membership.rda')

id_developed <- study_info$pubID[study_info$Developed=='Developed']
id_developing <- study_info$pubID[study_info$Developed=='Developing']

tst1 <- KM2IPD[intersect(id_developed, names(KM2IPD))]
tst2 <- KM2IPD[intersect(id_developing, names(KM2IPD))]

empirical_surv <- function(ipd){
  s1 <- IPD2Surv(ipd)
  tidy(summary(survfit(s1~1),c(5,10,15)))
}

KM_deaths <- tibble(pubID = names(KM2IPD),
                    ndeath = sapply(KM2IPD, function(ipd) length(ipd$d.times)))
KM_Ns <- KM_full %>% select(ids,N) %>%
  # group_by(ids) %>% summarise(N = sum(N)) %>% ungroup() %>%
  left_join(KM_deaths, by=c('ids' = 'pubID')) %>%
  rename_('pubID'='ids')

dat1 <- plyr::ldply(lapply(tst1, empirical_surv),.id='pubID') %>%
  left_join(study_info %>% select(pubID, yr_of_study, end_of_study)) %>%
  distinct() %>% left_join(KM_Ns) %>%
  select(pubID, N, ndeath, yr_of_study, end_of_study, time, Survival) %>%
  arrange(yr_of_study)

dat2 <- plyr::ldply(lapply(tst2, empirical_surv),.id='pubID') %>%
  left_join(study_info %>% select(pubID, yr_of_study, end_of_study)) %>%
  distinct() %>% left_join(KM_Ns) %>%
  select(pubID, N, ndeath, yr_of_study, end_of_study, time, Survival) %>%
  arrange(yr_of_study)

library(openxlsx)
write.xlsx(list('Developed' = dat1, 'Developing'=dat2),
           file = 'docs/Empirical_Survival_from_KM.xlsx')

out <- plyr::ldply(list('Developed countries'=dat1, 'Developing countries'=dat2), .id='Dev') %>%
  mutate(Year=factor(paste(time,'year')),
         Med = Survival,
         yr = yr_of_study) %>% select(Year, Dev, Med,yr)
