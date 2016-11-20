# This file will contain updates to various data sets that are resulting
# from data cleaning exercises

load('data/rda/study_info.rda')

## Estes' figures provide only summary survival data and not KM curves
study_info$KM.fig[study_info$Author=='Estes'] <- '0'

## Identify male only studies, which will be kept separate
male_only <- study_info %>% filter(female==0) %>% 
  select(pubID) %>% left_join(study_info %>% count(pubID)) %>% 
  filter(n==1)
study_info <- study_info %>% 
  mutate(male.only = factor(ifelse(pubID %in% male_only$pubID, 'Y','N')))

## Identify arms that represent full studies

bl <- study_info %>% filter(armID==pubID) %>% select(pubID)
study_info <- study_info %>% 
  mutate(fullstudy_arm = factor(ifelse(pubID %in% bl$pubID, 'Y','N')))

## Compute study year as middle of enrollment

bl <- study_info %>% 
  mutate(yr_of_study = round((start.enrollment + pmin(end.enrollment, end.fup, na.rm=T))/2),
         yr_of_study = ifelse(!is.na(yr_of_study), yr_of_study,
                              round((pubdate-1-start.enrollment)/2)),
         yr_of_study = ifelse(!is.na(yr_of_study), yr_of_study,
                              round(pmin(end.fup,pubdate-1, na.rm=T)-
                                      pmax(f.up.months,as.numeric(max.f.up), na.rm=T)/12/2))
  )

study_info <- study_info %>% left_join(bl %>% select(armID,yr_of_study))

## Identify developed status
