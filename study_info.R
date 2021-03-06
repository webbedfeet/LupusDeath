# Extracting information from Excel file

source('lib/reload.R')
reload()
source('preamble.R')

datadir <- AD_local_mac['popdata']
# datadir <- FH_dirs['popdata']
study_info <- read_excel(file.path(datadir,'double-checked_adult_Abhijit.xlsx'))[1:187, 1:43]
study_info$Author[study_info$Author=='Al Arfaj'] <- 'Al-Arfaj'

## Fix names of variables
names(study_info) <- str_trim(names(study_info))
study_info <- study_info %>%
  dplyr::rename(surv15yr = `surv15 y`,
         Time0 = `Time 0`) %>% 
  select(-study)
names(study_info) <- make.names(names(study_info)) %>% 
  str_replace('\\.+$','') %>%
  str_replace_all( '\\.+','.')

## Cleaning data
study_info <- study_info %>%
  mutate_if(is.character, str_trim) %>% # get rid of trailing spaces
  mutate(Author = Author %>% str_trim() %>% str_to_title() %>% 
           str_extract('^[\\w-]+'),
         Arm = ifelse(is.na(Arm),'',Arm),
         armID = paste(Author, pubdate, Arm, sep='_') %>% 
           str_replace('_$','') %>% str_replace(' ','_'), # create unique ID per arm
         pubID = paste(Author, pubdate, sep='_'), # create publication ID
         pubID = ifelse(str_detect(Arm,'^[abc]$'), # separate out time-separated arms
                        paste0(pubID,Arm), pubID),
         armID = ifelse(str_detect(Arm,'^[abc]$'),
                        pubID, armID),
         dis.dur.yrs = as.numeric(str_replace(dis.dur.yrs, '<', '')),
         Time0 = ifelse(inception==1,'diagnosis', Time0) %>% tolower()) %>%  # Account for inception cohorts
  nest(-pubID) %>% 
  mutate(data = map(data, ~mutate(., dis.dur.yrs = fillin(dis.dur.yrs)))) %>% 
  unnest() %>% 
  mutate(
         Lag = ifelse(Time0=='diagnosis', 0, dis.dur.yrs), # create Lag variable
         Lag = ifelse(is.na(Lag), median(Lag[Time0=='studyentry'], na.rm=T),Lag))
study_info$KM.fig[study_info$Author=='Zitnan'] <- NA # Fig is not KM curve
save(study_info, file='data/rda/study_info.rda', compress=T)

summary_survival <- study_info %>%
  select(armID, pubID, number, Lag, starts_with('surv')) %>%
  gather(year, surv_perc, starts_with('surv')) %>%
  mutate(year = as.numeric(str_replace(year, 'surv([0-9]+)yr', '\\1'))) %>%
  dplyr::filter(!is.na(surv_perc)) %>%
  arrange(armID)

pubs_with_KM <- study_info %>%
  dplyr::filter(KM.fig!='0', !is.na(KM.fig)) %>%
  select(pubID) %>%
  distinct()

summary_survival <- summary_survival %>%
  dplyr::filter(!(pubID %in% pubs_with_KM$pubID)) %>% # only keep studies without KM
  left_join(
    study_info %>% select(armID, deaths, f.up.months)
  ) %>% 
  mutate(n.deaths = round(deaths * number / 100))


save(summary_survival, file='data/rda/summary_survival.rda', compress=T)


load('data/rda/summary_survival.rda')
load('data/rda/study_info.rda')

with_survival <- union(summary_survival$pubID, pubs_with_KM$pubID)
without_survival <- study_info %>%
  dplyr::filter(pubID %in% setdiff(pubID, with_survival)) %>%
  select(armID, pubID, number, deaths, f.up.months:max.f.up)

save(without_survival, file='data/rda/without_survival.rda', compress=T)


