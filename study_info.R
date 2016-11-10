# Extracting information from Excel file

source('lib/reload.R')
reload()
source('preamble.R')

datadir <- AD_dirs_mac['popdata']
study_info <- read_excel(file.path(datadir,'double-checked_adult_Abhijit.xlsx'))[1:187, 1:43]

## Fix names of variables
names(study_info) <- str_trim(names(study_info))
study_info <- study_info %>%
  rename(surv15yr = `surv15 y`,
         Time0 = `Time 0`)
names(study_info) <- make.names(names(study_info)) %>% str_replace(., '\\.+$','') %>%
  str_replace_all(., '\\.+','.')

## Cleaning data
study_info <- study_info %>%
  mutate_if(is.character, str_trim) %>% # get rid of trailing spaces
  mutate(armID = paste(Author, pubdate, Arm, sep='_'), # create unique ID per arm
         armID = str_replace(armID,'_NA',''),
         armID = str_replace(armID,' ','_'),
         pubID = paste(Author, pubdate, sep='_'), # create publication ID
         dis.dur.yrs = as.numeric(str_replace(dis.dur.yrs, '<', '')),
         Time0 = ifelse(inception==1,'diagnosis', Time0),
         Time0 = tolower(Time0),  # Account for inception cohorts
         Lag = ifelse(Time0=='diagnosis', 0, dis.dur.yrs), # create Lag variable
         Lag = ifelse(is.na(Lag), median(Lag[Time0=='studyentry'], na.rm=T),Lag))
save(study_info, file='data/rda/study_info.rda', compress=T)

summary_survival <- study_info %>%
  select(armID, number, Lag, starts_with('surv')) %>%
  gather(year, surv_perc, starts_with('surv')) %>%
  mutate(year = as.numeric(str_replace(year, 'surv([0-9]+)yr', '\\1'))) %>%
  dplyr::filter(!is.na(surv_perc))

save(summary_survival, file='data/rda/summary_survival.rda', compress=T)
