# This file will contain updates to various data sets that are resulting
# from data cleaning exercises

source('study_info.R')
load('data/rda/study_info.rda')

study_info <- arrange(study_info, pubID) %>% 
  mutate(KM.fig = ifelse(is.na(KM.fig),'0',KM.fig))

# Data cleaning -----------------------------------------------------------

study_info$max.f.up <- as.numeric(study_info$max.f.up)

## Update Estes' information to start at 1961 and end at 1969
study_info[study_info$pubID=='Estes_1971',]$start.enrollment=1961
study_info[study_info$pubID=='Estes_1971',]$end.enrollment=1969

## Update Joo information
study_info[study_info$pubID=='Joo_2015',]$end.fup = 2007
study_info[study_info$pubID=='Joo_2015',]$end.enrollment = NA

## Make Siegel_1969_female the overall study record
study_info$armID[study_info$armID=='Siegel_1969_female'] <- 'Siegel_1969'

## Fix the information in the Design field
study_info <- study_info %>% 
  mutate(Design = str_to_title(Design),
         Design = ifelse(str_count(Design, pattern='[[:alpha:]]+')==2, Design,
                         paste(Design, 'Observational', sep=' ')))

# New variables -----------------------------------------------------------

## Identify male only studies, which will be kept separate
male_only <- study_info %>% filter(female==0) %>% 
  select(pubID) %>% left_join(study_info %>% count(pubID)) %>% 
  filter(n==1)
study_info <- study_info %>% 
  mutate(male.only = factor(ifelse(pubID %in% male_only$pubID, 'Y','N')))

## Identify arms that represent full studies

bl <- study_info %>% filter(armID==pubID) %>% select(armID)
study_info <- study_info %>% 
  mutate(fullstudy.arm = factor(ifelse(armID %in% bl$armID, 'Y','N')))


# Compute different temporal quantities for the studies -------------------

bl <- study_info %>%
  mutate(start_of_study = beginYear(.),
         yr_of_study = startYear(.),
         end_of_study = endYear(.),
         end_of_study_10 = endYear(., maxduration=10))

study_info <- study_info %>% 
  left_join(bl %>% select(armID, start_of_study, yr_of_study, end_of_study, end_of_study_10)) %>% 
  nest(-pubID) %>% 
  mutate(data = lapply(data, function(d){
    mutate_each(d, funs(fillin(.)), start_of_study, yr_of_study, end_of_study, 
                end_of_study_10)})) %>% 
  unnest()

# Identify developed status -----------------------------------------------
## Country is "Developed" if it's World Bank GNI per capita classification is 
## High Income

study_info <- study_info %>% 
  mutate(Country = str_to_title(Country),
         Country = ifelse(Country %in% c('Usa','Uk'), str_to_upper(Country), Country),
         Country = str_replace(Country, '[\\.,]',''))
transform_country <- c(
  "USA" = "United States",
  "Latin American Countries" = "Latin America",
  "Korea" = "Korea, Rep.",
  'UK' = "United Kingdom",
  "Dubai" = "United Arab Emirates",
  "Russia" = "Russian Federation",
  "Hong Kong" = 'Hong Kong SAR, China',
  "Iran" = "Iran, Islamic Rep."
)
study_info <- study_info %>% 
  mutate(Country = ifelse(Country %in% names(transform_country),
         transform_country[Country],
         Country))

thresh <- read_excel('data/raw/OGHIST.xls', sheet='Thresholds', skip=5)
thresh <- data.frame(t(thresh[c(1,18),-1]), stringsAsFactors=F)
thresh <- thresh %>% 
  rename(Year=X1, Threshold = X2) %>% 
  mutate(Year = as.numeric(Year),
         Threshold = Threshold %>% str_trim() %>% str_replace('> ','') %>% 
           str_replace(',','') %>% as.numeric)
incomes <- read_excel('data/raw/NY.GNP.PCAP.CD_Indicator_MetaData_en_EXCEL.xls')
names(incomes) <- str_replace(names(incomes),' ','.')
incomes <- gather(incomes, Year, Income, -(1:2)) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  left_join(thresh) %>% 
  mutate(Status = ifelse(Income >= Threshold, "Developed", "Developing"))

bl <- study_info %>% select(pubID,Country, yr_of_study) %>% 
  left_join(incomes, by=c('Country'='Country.Name',
                          'yr_of_study'='Year')) %>% 
  mutate(Status = ifelse(is.na(Status) & yr_of_study < 1987 & Income >=6000,
                         'Developed','Developing'),
         Status = ifelse(Country=='United States','Developed',Status),
         Status = ifelse(Country=='Latin America','Developing',Status),
         Status = ifelse(Country=='Germany', 'Developed',Status),
         Status = ifelse(Country=='Europe','Developed',Status),
         Status = ifelse(Country=='International','Developing',Status),
         Status = ifelse(Country=='Poland', 'Developing',Status),
         Status = ifelse(Country=='Antilles', 'Developing', Status),
         Status = ifelse(Country=='Russian Federation', 'Developing', Status),
         Status = ifelse(Country=='Usa/Spain','Developed', Status),
         Status = ifelse(Country=='New Zealand', 'Developed', Status))

study_info <- study_info %>% mutate(Developed = bl$Status)

save(study_info, file='data/rda/study_info.rda', compress=T)


# Windowing for moving average --------------------------------------------

study_duration <- study_info %>% mutate(start_date=yr_of_study,
                            end_date = end_of_study) %>% 
  select(pubID, start_date, end_date) %>% 
  nest(-pubID) %>% 
  mutate(final = map(data, ~mutate(., 
                                   start_date = min(start_date, na.rm=T),
                                   end_date = max(end_date, na.rm=T)))) %>% 
  select(-data) %>% 
  unnest() %>% 
  distinct()

time_range <- c(min(study_duration$start_date, na.rm=T), max(study_duration$end_date))
window_length <- 5
x <- seq(time_range[1], time_range[2]-window_length+1)
Windows <- do.call(cbind, 
                   lapply(1:window_length-1, '+', x)) %>% 
  unname()

study_years <- plyr::dlply(study_duration, ~pubID, 
                           function(d) seq(d$start_date, d$end_date))

membership <- plyr::ldply(study_years, 
                     function(x) apply(Windows,1, 
                                       function(y) length(intersect(x,y))>0))
names(membership) <- str_replace(names(membership),'V','Window')

save(membership, file = 'data/rda/window_membership.rda', compress=T)