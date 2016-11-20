# This file will contain updates to various data sets that are resulting
# from data cleaning exercises

source('study_info.R')
load('data/rda/study_info.rda')

study_info <- arrange(study_info, pubID) %>% 
  mutate(KM.fig = ifelse(is.na(KM.fig),'0',KM.fig))

## Estes' figures provide only summary survival data and not KM curves
study_info$KM.fig[study_info$Author=='Estes'] <- '0'

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

## Compute study year as middle of enrollment

bl <- study_info %>% 
  mutate(yr_of_study = round((start.enrollment + pmin(end.enrollment, end.fup, na.rm=T))/2),
         yr_of_study = ifelse(!is.na(yr_of_study), yr_of_study,
                              round(start.enrollment+(pubdate-1-start.enrollment)/2)),
         yr_of_study = ifelse(!is.na(yr_of_study), yr_of_study,
                              round(pmin(end.fup,pubdate-1, na.rm=T)-
                                      pmax(f.up.months,as.numeric(max.f.up), na.rm=T)/12/2))
  )

fillin <- function(x){
  if(!all(is.na(x)) & length(unique(x[!is.na(x)]))==1){
    x = rep(unique(x[!is.na(x)]),length(x))
  }
  return(x)
}
study_info <- study_info %>% left_join(bl %>% select(armID,yr_of_study)) %>% 
  nest(-pubID) %>% 
  mutate(data = map(data, ~mutate(., yr_of_study=fillin(yr_of_study)))) %>% 
  unnest()

## Identify developed status
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

study_info <- study_info %>% mutate(Status = bl$Status)

save(study_info, file='data/rda/study_info.rda', compress=T)
