## Processing figures with summary survival

source('preamble.R')
source('lib/reload.R')
reload()


# Import and process graphs of summary survival ---------------------------

csvdir = AD_local_mac['data']

## Create metadata for graph summaries
## 
load('data/rda/fig_metadata.rda')
load('data/rda/final_study_info.rda')
fig_metadata <- fig_metadata %>%
  mutate(male.only = ifelse(ids %in% study_info$pubID[study_info$male.only=='Y'],'Yes','No'))

metadata <- fig_metadata %>% filter(str_detect(filename, 'summ')) %>% 
  filter(male.only=='No')
Ns <- study_info$number[study_info$pubID %in% metadata$ids]
names(Ns) <- study_info$pubID[study_info$pubID %in% metadata$ids]
metadata$N <- Ns[metadata$ids]
metadata$N[metadata$needs.pooling=='Yes'] <- NA
metadata$N[is.na(metadata$N)] <- c(
  25,113,  # Ballou
  501,21,  # Hashimoto
  257, 42, # Kellum
  218,77,  # Reveille
  56, 254, # Seleznick
  93, 41, 158, # Siegel
  546, 63, # Wallace
  499, 40, # Wang
  330, 117, 25 # Wu
)

save(metadata, file = 'data/rda/SummSurvMetadata.rda') 

## Read csv files and process
## 
csvfiles <- dir(csvdir, pattern='csv')
csvfiles <- csvfiles[csvfiles %in% metadata$filename]

csvdata <- lapply(csvfiles, function(f){
  d <- read.csv(file.path(csvdir,f), header=F)
  names(d) <- c('Time','Prob')
  return(d)
})
names(csvdata) <- csvfiles

## Cleaning the digitized data
csvdata <- lapply(csvdata, Summ_clean)

id <- 'Hersh-2010-fig1-adult-summ.csv'
csvdata[[id]] <- mutate(csvdata[[id]], Time=Time/12) # This data is in months


# Import summary data not in graphs ---------------------------------------
load('data/rda/graph_types.rda')
ids <- c(csv_km, csv_summaries) %>% str_extract('^[\\w -]+[0-9]{4}[abc]?') %>% 
  str_replace('-([0-9]{4})','_\\1') %>% str_to_title()

'%!in%' <- Negate('%in%')
dat <- study_info %>% filter(pubID %!in% ids)
# Weibull estimation ------------------------------------------------------


param <- weib_param(csvdata[[n1]], metadata$Lag[metadata$filename==n1])
 