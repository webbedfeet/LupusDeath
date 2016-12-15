## Processing figures with summary survival

source('preamble.R')
source('lib/reload.R')
reload()

csvdir = AD_local_mac['data']

csvfiles <- dir(csvdir, pattern='csv')
csvfiles <- csvfiles[str_detect(csvfiles, 'summ')]
csvfiles <- csvfiles[!str_detect(csvfiles, 'child')] # remove child arms

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
  
id <- metadata %>% filter(needs.pooling=='No') %>% select(ids)
metadata <- metadata
n1 = names(csvdata)[1]

param <- weib_param(csvdata[[n1]], metadata$Lag[metadata$filename==n1])
 