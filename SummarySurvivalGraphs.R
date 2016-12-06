## Processing figures with summary survival

source('preamble.R')
source('lib/reload.R')
reload()
load

csvdir = AD_local_mac['data']

csvfiles <- dir(csvdir, pattern='csv')
csvfiles <- csvfiles[str_detect(csvfiles, 'summ')]

csvdata <- lapply(csvfiles, function(f){
  d <- read.csv(file.path(csvdir,f), header=F)
  names(d) <- c('Year','Prob')
  return(d)
})
names(csvdata) <- csvfiles

csvdata <- lapply(csvdata, function(d){
  d$Year <-  round(d$Year)
  d <- arrange(d, Year)
  return(d)
})

metadata <- fig_metadata %>% 
  left_join(study_info %>% select(pubID,Lag), by=c('ids'='pubID'))

n1 = names(csvdata)[1]

param <- weib_param(csvdata[[n1]], metadata$Lag[metadata$filename==n1])
 