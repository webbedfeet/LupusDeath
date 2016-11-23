source('lib/reload.R')
source('preamble.R')
reload()
load('data/rda/KM_digitized.rda') #loads KM_full & KM_digitized_clean

KM2stratIPD <- list()
for(u in names(KM_digitized_clean)){
  KM2stratIPD[[u]] <- get.IPD(KM_digitized_clean[[u]], 
                         KM_full$N[KM_full$filename==u])
}

## Pooling studies as needed
KM2IPD <- list()
for(u in KM_full$ids){
  lst <- KM2stratIPD[KM_full$filename[KM_full$ids==u]]
  KM2IPD[[u]] <- pool.ipd(lst)
  KM2IPD[[u]] <- lapply(KM2IPD[[u]], unname)
}
save(KM2IPD, file='data/rda/KM2IPD.rda', compress=T)
