# Reading and cleaning digitized KM curves

source('lib/reload.R')
source('preamble.R')
library(stringr)
reload()
datadir <- AD_local_mac['data']

load('data/rda/fig_metadata.rda')
KM_full <- fig_metadata %>% filter(male.only=='No', is.KM=='Yes')
Ns <- study_info$number; names(Ns) <- study_info$pubID
KM_full$N <- Ns[KM_full$ids]
KM_full <- KM_full %>% mutate(N = ifelse(needs.pooling=='Yes',NA, N))
KM_full$N[is.na(KM_full$N)] <- c(
  83,271,170,175,153, # Artim-Essen
  193,40, # Bujan
  62,43, # Cardoso
  511, 156, # Drenkard
  221, 117, 103, 176, # Fernandez
  115, 101, # Flower
  54, 101, 151, # Funauchi
  147, 35, # Jallouli
  64, 638, 52, # Johnson
  276, 67, # Kao
  795, 133, # Merola
  32, 645, # Mok
  150, 82, # Ruiz-Irastorza
  43, 116, # Schmid
  1141, 339, # Shinjo
  76, 4, # Stoll
  NA,NA # Voss
)
