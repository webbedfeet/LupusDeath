# Extracting results from moving average analysis
source('lib/reload.R')
reload()
load('data/rda/window_membership.rda')
load('data/rda/study_info.rda')

plt <- 'adult' %>% mcmcout() %>% 
  collapseResults() %>% 
  pltResults()

pdf(file='graphs/adultMA.pdf')
print(plt)
dev.off()


