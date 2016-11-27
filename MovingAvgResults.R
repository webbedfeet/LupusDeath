# Extracting results from moving average analysis
source('lib/reload.R')
reload()

plt <- 'adult' %>% mcmcout() %>% 
  collapseResults() %>% 
  pltResults()

pdf(file='graphs/adultMA.pdf')
print(plt)
dev.off()
