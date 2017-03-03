# Extracting results from moving average analysis
source('lib/reload.R')
reload()
load('data/rda/window_membership.rda')
load('data/rda/study_info.rda')

titles <- c('adult' = 'All data',
            'adult_10' = 'Restricted to 10 years',
            'inception' = 'Inception cohorts')
pdf(file = 'graphs/adultMA.pdf')
for(x in paste('rda',c('adult','adult_10','inception'), sep='/')){
plt <- x %>% mcmcout() %>%
  collapseResults() %>%
  pltResults()
print(plt+ggtitle(titles[str_replace(x, 'rda/','')]))
}
for (x in paste('rda',c('adult','adult_10','inception'), sep='/')){
  plt <- x %>% mcmcout() %>%
    collapseResults() %>%
    pltResults(bydev=F)
  print(plt + ggtitle(titles[str_replace(x, 'rda/','')]))
}
dev.off()

# Compute 2008-2016 pooled results
bl <- pooledCR(2008,2016)
library(ReporteRs)
output <- docx() %>%
  addParagraph(value="Pooled estimate, 2008-2016 (mortality rate)") %>%
  addFlexTable(FlexTable(bl)) %>%
  writeDoc(file = 'docs/pooledEstimate.docx')


