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


# Updates for paper -------------------------------------------------------
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

bl <- 'rda/adult' %>% mcmcout() %>% collapseResults() %>%
  mutate(Dev = ifelse(Dev=='Developed', 'High Income Countries','Low/Middle Income Countries'))
pltResults(bl) + scale_color_brewer(palette = 'Set1')
ggsave('graphs/Fig3.pdf')

pltResults(bl, bydev = F) + scale_color_brewer(palette='Set1')
ggsave('graphs/SupplFig2.pdf')

bl <- 'rda/adult_10' %>% mcmcout() %>% collapseResults() %>%
  mutate(Dev = ifelse(Dev=='Developed','High Income Countries','Low/Middle Income Countries'))
pltResults(bl) + scale_color_brewer(palette='Set1')
ggsave('graphs/SupplFig3.pdf')

bl <- 'rda/inception' %>% mcmcout() %>% collapseResults() %>%
  mutate(Dev = ifelse(Dev=='Developed','High Income Countries','Low/Middle Income Countries'))
pltResults(bl) + scale_color_brewer(palette='Set1')
ggsave('graphs/SupplFig4.pdf')



# Compute 2008-2016 pooled results ------
bl <- pooledCR(2008,2016)
output <- bl$output %>% mutate(out = paste0(Median,' (',`LCB (0.95)`,', ',`UCB (0.95)`,')')) %>%
  select(Developed, Year, out) %>%
  mutate(Year = paste0(Year, ' year')) %>%
  spread(Year, out)
devcount <- study_info %>% filter(armID==pubID, pubID %in% bl$studies) %>% count(Developed)
output <- cbind(output, n = devcount$n)[,c(1,5,4,2,3)]
# library(ReporteRs)
# output <- docx() %>%
#   addParagraph(value="Pooled estimate, 2008-2016 (mortality rate)") %>%
#   addFlexTable(FlexTable(bl)) %>%
#   writeDoc(file = 'docs/pooledEstimate.docx')
# Currently not working on work desktop, with Java issues

# inception-only
bl2 <- pooledCR(2008,2016, inception=T)
output <- bl2$output %>% mutate(out = paste0(Median,' (',`LCB (0.95)`,', ',`UCB (0.95)`,')')) %>%
  select(Developed, Year, out) %>%
  mutate(Year = paste0(Year, ' year')) %>%
  spread(Year, out)
devcount <- study_info %>% filter(armID==pubID, pubID %in% bl2$studies) %>% count(Developed)
output <- cbind(output, n = devcount$n)[,c(1,5,4,2,3)]
