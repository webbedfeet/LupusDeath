# Creating stairplot for study

source('lib/reload.R')
reload()
load('data/rda/study_info.rda')

info <- study_info %>% filter(armID==pubID)
info <- rbind(info,
              study_info %>% filter(!(pubID %in% info$pubID)))

plt <- info %>% 
  mutate(yr_of_study_end = end_of_study) %>% 
  stairdata() %>% 
  stairplot()

plt2 <- info %>% 
  mutate(yr_of_study_end = end_of_study_10) %>% 
  stairdata() %>% 
  stairplot()

pdf(file='graphs/stairplot.pdf')
print(plt)
dev.off()

pdf(file = 'graphs/stairplot_10yr.pdf')
print(plt2)
dev.off()
