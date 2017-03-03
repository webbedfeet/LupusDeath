# Creating stairplot for study

source('lib/reload.R')
reload()
load('data/rda/final_study_info.rda')

filter_overall <- function(d){
  if(any(str_detect(d$Arm, '[abc]$'))){
    d <- filter(d, Arm!='')
  }
  return(d)
}

# Remove male-only studies and Grigor

info <- study_info %>% filter(armID==pubID) %>% filter(male.only=='N')
# info <- rbind(info,
#               study_info %>% filter(!(pubID %in% info$pubID))) %>%
  # filter(male.only=='N') %>%
  # filter(pubID != 'Grigor_1978') ## Mike has no info on this 3/3/17

## Just present 10 year truncation since the graphs will contain all the information
plt <- info %>%
  mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')')) %>%
  mutate(yr_of_study_end = end_of_study_10) %>%
  stairdata() %>%
  stairplot() + ggtitle('Overall')

plt_developed <- info %>%
  mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')'),
         yr_of_study_end = end_of_study_10) %>%
  filter(Developed=='Developed') %>%
  stairdata() %>%
  stairplot()+ggtitle('Developed countries')

plt_developing <- info %>%
  mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')'),
         yr_of_study_end = end_of_study_10) %>%
  filter(Developed=='Developing') %>%
  stairdata() %>%
  stairplot() + ggtitle('Developing countries')
pdf(file='graphs/stairplot_adult.pdf')
print(plt)
print(plt_developed)
print(plt_developing)
dev.off()

# plt2 <- info %>%
#   mutate(pubID = pubID %>% str_replace('_',' (') %>% paste0(')') ) %>%
#   mutate(yr_of_study_end = end_of_study_10) %>%
#   stairdata() %>%
#   stairplot()
#
#
# pdf(file = 'graphs/stairplot_adult_10yr.pdf')
# print(plt2)
# dev.off()
