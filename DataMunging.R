# This file will contain updates to various data sets that are resulting
# from data cleaning exercises

load('data/rda/study_info.rda')

## Estes' figures provide only summary survival data and not KM curves
study_info$KM.fig[study_info$Author=='Estes'] <- '0'
