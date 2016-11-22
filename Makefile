
data: data/rda/study_info.rda data/rda/fig_metadata.rda
data/rda/study_info.rda: study_info.R DataMunging.R
		R CMD BATCH study_info.R
		R CMD BATCH DataMunging.R

data/rda/fig_metadata.rda: csvfiles.R
		R CMD BATCH $<
		
data/rda/KM_digitized.rda: Cleaning_KM.R
		R CMD BATCH $<
		
clean:
		rm *.Rout 
