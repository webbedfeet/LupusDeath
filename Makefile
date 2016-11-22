
data: study_info fig_metadata KM_digitized KM2IPD

study_info: study_info.R DataMunging.R
	R CMD BATCH study_info.R
	R CMD BATCH DataMunging.R

fig_metadata: csvfiles.R
	R CMD BATCH $<

KM_digitized: Cleaning_KM.R
	R CMD BATCH $<

KM2IPD: KM2IPD.R
	R CMD BATCH KM2IPD.R

clean:
		rm *.Rout
