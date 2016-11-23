
data: study_info fig_metadata KM_digitized KM2IPD

## study_info		: Create data from study characteristics
study_info: study_info.R DataMunging.R
	R CMD BATCH study_info.R
	R CMD BATCH DataMunging.R

## fig_metadata 		: Generate metadata for digitized curves
fig_metadata: csvfiles.R
	R CMD BATCH $<

## KM_digitized  		: Clean and store digitized curves
KM_digitized: Cleaning_KM.R
	R CMD BATCH $<

## KM2IPD  		: Generate IPD from clean KM curves
KM2IPD: KM2IPD.R
	R CMD BATCH KM2IPD.R

## membership : Generate which study belongs in which moving avg window
membership: DataMunging.R
	R CMD BATCH DataMunging.R

.PHONY : clean
clean:
	-rm  -f *.Rout *~

help: Makefile
		@sed -n 's/^##//p' $<