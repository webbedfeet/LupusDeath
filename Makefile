
DATADIR=/Users/abhijit/Dropbox/Work/Ward/Studies/LupusMetaDeath/Abhijit\ SLE\ Mortality\ Library


## study_info		: Create data from study characteristics
study_info: $(DATADIR)/*.csv study_info.R
	R CMD BATCH study_info.R

update_study_info: study_info.R
	R CMD BATCH DataMunging.R

## fig_metadata 		: Generate metadata for digitized curves
fig_metadata: csvfiles.R
	R CMD BATCH $<

## KM_digitized  		: Clean and store digitized curves
KM_digitized: Cleaning_KM.R data/rda/fig_metadata.rda
	R CMD BATCH $<

## KM2IPD  		: Generate IPD from clean KM curves
KM2IPD: KM2IPD.R data/rda/KM_digitized.rda
	R CMD BATCH KM2IPD.R

## membership : Generate which study belongs in which moving avg window
membership: DataMunging.R
	R CMD BATCH DataMunging.R

data: study_info update_study_info fig_metadata KM_digitized KM2IPD

.PHONY : clean
clean:
	-rm  -f *.Rout *~

help: Makefile
		@sed -n 's/^##//p' $<
