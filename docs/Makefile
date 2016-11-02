RMD = $(wildcard *.Rmd)
DOCX = $(RMD:.Rmd=.docx)

docs: $(DOCX)

%.docx: %.Rmd
	Rscript -e "library(rmarkdown); render('$<', word_document())"
