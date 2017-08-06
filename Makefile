all: clean_all data_all doc test paper readme

#CLEANING
#remove any intermediate files
clean:
	rm -f README.md

#remove all intermediate files, including data
clean_data:
	rm -f data/raw/* data/proc/* data/final/*

clean_all: clean clean_data


#DATA
#generate new data without downloading new data
data:

#TESTS
test:

#DOCUMENTATION
#document the package
doc:
	R -e 'devtools::document()'


#Project README
readme: README.Rmd
	R -e "rmarkdown::render('$(<F)')"

#R PACKAGE
#build and install the package
install: doc
	R CMD check .
	R CMD INSTALL .

