all: clean readme data doc test install

#CLEANING
#remove any intermediate files
clean:
	rm -f README.md data/* man/*

#DATA
#generate new data without downloading new data
data:
	Rscript 'data-raw/weasels.R'	

#TESTS
test: tests/testthat.R tests/testthat
	R -e 'devtools::test()'

#DOCUMENTATION
#document the package
doc:
	R -e 'devtools::document()'


#Project README
readme: README.Rmd
	R -e "rmarkdown::render('$(<F)')"

#R PACKAGE
#build and install the package
install: 
	R -e 'devtools::install()'
#	R CMD build .
#	R CMD check .
#	R CMD INSTALL .

