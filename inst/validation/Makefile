all:
	Rscript -e 'testthat::test_dir(".")'
all-tests:
	Rscript run_test.R
doc:
	Rscript -e 'rmarkdown::render("validation-testing.Rmd")'
