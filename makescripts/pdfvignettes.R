#!/usr/bin/Rscript

library(knitr)
library(rmarkdown)
library(methods)

fmt <- "pdf_document"
loc <- "rdev/vignettes"
files <- c("first_model.Rmd", "data.Rmd", "re.Rmd", "example.Rmd")
files <- file.path(loc,files)

for(file in files) rmarkdown::render(file, output_format="html_document", output_dir="../../rdev/inst/doc")






