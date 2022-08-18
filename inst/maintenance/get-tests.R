library(stringfish)
library(dplyr)
library(here)


files1 <- list.files(here("tests/testthat"), full.names = TRUE, pattern = "\\.R")
files2 <- list.files(here("inst/maintenance/unit"), full.names = TRUE, pattern = "\\.R")
files <- c(files1, files2)
x <- lapply(files, readLines)
x <- lapply(x, function(text) {
  grep("test_that", text, value = TRUE)  
})
x <- lapply(x, function(text) {
  grep("SLV|TEST", text, value = TRUE)  
})
x <- x[lengths(x) > 0] %>% unlist()
m <- regexec("\\[[A-Z]{3,4}\\-[A-Z]{3,4}\\-([0-9]{4,5})\\]", x)
matches <- unlist(regmatches(x, m))
matches



