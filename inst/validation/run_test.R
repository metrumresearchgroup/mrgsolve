library(testthat)
library(dplyr)
library(mrgsolve)


run_test <- function(dir,d) {
  
 cwd <- getwd()
 on.exit(setwd(cwd))
 setwd("../../")
 x <- test_dir(dir) %>% as_tibble
 x$result <- NULL
 x$location <- dir
 x$date <- d
 x
}

d  <- Sys.time()
x <- run_test("inst/validation",d)
y <- run_test("tests/testthat",d) 
z <- run_test("inst/maintenance/unit/",d)

df <- bind_rows(x,y,z)

readr::write_csv(path="all_tests.csv", df)
