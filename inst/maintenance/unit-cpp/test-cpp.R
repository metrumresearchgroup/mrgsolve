
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-cpp")

code <- '
[ plugin ] nm-vars, mrgx, Rcpp

[ cmt ] @number 1

[ des ]  
DADT(1) = -0.1 * A(1);
'

test_that("build a model with mrgx and nm-vars", {
  expect_is(
    mcode("test-cpp-mrgx-nm-vars", code, quiet = TRUE), 
    "mrgmod"
  )
})

