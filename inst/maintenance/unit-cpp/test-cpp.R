
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-cpp")

code <- '
[ global ]
std::vector<double> a;

[ preamble ] a.clear();

[ plugin ] nm-vars, Rcpp, mrgx

[ cmt ] @number 1

[ des ]  
DADT(1) = -0.11 * A(1)  ;  

[ error ] 
a.push_back(1); 
a.push_back(2)  ;
'

system.time(
  mod <- mcode_cache("test-cpp-mrgx-nm-vars", code)
)


test_that("build a model with mrgx and nm-vars", {
  expect_is(
    mod, 
    "mrgmod"
  )
})

out <- mrgsim(mod, end = 1)
mod@envir$data

