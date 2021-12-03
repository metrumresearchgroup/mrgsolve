
library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

test_that("THETA(n) is allowed", {
  code <- '
  [ param ] THETA1 = 1.2;
  [ main ] 
  capture CL = THETA(1);
  '
  mod <- mcode("thetan", code)
  out <- mrgsim(mod, end = 1)
  expect_equal(out$CL[1], 1.2)
  expect_error(
    mcode("thetan2", "[param] THETA = 2"), 
    regexp="Reserved words in model names: THETA"
  )
})
