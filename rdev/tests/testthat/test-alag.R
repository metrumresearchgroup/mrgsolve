

library(mrgsolve)
library(testthat)
library(dplyr)


Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)


rm(list=ls())

code <- '
$PARAM LAG = 2.8
$CMT CENT
$MAIN
ALAG_CENT = LAG;
'
mod <- mcode("alag1", code)


context("Lagged doses")

test_that("Lagged bolus", {
    out <- mod %>% ev(amt=100) %>% mrgsim(delta=0.01,add=c(2.7999999,2.8000001), end=10)
    first <- with(as.tbl(out),time[which(CENT > 0)[1]])
    expect_equal(first,2.8)
})

## Issue #109
test_that("Very small lag time doesn't crash", {

  out <- mod %>% ev(amt=100) %>% param(LAG = 1E-30) %>% mrgsim
  expect_is(out, "mrgsims")
  expect_equal(out$time[which.max(out$CENT)],0)
  
  out <- mod %>% ev(amt=100) %>% param(LAG = 2) %>% mrgsim
  expect_is(out, "mrgsims")
  expect_equal(out$time[which.max(out$CENT)],2)
})














