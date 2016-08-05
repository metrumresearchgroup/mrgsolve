
library(dplyr)
library(mrgsolve)
library(testthat)


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

















