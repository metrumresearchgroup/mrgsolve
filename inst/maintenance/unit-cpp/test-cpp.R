
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

code <- '
$INCLUDE include-1.h
$CMT @number 4
$MAIN capture cmt = include1::getcmt(self);
'
incl <- '
namespace include1 {int getcmt(databox& self) {return self.cmt; }}
'

cfile <- file.path(tempdir(), "get-cmt.cpp")
hfile <- file.path(tempdir(), "include-1.h")
writeLines(code, con = cfile)
writeLines(incl, con = hfile)

test_that("included file understands databox", {
  mod <- mread(cfile)  
  data <- expand.ev(amt = 100, cmt = c(1,2,3,4))
  out <- mrgsim(mod, data, end = -1)
  expect_equal(out$cmt, c(1,2,3,4))
})
