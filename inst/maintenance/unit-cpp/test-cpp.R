
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
$PK
if(NEWIND <= 1) capture FLAG = 0;
$CMT A
$TABLE 
if(TIME==2) self.mevent(2.001,33);
if(EVID==33) ++FLAG;
'
test_that("ev history is reset", {
  mod <- mcode("test-cpp-re-event", code, end = -1, add = c(1,2,3))
  out <- mrgsim_df(mod, ev(amt = 0, ID = 1:2, evid = 2)) 
  expect_equal(sum(out$FLAG), 2)  
  out3 <- subset(out, time==3)
  expect_length(out3$FLAG, 2)
  expect_true(all(out3$FLAG==1))
})
