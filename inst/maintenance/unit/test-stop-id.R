library(testthat)
library(mrgsolve)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-stop-id")

code <- '
$PROB
We can make the simulation stop for the current ID and fill either 
NA or the last value; OR we can just make the simulation stop with error.
$PARAM STOP_CF = 0, STOP_NA = 0, STOP_CRUMP = 0
$CMT A
$ODE dxdt_A = 1;
$MAIN
if(NEWIND <=1) capture stopped = 0;
$ERROR
if(STOP_CF==1) {
  if(TIME==4) {
    self.stop_id_cf();
    stopped = 1;
  }
}
if(STOP_NA==1) {
  if(TIME==3) {
    self.stop_id(); 
    stopped = 1;
  }
}
if(STOP_CRUMP==1) {
  self.stop();
}
'

mod <- mcode("test-stop-id", code, end = 6)

test_that("Stop the current ID and carry forward", {
  mod <- param(mod, STOP_CF = 1, STOP_NA = 0, STOP_CRUMP = 0)
  out <- mrgsim_df(mod)  
  pre <- out[out$time <= 3,,drop=FALSE]
  post <- out[out$time > 3,,drop=FALSE]
  expect_true(all(pre$stopped==0))
  expect_true(all(post$stopped==1))
  expect_equal(pre$A, c(0,1,2,3))
  expect_true(all(post$A==4))
})

test_that("Stop the current ID and fill NA", {
  mod <- param(mod, STOP_NA = 1, STOP_CF = 0, STOP_CRUMP = 0)
  out <- mrgsim_df(mod)  
  pre <- out[!is.na(out$time),,drop=FALSE]
  post <- out[is.na(out$time),,drop=FALSE]
  expect_equal(nrow(pre), 4)
  expect_equal(pre$A, c(0,1,2,3))
  expect_equal(pre$stopped, c(0,0,0,1))
  expect_equal(nrow(post), 3)
  expect_true(all(is.na(post)))
})

test_that("Stop the entire simulation", {
  mod <- param(mod, STOP_NA = 0, STOP_CF = 0, STOP_CRUMP = 1)
  expect_error(mrgsim_df(mod), regexp="the problem was stopped at user request.")
})
