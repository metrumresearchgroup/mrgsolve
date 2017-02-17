library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

e <- ev(amt=100)
b <- ev(amt=200)

test_that("Input error", {
  expect_error(ev_days(e,"abc"))
  expect_error(ev_days(e))
})

test_that("Schedule with days argument", {
  x <- as.data.frame(ev_days(e,"m,w,f"))
  expect_identical(x$time,c(0,48,96))
  x <- as.data.frame(ev_days(e,"m,t,w"))
  expect_identical(x$time,c(0,24,48))
})

test_that("Schedule with missing arguments", {
  x <- as.data.frame(ev_days(t=e, th=e, s=e))
  expect_identical(x$time, c(24,72,144))
  x <- as.data.frame(ev_days(f=e,sa=b))
  expect_identical(x$time, c(96,120))
  expect_identical(x$amt, c(100,200))
})


