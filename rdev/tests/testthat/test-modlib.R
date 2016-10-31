

library(mrgsolve)
library(testthat)
library(dplyr)


Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-modlib models")

test_that("Lagged bolus", {
  
  test_lib <- function(x) {
    mod <- mread(x,modlib())
    out <- mrgsim(mod)
    return(list(mod,out))
  }
  
  x <- test_lib("pk1cmt")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("pk2cmt")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("pk3cmt")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("irm1")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("irm2")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("irm3")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("irm4")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("effect")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  

  x <- test_lib("emax")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("viral1")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  

  x <- test_lib("viral2")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
  x <- test_lib("tmdd")
  expect_is(x[[1]],"mrgmod")
  expect_is(x[[2]],"mrgsims")
  
})

















