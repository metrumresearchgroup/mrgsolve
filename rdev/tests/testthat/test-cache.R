library(mrgsolve)
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
context("test-cache")


test_that("model caches via mread_cache", {
  mod <- mread_cache("pk1cmt", modlib())
  
  mod2 <- mread_cache("pk1cmt", modlib())
  
  expect_identical(mod,mod2)
})

test_that("model caches via mcode_cache", {
  code <- '
  $PARAM A = 1 
  $CMT B
  $MAIN double z = 4;
  '
  code2 <- paste0(code, "double x = 5;")
  mod <- mcode_cache("test_mcode_cache",code)
  mod2 <- mcode_cache("test_mcode_cache",code)
  mod3 <- mcode_cache("test_mcode_cache", code2)
  expect_identical(mod,mod2)
  expect_false(identical(mod,mod3))
})




