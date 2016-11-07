library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-cache")

mrgsolve:::update_wait_time(0)

test_that("model caches via mread_cache", {
  
  mod <- mread_cache("pk1cmt", modlib())
  
  cache_file <- file.path(mrgsolve:::soloc(mod), "mrgmod_cache.RDS")
  
  file.exists(cache_file)
  expect_true(file.exists(cache_file))
  
  mo <- readRDS(cache_file)
  
  #expect_identical(mo,mod)
  
  mo@shlib$foo <- "test"

  saveRDS(mo,cache_file)  

  mod2 <- mread_cache("pk1cmt", modlib())
  
  expect_equal(mod2@shlib$foo,"test")
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
  #expect_identical(mod,mod2)
  expect_false(identical(mod,mod3))
})


mrgsolve:::update_wait_time(3)

