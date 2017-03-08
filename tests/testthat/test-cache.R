# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.

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

