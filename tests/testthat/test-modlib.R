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

















