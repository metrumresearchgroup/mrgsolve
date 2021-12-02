# Copyright (C) 2013 - 2019  Metrum Research Group
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

nmv <- function(x) {
  paste0("[ plugin ] nm-vars \n ", x) 
}

test_that("FRDA in param is error", {
  expect_error(
    mcode("frda1", nmv("[ param ] CL = 1, F1 = 5")), 
    regexp = "reserved: F1"
  )
  expect_error(
    mcode("frda2", nmv("[ param ] CL = 1, R2 = 2")), 
    regexp = "reserved: R2"
  )
  expect_error(
    mcode("frda3", nmv("[ param ] CL = 1, D5 = 2")), 
    regexp = "reserved: D5"
  )
  expect_error(
    mcode("frda4", nmv("[ param ] CL = 1, ALAG1 = 2")), 
    regexp = "reserved: ALAG1"
  )
})

test_that("FRDA in cmt is error", {
  expect_error(
    mcode("frda5", nmv("[ cmt ] GUT CENT F1")), 
    msg = "reserved: F1"
  )
  expect_error(
    mcode("frda5", nmv("[ cmt ] GUT R1 CENT")), 
    regexp = "reserved: R1"
  )
  expect_error(
    mcode("frda7", nmv("[ cmt ] D5 GUT CENT")), 
    regexp = "reserved: D5"
  )
  expect_error(
    mcode("frda7", nmv("[ cmt  ] B C  ALAG1 D")), 
    regexp = "reserved: ALAG1"
  )
})

test_that("Compartment number bounds checking", {
  expect_error(
    mcode("frda8", nmv("[ cmt ] B C D \n [ main ] F5 = 2;")), 
    regexp = "out of range: F5"
  )
  expect_error(
    mcode("frda8", nmv("[ cmt ] B C D \n [ main ] ALAG50 = 2; \n F2 = 1;")), 
    regexp = "out of range: ALAG50"
  )
  expect_error(
    mcode("frda9", nmv("[ cmt ] B C D \n [ table ] double Foo = A(10);")), 
    regexp = "out of range: A\\(10\\)"
  )
  expect_error(
    mcode("frda10", nmv("[ cmt ] B  \n [ ode ] @!audit \n DADT(5) = 0;")), 
    regexp = "out of range: DADT\\(5\\)"
  )
})
