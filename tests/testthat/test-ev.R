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

test_that("collection of events", {
  e1 <- ev(amt=200)
  e2 <- ev(amt=100,time=1)
  e <- c(e1,e2)
  expect_is(e,"ev")
  e <- as.data.frame(e)
  expect_equal(e$time, c(0,1))
  expect_equal(e$amt, c(200,100))
})


test_that("realized events", {
  e <- as.data.frame(ev(amt=100, ii=24, addl=4))
  expect_equal(nrow(e),1)
  e <- as.data.frame(ev(amt=100, ii=24, addl=4,realize=TRUE))
  expect_equal(nrow(e),5)
  expect_true(all(e$amt==100))
  expect_true(all(e$ii==0))
  expect_true(all(e$addl==0))
  
  e1 <- ev(amt=100, ii=24, addl=1)
  e2 <- ev(amt=200, ii=24, addl=3, time=1)
  e <- as.data.frame(ev(e1+e2,realize_addl=TRUE))
  expect_equal(e$time, c(0,1,24,25,49,73))
})

test_that("realized event error", {
  expect_error(ev(amt=100, realize_addl=TRUE))
  expect_error(ev(amt=100, ii=24, realize_addl=TRUE))
  expect_error(ev(amt=100, addl=24, realize_addl=TRUE))
})



