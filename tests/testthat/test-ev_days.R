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


