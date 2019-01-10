# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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

context("test-ev_assign")

e <- ev(amt=100)
b <- ev(amt=200)

data0 <- tibble(ID = c(1,2,3), COV = c(1,2,3))

data <-tibble(ID = c(1,2,3,4), COV = c(2,1,1,2))


test_that("Input error", {
  expect_error(ev_assign(list(e,b), data0, "COV"))
  expect_is(ev_assign(list(e,b), data, "COV"), "data.frame")
})

test_that("Assignment on sorted values", {
  df <- ev_assign(list(b,e), data, "COV")
  expect_equal(df$amt, c(100, 200, 200, 100))
})

