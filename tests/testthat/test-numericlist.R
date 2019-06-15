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

context("test-numericlist")

test_that("numericlist", {
  valid <- mrgsolve:::valid.numericlist
  x <- param(A = 1, B = 2)
  expect_true(valid(x))
  capture.output(expect_error(param(A.2 = 1)))
  expect_error(param(A = c(1,2,3)))
  x <- mrgsolve:::create_numeric_list(list(A = 1, B = 2), "parameter_list")
  expect_is(x,"parameter_list")
})
