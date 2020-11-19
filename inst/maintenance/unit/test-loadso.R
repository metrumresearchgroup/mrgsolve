# Copyright (C) 2013 - 2020  Metrum Research Group
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

context("test-loadso")

test_that("try loading model object if it is not loaded", {
  mod <- mcode("loadso-bad","$PARAM CL = 1", compile = FALSE)
  expect_error(
    suppressMessages(mrgsim(mod)), 
    "There was a problem accessing the model shared object."
  )
})

test_that("loadso fails if shared object file doesn't exist", {
  mod <- mcode("loadso-bad","$PARAM CL = 1", compile = FALSE)
  expect_error(
    loadso(mod), 
    "the model dll file doesn't exist"
  )
})
