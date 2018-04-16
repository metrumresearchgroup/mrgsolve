# Copyright (C) 2013 - 2018  Metrum Research Group, LLC
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

context("test-as_list_mrgmod")

options(mrgsolve_mread_quiet=TRUE)

test_that("check items in as.list output", {
  expect_exists <- function(object,name) {
    expect_true(name %in% names(object))
  }
  x <- as.list(mrgsolve:::house())  
  expect_exists(x, "start")
  expect_exists(x, "end")
  expect_exists(x, "delta")
  expect_exists(x, "add")
  expect_exists(x, "param")
  expect_exists(x, "init")
  expect_exists(x, "omega")
  expect_exists(x, "sigma")
  expect_exists(x, "request")
  expect_exists(x, "atol")
  expect_exists(x, "rtol")
  expect_exists(x, "hmin")
  expect_exists(x, "hmax")
  expect_exists(x, "maxsteps")
  expect_exists(x, "tscale")
  expect_exists(x, "digits")
  expect_exists(x, "code")
  expect_exists(x, "cmt")
  expect_exists(x, "neq")
  expect_exists(x, "pars")
  expect_exists(x, "npar")
  expect_exists(x, "preclean")
  expect_exists(x, "mindt")
  expect_exists(x, "debug")
  expect_exists(x, "verbose")
  expect_exists(x, "envir")
  expect_exists(x, "cfile")
  expect_exists(x, "shlib")
  expect_exists(x, "capture")
  expect_exists(x, "details")
  expect_exists(x, "sodll")
  expect_exists(x, "soloc")
  expect_exists(x, "covariates")
  expect_exists(x, "fixedp")
  expect_exists(x, "plugins")
  expect_exists(x, "random")
  expect_exists(x, "model")
  expect_exists(x, "project")
})
