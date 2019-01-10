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

context("test-as_list_mrgmod")

options(mrgsolve_mread_quiet=TRUE)

test_that("check items in as.list output", {
  x <- as.list(mrgsolve:::house())  
  x_names <- names(x)
  expect_true("start" %in% x_names)
  expect_true("end" %in% x_names)
  expect_true("delta" %in% x_names)
  expect_true("add" %in% x_names)
  expect_true("param" %in% x_names)
  expect_true("init" %in% x_names)
  expect_true("omega" %in% x_names)
  expect_true("sigma" %in% x_names)
  expect_true("request" %in% x_names)
  expect_true("atol" %in% x_names)
  expect_true("rtol" %in% x_names)
  expect_true("hmin" %in% x_names)
  expect_true("hmax" %in% x_names)
  expect_true("maxsteps" %in% x_names)
  expect_true("tscale" %in% x_names)
  expect_true("digits" %in% x_names)
  expect_true("code" %in% x_names)
  expect_true("cmt" %in% x_names)
  expect_true("neq" %in% x_names)
  expect_true("pars" %in% x_names)
  expect_true("npar" %in% x_names)
  expect_true("preclean" %in% x_names)
  expect_true("mindt" %in% x_names)
  expect_true("debug" %in% x_names)
  expect_true("verbose" %in% x_names)
  expect_true("envir" %in% x_names)
  expect_true("cfile" %in% x_names)
  expect_true("shlib" %in% x_names)
  expect_true("capture" %in% x_names)
  expect_true("details" %in% x_names)
  expect_true("sodll" %in% x_names)
  expect_true("soloc" %in% x_names)
  expect_true("covariates" %in% x_names)
  expect_true("fixed" %in% x_names)
  expect_true("plugins" %in% x_names)
  expect_true("random" %in% x_names)
  expect_true("model" %in% x_names)
  expect_true("project" %in% x_names)
})
