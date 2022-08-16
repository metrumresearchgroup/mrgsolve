# Copyright (C) 2013 - 2022  Metrum Research Group
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

context("test-S4")
#' These tests focus on monitoring items that are accessed via `.slot()` in the 
#' C++ simulation layer

test_that("test-S4 parameters are preserved", {
  mod <- house()
  mod <- param(mod, IC50 = 445)
  p1 <- param(mod)
  out <- mrgsim(mod)
  expect_identical(p1, param(mod))
  
  x <- touch_funs(mod)
  expect_identical(p1, param(mod))
})

test_that("test-S4 compartments are preserved", {
  mod <- house()
  mod <- init(mod, GUT = 987)
  i1 <- init(mod)
  out <- mrgsim(mod)
  expect_identical(i1, init(mod))
  
  x <- touch_funs(mod)
  expect_identical(i1, init(mod))
})

test_that("test-S4 capture is preserved", {
  mod <- house()
  cap <- mod@capture
  out <- mrgsim(mod)
  expect_identical(cap, mod@capture)
})

test_that("test-S4 Icap is preserved", {
  mod <- house()
  Icap <- mod@Icap
  out <- mrgsim(mod)
  expect_identical(Icap, mod@Icap)
})

test_that("test-S4 Icmt is preserved", {
  mod <- house()
  Icmt <- mod@Icmt
  out <- mrgsim(mod)
  expect_identical(Icmt, mod@Icmt) 
})

test_that("test-S4 OMEGA and SIGMA are preserved", {
  mod <- house()
  mod <- omat(mod, dmat(4,3,2,1))
  mod <- smat(mod, bmat(1.234))
  om <- omat(mod)
  sm <- smat(mod)
  out <- mrgsim(mod)
  expect_identical(om, omat(mod))
  expect_identical(sm, smat(mod))
})
