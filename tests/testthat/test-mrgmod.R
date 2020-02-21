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

context("test-mrgmod")

mod <- mrgsolve::house()

test_that("methods", {
  expect_equal(mod$CL,1)
  expect_equal(mod$VC,20)
  expect_equal(mod$RESP,50)
  expect_equal(mod[["RESP"]],50)
  expect_equal(mod$end,120)
  expect_equal(mod[["end"]],mod@end)
  expect_error(mod$kylebaron) 
  expect_error(mod[["kylebaron"]],regexp = "not found or not extractable")
  expect_is(as.list(mod), "list")
  expect_output(summary(mod), "Model: housemodel")
  expect_true(mrgsolve:::valid.mrgmod(mod))
  expect_true(all.equal(mod, mrgsolve::house()))
  l <- mod[c("CL", "VC", "CENT", "end")]
  expect_identical(l, list(CL = mod$CL, VC = mod$VC, CENT = mod$CENT, end = mod$end))
  expect_error(mod[c("CL", "Kyle")],regexp = "not found or not extractable")
  x <- capture.output(see(mod))
  expect_true(grepl("Model file", x[2]))
  expect_true(grepl("housemodel\\.cpp", x[2]))
})

test_that("defaults issue-540", {
  mod <- modlib("pk1", compile = FALSE)  
  expect_equal(mod@mxhnil,2)
  expect_equal(mod@maxsteps,20000)
  expect_equal(mod@rtol,1e-8)
})

