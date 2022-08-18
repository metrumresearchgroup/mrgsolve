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

context("test-param")

test_that("params are constructed", {
  x <- param(A = 1, B = 2)  
  expect_is(x, "parameter_list")
  x <- param(list(A = 1, B = 2))
  expect_is(x, "parameter_list")
  x <- param(c(A = 1, B = 2))
  expect_is(x, "parameter_list")
  expect_error(param(A = c(1,2)))
  expect_error(param(A = "B"))
  capture.output(expect_error(param(A.2 = 3)))
})

test_that("params are accessed", {
  mod <- house()
  expect_identical(mod$CL, unname(mod@param@data$CL))
  expect_identical(mod[["KA"]], unname(mod@param@data$KA))
})

test_that("params are updated from dots [SLV-TEST-0013]", {
  mod <- house()
  mod2 <- param(mod, CL = 55, VC = 111)
  expect_equal(mod2$CL,55)
  expect_equal(mod2$VC,111)
})
  
test_that("params are updated from list [SLV-TEST-0014]", {
  mod2 <- update(house(), param = list(CL=99, KA = 9))
  expect_equal(mod2$CL,99)
  expect_equal(mod2$KA,9)
})

test_that("params are update from data frame [SLV-TEST-0015]", {
  data <- data.frame(CL = 44, D1 = 999)
  mod <- house()
  mod2 <- param(mod, data)
  expect_equal(mod2$CL,44)
  expect_equal(mod2$D1,999)
  expect_equal(mod$VC, mod2$VC)
})
  
test_that("param update errors or warnings [SLV-TEST-0016]", {
  mod <- house()
  expect_error(
    param(mod, KYLE = 2, CL = 5), 
    regexp = "[param-update] not a model parameter", 
    fixed = TRUE
  )
  expect_error(
    param(mod, KA = "A", CL = 1), 
    regexp = "[param-update] parameters must be single numeric values.", 
    fixed = TRUE
  )
  expect_warning(
    param(mod, list(ABC = 123)), 
    regexp="[param-update] no matching items to update", 
    fixed = TRUE
  )
  expect_error(
    param(mod, list()), 
    regexp ="[param-update] all parameters must have names",
    fixed = TRUE
  )
  expect_error(
    param(mod, list(CL = 5, 2)), 
    regexp ="[param-update] all parameters must have names",
    fixed = TRUE
  )
  expect_silent(param(mod, NULL))
  expect_error(
    param(mod, list(CL = factor(5))), 
    regexp ="[param-update] parameters must be single numeric values", 
    fixed = TRUE
  )
})

test_that("non-parameter character values are ignored  [SLV-TEST-0017]", {
  mod <- house()
  expect_silent(mod <- param(mod, list(CL = 5, ABC = "abc")))
  expect_s4_class(mod, class="mrgmod")
})

test_that("param update can be strict  [SLV-TEST-0018]", {
  mod <- house()
  expect_error(
    param(mod, list(CL = 1, KYLE = 2), .strict = TRUE), 
    regexp = "[param-update] not a model parameter", 
    fixed = TRUE
  )
})

test_that("params are shown", {
  mod <- house()
  x <- capture.output(param(mod))
  expect_match(x[2], "Model parameters")
})
