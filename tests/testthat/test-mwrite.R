# Copyright (C) 2013 - 2024  Metrum Research Group
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

context("test-mwrite")

test_that("convert list to code", {
  # This could be simpler, but haven't found an easy way to do `c = "a"`
  l <- list(a = 1, b = 1.234, c = "a", d = c("x", "y", "z"), e = FALSE)  
  x <- mrgsolve:::tocode(l)  
  expect_equal(x[1], "a = 1")
  expect_equal(x[2], "b = 1.234")
  expect_equal(x[3], 'c = "a"')
  expect_equal(x[4], 'd = c("x", "y", "z")')
  expect_equal(x[5], "e = FALSE")
})

test_that("convert model to list", {
  mod <- house(end = 26, delta = 2, outvars = "GUT, DV, CP")
  l <- mrgsolve:::mwrite_model_to_list(mod)
  expect_equal(l$format, "list")
  expect_equal(l$transport, 1)
  expect_equal(l$update$end, 26)
  expect_equal(l$update$delta, 2)
  expect_equal(l$update$outvars, c("GUT", "DV", "CP"))
  expect_equal(l$set$end, 120)
  expect_equal(l$set$delta, 0.25)
})
