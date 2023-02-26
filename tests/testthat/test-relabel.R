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

context("test-relabel")

test_that("rename, from vector", {
  x <- mrgsolve:::cvec_cs("a b = boy, cat = c, z, y = yak")
  obj <- mrgsolve:::.ren.create(x)
  a <- c("a", "b", "cat", "z", "y")
  b <- c("a", "boy", "c", "z", "yak")
  expect_equal(a, obj$new)
  expect_equal(b, obj$old)
  expect_equal(setNames(a,b),mrgsolve:::.ren.chr(obj))
})

test_that("rename, from string", {
  x <- "a b = boy cat = c z y = yak"
  obj <- mrgsolve:::.ren.create(x)
  a <- c("a", "b", "cat", "z", "y")
  b <- c("a", "boy", "c", "z", "yak")
  expect_equal(a, obj$new)
  expect_equal(b, obj$old)
  expect_equal(setNames(a,b),mrgsolve:::.ren.chr(obj))
})


test_that("ren rename", {
  
  x <- mrgsolve:::.ren.create("a = apple b c = Cat")
  out <- mrgsolve:::.ren.rename(x,c("fiddle", "apple", "Cat", "toothpick", "a"))
  expect_equal(out, c("fiddle", "a", "c", "toothpick", "a"))

  x <- mrgsolve:::.ren.create("a b c = c d = d")
  out <- mrgsolve:::.ren.rename(x,c("kyle", "baron", "b", "d"))
  expect_equal(out, c("kyle", "baron", "b", "d"))
  
})



