# Copyright (C) 2013 - 2025  Metrum Research Group
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
Sys.setenv(R_TESTS="")

context("test-mhash")

test_that("hash is a string and is repeatable", {
  mod <- house()
  h <- mhash(mod)
  expect_is(h, "character")
  expect_length(h, 1)
  expect_equal(nchar(h), 32)
  expect_identical(h, mhash(mod))
  expect_identical(h, mhash(house()))
})

test_that("hash changes when the model changes", {
  mod <- house()
  h <- mhash(mod)
  expect_false(identical(h, mhash(param(mod, CL = 5))))
  expect_false(identical(h, mhash(init(mod, GUT = 10))))
  expect_false(identical(h, mhash(update(mod, end = 12))))
  expect_identical(h, mhash(param(mod, CL = param(mod)$CL)))
})

test_that("hash includes the model environment", {
  a <- mcode("test_mhash_env", "$ENV a <- 1", compile = FALSE)
  b <- mcode("test_mhash_env", "$ENV a <- 2", compile = FALSE)
  expect_false(identical(mhash(a), mhash(b)))
  # object order in the environment does not matter
  mod <- house()
  assign("x", 1, envir = mod@envir)
  assign("y", 2, envir = mod@envir)
  mod2 <- house()
  assign("y", 2, envir = mod2@envir)
  assign("x", 1, envir = mod2@envir)
  expect_identical(mhash(mod), mhash(mod2))
})

test_that("hash ignores model location", {
  mod <- house()
  mod2 <- mod
  mod2@soloc <- file.path(tempdir(), "somewhere-else")
  mod2@project <- file.path(tempdir(), "another-place")
  expect_identical(mhash(mod), mhash(mod2))
})

test_that("hash works for a model that is not compiled", {
  mod <- mcode("test_mhash_nc", "$PARAM CL = 1", compile = FALSE)
  expect_is(mhash(mod), "character")
})

test_that("invalid input is an error", {
  expect_error(mhash(list()), "must be a model object")
})

test_that("hash ignores the random shared object name", {
  code <- "$PARAM CL = 1"
  a <- mcode("test_mhash_so", code, compile = FALSE)
  b <- mcode("test_mhash_so", code, compile = FALSE)
  expect_false(identical(a@package, b@package))
  expect_identical(mhash(a), mhash(b))
})
