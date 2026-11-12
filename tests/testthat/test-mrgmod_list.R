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
library(testthat)
library(mrgsolve)
Sys.setenv(R_TESTS="")

context("test-mrgmod_list")

test_that("round trip mrgmod", {
  mod <- house()
  l <- mrgmod_to_list(mod)
  expect_is(l, "mrgmod_list")
  expect_identical(names(l), methods::slotNames(mod))
  expect_is(l$envir, "list")
  mod2 <- mrgmod_from_list(l)
  expect_is(mod2, "mrgmod")
  expect_true(isTRUE(all.equal(mod, mod2)))
  a <- mrgsim(mod, end = 5)
  b <- mrgsim(mod2, end = 5)
  expect_equal(as.data.frame(a), as.data.frame(b))
})

test_that("round trip preserves modifications", {
  mod <- param(house(), CL = 5, VC = 50)
  mod <- init(mod, GUT = 10)
  mod <- update(mod, end = 12, delta = 2)
  mod2 <- mrgmod_from_list(mrgmod_to_list(mod))
  expect_equal(param(mod2)$CL, 5)
  expect_equal(init(mod2)$GUT, 10)
  expect_equal(mod2@end, 12)
  expect_equal(mod2@delta, 2)
})

test_that("round trip preserves envir", {
  mod <- mcode("test_mrgmod_list_env", "$ENV a <- 1\nb <- 2", compile = FALSE)
  l <- mrgmod_to_list(mod)
  expect_is(l$envir, "list")
  expect_true(all(c("a", "b") %in% names(l$envir)))
  mod2 <- mrgmod_from_list(l)
  expect_is(mod2@envir, "environment")
  expect_false(identical(mod@envir, mod2@envir))
  expect_equal(mget(c("a", "b"), mod2@envir), list(a = 1, b = 2))
})

test_that("round trip preserves class", {
  mod <- house()
  expect_identical(attr(mrgmod_to_list(mod), "mrgmod_class"), class(mod))
  expect_identical(class(mrgmod_from_list(mrgmod_to_list(mod))), class(mod))
})

test_that("nested environment is an error", {
  mod <- house()
  assign("e", new.env(), envir = mod@envir)
  expect_error(mrgmod_to_list(mod), "nested environments")
})

test_that("invalid input is an error", {
  expect_error(mrgmod_to_list(list()))
  expect_error(mrgmod_from_list(list()), "mrgmod_list")
  expect_error(mrgmod_from_list(unclass(mrgmod_to_list(house()))), "mrgmod_list")
})

test_that("shlib is restored as is without checking for compiled model", {
  mod <- house()
  l <- mrgmod_to_list(mod)
  l$shlib$compiled <- TRUE
  l$soloc <- file.path(tempdir(), "does-not-exist")
  mod2 <- mrgmod_from_list(l)
  expect_identical(mod2@shlib, l$shlib)
  expect_true(mod2@shlib$compiled)
  expect_identical(mod2@soloc, l$soloc)
})

test_that("envir is the only environment slot", {
  mod <- house()
  what <- methods::slotNames(mod)
  is_env <- vapply(what, function(w) is.environment(methods::slot(mod, w)), TRUE)
  expect_identical(what[is_env], "envir")
})

test_that("list entries that are not slots is an error", {
  l <- mrgmod_to_list(house())
  l$not_a_slot <- 1
  expect_error(mrgmod_from_list(l), "not_a_slot")
  expect_error(mrgmod_from_list(l), "not slots of class")
})

test_that("round trip of a model from mcode", {
  mod <- modlib("nm-like", compile = FALSE)
  mod2 <- mrgmod_from_list(mrgmod_to_list(mod))
  expect_true(isTRUE(all.equal(mod, mod2)))
})

test_that("list records mrgsolve version and it must match on restore", {
  l <- mrgmod_to_list(house())
  current <- as.character(utils::packageVersion("mrgsolve"))
  expect_identical(attr(l, "mrgsolve_version"), current)
  attr(l, "mrgsolve_version") <- "0.0.1"
  expect_error(mrgmod_from_list(l), "0.0.1")
  attr(l, "mrgsolve_version") <- NULL
  expect_error(mrgmod_from_list(l), "<unknown>")
})
