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
  expect_error(mrgmod_to_list(mod), "Nested environments")
})

test_that("invalid input is an error", {
  expect_error(mrgmod_to_list(list()), "must be a model object")
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
})

test_that("error when the version that created the list is unknown", {
  l <- mrgmod_to_list(house())
  msg <- "Cannot determine the version"
  l2 <- l
  attr(l2, "mrgsolve_version") <- NULL
  expect_error(mrgmod_from_list(l2), msg)
  attr(l2, "mrgsolve_version") <- NA_character_
  expect_error(mrgmod_from_list(l2), msg)
  attr(l2, "mrgsolve_version") <- c("2.0.0", "2.0.1")
  expect_error(mrgmod_from_list(l2), msg)
  attr(l2, "mrgsolve_version") <- 2
  expect_error(mrgmod_from_list(l2), msg)
})

test_that("S4 objects in the args slot are converted and restored", {
  mod <- ev(house(), amt = 100)
  expect_true(isS4(mod@args$events))
  l <- mrgmod_to_list(mod, deep = TRUE)
  expect_false(isS4(l$args$events))
  expect_s3_class(l$args$events, "s4_list")
  mod2 <- mrgmod_from_list(l)
  expect_true(is(mod2@args$events, "ev"))
  expect_identical(mod2@args, mod@args)
  expect_true(isTRUE(all.equal(mod, mod2)))
})

s4_slots <- function(x) {
  what <- methods::slotNames(x)
  what[vapply(what, function(w) isS4(methods::slot(x, w)), TRUE)]
}

test_that("default is shallow: S4 slots kept, envir converted", {
  mod <- ev(house(), amt = 100)
  l <- mrgmod_to_list(mod)
  expect_identical(l, mrgmod_to_list(mod, deep = FALSE))
  expect_identical(names(l), methods::slotNames(mod))
  expect_is(l$envir, "list")
  expect_true(length(s4_slots(mod)) > 0)
  for(w in s4_slots(mod)) {
    expect_true(isS4(l[[w]]), info = w)
    expect_identical(l[[w]], methods::slot(mod, w), info = w)
  }
  expect_true(isS4(l$args$events))
  expect_false(inherits(l$omega, "s4_list"))
})

test_that("deep conversion leaves no S4 objects", {
  mod <- ev(house(), amt = 100)
  l <- mrgmod_to_list(mod, deep = TRUE)
  for(w in s4_slots(mod)) {
    expect_false(isS4(l[[w]]), info = w)
    expect_s3_class(l[[w]], "s4_list")
  }
  expect_false(isS4(l$args$events))
  expect_is(l$envir, "list")
})

test_that("shallow and deep lists both restore the model", {
  mod <- ev(param(house(), CL = 5), amt = 100)
  for(deep in c(FALSE, TRUE)) {
    l <- mrgmod_to_list(mod, deep = deep)
    mod2 <- mrgmod_from_list(l)
    expect_true(isTRUE(all.equal(mod, mod2)), info = deep)
    expect_identical(mod2@args, mod@args)
    expect_equal(param(mod2)$CL, 5)
    expect_is(mod2@envir, "environment")
    expect_equal(
      as.data.frame(mrgsim(mod, end = 5)), 
      as.data.frame(mrgsim(mod2, end = 5))
    )
  }
})

test_that("shallow list keeps attributes, version check and checks apply", {
  l <- mrgmod_to_list(house())
  expect_identical(attr(l, "mrgmod_class"), class(house()))
  expect_identical(
    attr(l, "mrgsolve_version"), 
    as.character(utils::packageVersion("mrgsolve"))
  )
  attr(l, "mrgsolve_version") <- "0.0.1"
  expect_error(mrgmod_from_list(l), "0.0.1")
  l <- mrgmod_to_list(house())
  l$not_a_slot <- 1
  expect_error(mrgmod_from_list(l), "not slots of class")
})

test_that("nested environment is an error with deep = FALSE and TRUE", {
  mod <- house()
  assign("e", new.env(), envir = mod@envir)
  expect_error(mrgmod_to_list(mod, deep = FALSE), "Nested environments")
  expect_error(mrgmod_to_list(mod, deep = TRUE), "Nested environments")
})
