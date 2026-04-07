# Copyright (C) 2013 - 2026  Metrum Research Group
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

context("test-env")

test_that("ENV", {
  mod <- mcode("test_env_1", "$ENV a <- 1\nb <- 2",compile=FALSE)
  expect_is(mod, "mrgmod")
})  

test_that("ENV sub into $PARAM", {
  code <- '
  $ENV a <- 1; b <- 2
  $PARAM A = a, B = b
  '
  mod <- mcode("test_env_2", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  expect_identical(mrgsolve:::pars(mod),c("A", "B"))
  expect_equivalent(as.numeric(param(mod)), c(1,2))
})

test_that("ENV sub into $INIT", {
  code <- '
  $ENV a <- 1\n b <- 2
  $INIT A = a, B = b
  '
  mod <- mcode("test_env_3", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  expect_identical(mrgsolve:::cmt(mod),c("A", "B"))
  expect_equivalent(as.numeric(init(mod)), c(1,2))
})

test_that("Get $OMEGA matrix from $ENV", {
  code <- '
  $ENV mat <- dmat(11,22,33)
  $OMEGA  @object mat @labels z y x @name OM
  '
  mod <- mcode("test-env-5", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  m <- as.matrix(omat(mod))
  expect_identical(names(omat(mod)), "OM")
  expect_identical(diag(m),c(11,22,33))
  expect_identical(labels(omat(mod))[[1]], c("z", "y", "x"))
  
})


test_that("ENV sub into $FIXED", {
  code <- '
  $ENV a <- 1\n b <- 2
  $FIXED A = a, B = b
  '
  mod <- mcode("test-env-6", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  x <- allparam(mod)
  expect_identical(names(x),c("A", "B"))
  expect_equivalent(as.numeric(x), c(1,2))
  
  l <- env_ls(mod)
  expect_is(l,"data.frame")
})


code <- '
$ENV
x <- 1
y <- 2
e <- ev(amt=100)
df <- tibble(ID=1:2,amt=3)
'


test_that("env-funs", {
  mod <- mcode("env-funs", code, compile = FALSE)

  expect_is(env_ls(mod),"data.frame")

  mm <- env_update(mod,y=3)
  expect_equal(env_get(mm, "y"), 3)

  mmm <- env_eval(mod)
  expect_is(mmm,"mrgmod")
})

test_that("env_get.mrgmod retrieves object by name", {
  mod <- house(end = 1)
  assign("let", letters, mod@envir)
  result <- env_get(mod, "let")
  expect_identical(result, letters)
})


test_that("env_get.mrgsims retrieves same object as env_get.mrgmod", {
  mod <- house(end = 1)
  assign("let", letters, mod@envir)
  out <- mrgsim(mod)
  expect_identical(env_get(out, "let"), env_get(mod, "let"))
})

test_that("env_get_env.mrgmod returns an environment", {
  mod <- house(end = 1)
  e <- env_get_env(mod)
  expect_true(is.environment(e))
})

test_that("env_get_env.mrgsims returns an environment", {
  mod <- house(end = 1)
  out <- mrgsim(mod)
  e <- env_get_env(out)
  expect_true(is.environment(e))
})

test_that("env_get_obj is an alias to env_get", {
  mod <- house(end = 1)
  assign("lett", letters, mod@envir)
  out <- mrgsim(mod)
  a <- env_get(out, "lett")
  b <- env_get_obj(out, "lett")
  expect_identical(a,b)
  c <- env_get(mod, "lett")
  d <- env_get_obj(mod, "lett")
  expect_identical(c,d)
})

test_that("env_get_env returns the same environment from mod and out", {
  mod <- house(end = 1)
  out <- mrgsim(mod)
  expect_identical(env_get_env(mod), env_get_env(out))
})

test_that("objects assigned via env_get_env are visible via env_get from mrgsims", {
  mod <- house(end = 1)
  assign("let", letters, env_get_env(mod))
  out <- mrgsim(mod)
  expect_identical(env_get(out, "let"), letters)
})

test_that("env_get requires what argument for mrgmod", {
  mod <- house(end = 1)
  expect_error(env_get(mod), "missing, with no default")
  expect_error(env_get(mod, letters[1:2]), "must be character with length 1")
  expect_error(env_get(mod, FALSE), "must be character with length 1")
})

test_that("env_get errors on missing object", {
  mod <- house(end = 1)
  expect_error(env_get(mod, "noex"), "object 'noex' not found")
})
