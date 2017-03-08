# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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

context("Test $ENV")

test_that("$ENV", {
  mod <- mcode("test_env_1", "$ENV a <- 1\nb <- 2",compile=FALSE)
  expect_is(mod, "mrgmod")
})  

test_that("$ENV sub into $PARAM", {
  code <- '
  $ENV a <- 1; b <- 2
  $PARAM A = a, B = b
  '
  mod <- mcode("test_env_2", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  expect_identical(mrgsolve:::pars(mod),c("A", "B"))
  expect_equivalent(as.numeric(param(mod)), c(1,2))
})

test_that("$ENV sub into $INIT", {
  code <- '
  $ENV a <- 1\n b <- 2
  $INIT A = a, B = b
  '
  mod <- mcode("test_env_3", code, compile=FALSE)
  expect_is(mod, "mrgmod")
  expect_identical(mrgsolve:::cmt(mod),c("A", "B"))
  expect_equivalent(as.numeric(init(mod)), c(1,2))
})

# test_that("$ENV sub into block option", {
#   code <- '
#   $ENV yes <- TRUE; a <- 1; b <- 2
#   $INIT >> annotated=yes
#   A: a : A
#   B: b : B
#   '
#   mod <- mcode("test-env-4", code, compile=FALSE)
#   expect_is(mod, "mrgmod")
#   expect_identical(cmt(mod),c("A", "B"))
#   expect_equivalent(as.numeric(init(mod)), c(1,2))
#   
# })

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


test_that("$ENV sub into $FIXED", {
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
df <- data_frame(ID=1:2,amt=3)
'


test_that("env-funs", {
  mod <- mcode("env-funs", code,compile=FALSE)
  l <- env_get(mod)
  expect_equal(names(l), c("x", "y", "e", "df"))
  expect_equal(l$y,2)
  
  expect_is(env_ls(mod),"data.frame")
  
  mm <- env_update(mod,y=3)
  l <- env_get(mm)
  expect_equal(l$y,3)
  
  mmm <- env_eval(mod)
  expect_is(mmm,"mrgmod")
  
})



