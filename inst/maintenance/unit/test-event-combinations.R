# Copyright (C) 2013 - 2019  Metrum Research Group
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


context("tran-inputs")
code <- '
[ param ] CL = 1, V=10
[ pkmodel ] 
cmt = "CENT"

[ main ] 
D_CENT = 2;
R_CENT = 10;
'
mod <- mcode("context_tran_inputs", code)

tr <- function(e) mrgsim(mod,e)
err <- function(e) inherits(try(tr(e),silent=TRUE), "try-error")

test_that("good events issue-249", {
  # Good
  a <- ev(amt = 100)
  expect_false(err(a))
  b <- ev(amt = 100, ii = 24)
  expect_false(err(b))
  d <- ev(amt = 100, ii = 24, addl = 4)
  expect_false(err(d))
  f <- ev(amt = 100, ii = 24, ss=1)
  expect_false(err(f))
  g <- ev(amt = 100, rate = 100)
  expect_false(err(g))
  k <- ev(amt = 0, ss=1, ii = 24, addl = 5)
  expect_false(err(k))
  n <- ev(amt = 0, ss=1, rate = -1, ii = 24)
  expect_false(err(n))
})

test_that("bad events issue-249", {
  # Bad
  c <- ev(amt = 100, addl = 4)
  expect_error(tr(c), "addl: 4")
  e <- ev(amt = 100, ss=1)
  expect_error(tr(e), "row: 1")
  h <- ev(amt = 100, rate = 100, ss=1)
  expect_error(tr(h), "row: 1")
  i <- ev(amt = 0, ss=1)
  expect_error(tr(i), "row: 1")
  j <- ev(amt = 0, ss=1, addl = 5)
  expect_error(tr(j), "addl: 5")
  l <- ev(amt = 0, ss=2)
  expect_error(tr(l), "row: 1")
  m <- ev(amt = 0, ss=1, rate = -2, ii = 24)
  expect_error(tr(m), "rate: -2")
  o <- ev(amt = 0, ss=1, ii = 24, addl = 5, rate = 3)
  expect_error(tr(o), "addl must be zero for ss infusion")
  p <- ev(amt = 100, cmt = 1E5)
  expect_error(tr(p), "cmt: 100000")
  p <- ev(amt = 100, addl = -1)
  expect_error(tr(p), "must not be negative")
  p <- ev(amt = 100, ss = -1)
  expect_error(tr(p), "must not be negative")
})

test_that("addl or ss are passed as negative numbers", {
  # Bad
  p <- ev(amt = 100, addl = -1)
  expect_error(tr(p), "must not be negative")
  p <- ev(amt = 100, ss = -1)
  expect_error(tr(p), "must not be negative")
})


