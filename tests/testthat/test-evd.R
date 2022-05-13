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

context("test-evd")

test_that("create evd object", {
  a <- evd(amt = 100)
  expect_is(a, "ev")
  expect_equal(a@case, 1)
  b <- ev(amt = 100)
  expect_equal(b@case, 0)
  
  mod <- house()
  mod <- evd(mod, amt = 100)
  expect_identical(mod@args$events, a)
  
  c <- ev(a)
  expect_is(c, "ev")
  expect_equal(c@case, 0L)
  
  d <- evd(b)
  expect_is(d, "ev")
  expect_equal(d@case, 1L)
})

test_that("evd object has all lower case names", {
  a <- ev(amt = 100, ii = 12, addl = 23)
  b <- evd(amt = 100, ii = 12, addl = 23)
  expect_identical(names(a), names(b))
  expect_identical(names(a), tolower(names(b)))
})

test_that("evd object realize names", {
  a <- evd(amt = 100, ii = 12, addl = 23, ss = 1, rate = 2, 
          cmt = 5, time = 12, evid = 3, kyle = 0)
  
  b <- as.data.frame(a, add_ID = 1)
  tnames <- seq(length(names(a))-2)
  expect_identical(names(b)[tnames], toupper(names(a)[tnames]))
  c <- mrgsolve:::ev_to_ds(a)
  expect_identical(b, c)
  expect_identical(names(b)[tnames], toupper(names(a))[tnames])
})

test_that("evd object simulated names", {
  a <- evd(amt = 100)
  idata <- data.frame(ID = 1)
  mod <- update(house(), end = -1)
  out1 <- mrgsim(mod, a)
  out2 <- mrgsim_e(mod, a)
  out3 <- mrgsim_ei(mod, a, idata)
  out4 <- qsim(mod, a)
  out5 <- mrgsim_q(mod, a)
  out6 <- mrgsim_d(mod, a)
  out7 <- mrgsim_di(mod, a, idata)
  x <- names(out1)
  expect_equal(x, toupper(x))
  expect_equal(x, names(out2))
  expect_equal(x, names(out3))
  expect_equal(x, names(out4))
  expect_equal(x, names(out5))
  expect_equal(x, names(out6))
  expect_equal(x, names(out7))
})

test_that("evd object carry out tran names", {
  a <- evd(amt = 100, ii = 12, addl = 2, rate = 1)
  mod <- update(house(), end = -1)
  out1 <- mrgsim(mod, a, carry_out = "AMT, II, ADDL, RATE,CMT")
  out2 <- mrgsim(mod, a, carry_out = "amt, ii, addl, rate, cmt")
  expect_equal(names(out1), toupper(names(out1)))
  expect_equal(names(out2)[1:2], c("ID", "TIME"))
  carried <- names(out2)[seq(3,7)]
  expect_equal(carried, tolower(carried))
})

test_that("coerce ev object to evd", {
  a <- ev(amt = 100)
  b <- as.evd(a)
  expect_identical(a@case, 0L)
  expect_identical(b@case, 1L)
})

test_that("ev operations with evd objects", {
  a <- evd(amt = 100) 
  b <- ev(amt = 200)
  c <- evd(amt = 300)
  
  e <- ev_seq(a, b, a)
  expect_is(e, "ev")
  expect_equal(e@case, 1L)
  ee <- as.data.frame(e)
  expect_equal(names(ee), toupper(names(ee)))
  
  e2 <- ev_seq(b, a, b)
  expect_is(e2, "ev")
  expect_equal(e2@case, 0L)
  ee2 <- as.data.frame(e2)
  expect_equal(names(ee2), tolower(names(ee2)))
  
  e3 <- c(a, b)
  expect_is(e3, "ev")
  expect_equal(e3@case, 1L)
  
  e4 <- c(b, a)
  expect_is(e4, "ev")
  expect_equal(e4@case, 0L)
  
  d <- ev_rep(a, seq(3))
  expect_is(d, "data.frame")
  expect_equal(names(d), toupper(names(d)))

  d2 <- ev_rep(a, seq(3), n = 2)
  expect_is(d2, "data.frame")
  expect_equal(names(d2), toupper(names(d2)))

  d3 <- as_data_set(a, b, c)
  expect_is(d3, "data.frame")
  expect_equal(names(d3), toupper(names(d3)))
  
  d4 <- as_data_set(b, a, b)
  expect_is(d4, "data.frame")
  check <- names(d4)[-1]
  expect_equal(check, tolower(check))
  expect_equal(names(d4)[1], "ID")
  
  d5 <- as_data_set(a)
  expect_is(d5, "data.frame")
  check <- names(d5)[-1]
  expect_equal(check, toupper(check))
})

test_that("test-evd expand.evd and evd_expand [SLV-TEST-0003]", {
  data1 <- expand.ev(amt = 100, ii = 12, addl = 5, ss = 2)
  data2 <- expand.evd(amt = 100, ii = 12, addl = 5, ss = 2)
  data3 <- evd_expand(amt = 100, ii = 12, addl = 5, ss = 2)
  expect_identical(data2, uctran(data1))
  expect_identical(data3, data2)
})

test_that("test-evd coerce to ev", {
  x <- evd(amt = 100, cmt = 5)
  y <- as.ev(x)
  expect_identical(y, ev(amt = 100, cmt = 5))
})
