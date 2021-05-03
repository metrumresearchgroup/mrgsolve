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

context("test-pred")

mod <- mcode(
  "pred", 
  '$PARAM B = -1, beta0 = 100, beta1 = 0.1
  $OMEGA 2 0.3
  $PRED
  double beta0i = beta0 + ETA(1);
  double beta1i = beta1*exp(ETA(2));
  capture Y = beta0i + beta1i*B;
  '
)

modd <- mrgsolve:::house()

##' The model time grid is used
test_that("with no data set", {
  out <- mrgsim(mod)
  expect_is(out, "mrgsims")
  expect_true("time" %in% names(out))
  expect_true(identical(out$time,stime(mod)))
})

##' Time is assumed to be zero
test_that("data_set with no time", {
  d <- data.frame(ID = 1, A = seq(0,10), tyme = 1)
  out <- mrgsim_d(mod,d)
  expect_is(out, "mrgsims")
  expect_true("time" %in% names(out))
  expect_true(all(out$time==0))
})

test_that("data_set with negative times", {
  d <- data.frame(ID = 1, A = 2, time = seq(-10,10))
  out <- mrgsim_d(mod,d)
  expect_is(out, "mrgsims")
  expect_true("time" %in% names(out))
  expect_equal(out$time,d[["time"]])
  out2 <- mrgsim_d(modd,d)
  expect_equal(out2$time,d[["time"]])
})

test_that("time/TIME required when neq > 0", {
  data <- data.frame(A = 1, B = 2, ID = 1)
  expect_error(
    mrgsim_d(modd,data), 
    "A time or TIME column is required"
  )
})

test_that("time/TIME not required when neq > 0", {
  data <- data.frame(A = 1, B = 2, ID = 1)
  expect_is(
    mrgsim_d(mod,data), 
    "mrgsims"
  )
})

test_that("cmt with pred is zero", {
  expect_error(
    mod %>% ev(amt = 100) %>% mrgsim, 
    regexp="all records must have cmt set to zero"
  )
})

test_that("rate with pred is zero", {
  expect_error(
    mod %>% ev(amt = 0,cmt = 0, rate = 2) %>% mrgsim, 
    regexp="all records must have rate set to zero"
  )
})

test_that("ss with pred is zero", {
  expect_error(
    mod %>% ev(amt = 0, cmt = 0, ss = 1) %>% mrgsim, 
    regexp="all records must have ss set to zero"
  )
})

test_that("amt is ok", {
  expect_is(
    mod %>% ev(amt = 100, cmt = 0) %>% mrgsim, 
    "mrgsims"
  )
})

test_that("obsonly works with dollar-pred", {
  data <- tibble(time = c(0,1,2,3,4), evid = c(0,2,1,0,0), ID = 1)
  out1 <- mrgsim(mod, data)
  out2 <- mrgsim(mod, data, obsonly = TRUE)
  expect_equal(nrow(out1), nrow(data))
  expect_equal(out2$time, c(0,3,4))
})
