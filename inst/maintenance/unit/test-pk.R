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

skip_if_not_installed("pmxTools")
skip_if(!require("dplyr"))
select <- dplyr::select

library(pmxTools)

skeleton <- tibble(time = seq(0,240,1))
.cl <- 1
.v <- 20
.v1 <-  10
.v2 <- 200
.q <- 4
.ka <- 1.3

dosim <- function(mod,event,...) {
  mrgsim_e(
    mod, 
    event = event, ..., 
    output = "df", obsonly=TRUE, recsort=3, digits=5
  )
}

docalc <- function(data,e, fun, ...) {
  e <- as.data.frame(e)
  mutate(data, CPcalc = fun(time,CL=e$CL,V=e$V,dose=e$amt,...)) %>%
    mutate(CPcalc = signif(CPcalc,5))
}

docalc2 <- function(data,e, fun, ...) {
  e <- as.data.frame(e)
  mutate(data, CPcalc = fun(time,CL=e$CL,V1=e$V2,V2=e$V3,Q=e$Q,dose=e$amt,...)) %>%
    mutate(CPcalc = signif(CPcalc,5))
}

dotest <- function(out) {
  ans <- out %>% 
    summarise(
      difference = sum(CP-CPcalc), 
      status = abs(difference) < 1E-6,
      date = Sys.Date()
    ) %>% select(status,total_difference = difference,date) 
  if(!ans$status) warning("STATUS: FAIL",call.=FALSE,immediate.=TRUE)
  return(ans$status)
}

mod <- modlib("pk1",end=48,delta=0.1,ss_rtol=1e-10)
mod1 <- mod

context("One compartment model tests")
test_that("one-compartment, bolus", {
  e <- ev(amt = 100, cmt = 2, CL=.cl,V=.v)
  out <- dosim(mod,e)
  out <- docalc(out,e,calc_sd_1cmt_linear_bolus)
  expect_true(dotest(out))
})

test_that("one-compartment, bolus, ss", {
  e <- ev(amt = 100, cmt = 2, CL=.cl,V=.v, ss = 1, ii = 24)
  out <- dosim(mod,e)
  out <- docalc(out,e,calc_ss_1cmt_linear_bolus, tau=24)
  expect_true(dotest(out))
})

test_that("one-compartment, oral, first", {
  e <- ev(amt = 100, cmt = 1, CL=.cl, V =.v, KA = .ka)
  out <- dosim(mod,e)
  out <- docalc(out,e,calc_sd_1cmt_linear_oral_1, ka = .ka)
  expect_true(dotest(out))
})

test_that("one-compartment, oral, first, ss", {
  e <- ev(amt = 100, cmt = 1, CL=.cl, V =.v, KA = .ka, ss=1, ii = 24)
  out <- dosim(mod,e)
  out <- docalc(out,e,calc_ss_1cmt_linear_oral_1, ka = .ka, tau=24 )
  expect_true(dotest(out))
})

test_that("one-compartment, infusion", {
  e <- ev(amt = 100, cmt = 2, CL=.cl,V=.v, rate = 10)
  out <- dosim(mod,e)
  out <- docalc(out,e,calc_sd_1cmt_linear_infusion,tinf=10)
  expect_true(dotest(out))
})

test_that("one-compartment, infusion tau", {
  e <- ev(amt = 100, cmt = 2, CL=.cl,V=.v, rate = 100/8, ss=1, ii = 24)
  out <- dosim(mod,e)
  out <- docalc(out,e,calc_ss_1cmt_linear_infusion,tinf=8, tau = 24)
  expect_true(dotest(out))
})

context("Two compartment model tests")

mod <- modlib("pk2",end = 48, delta=0.1,ss_rtol = 1e-10)

test_that("two-compartment, bolus", {
  e <- ev(amt = 100, cmt = 2, CL=.cl,V2=.v1, V3=.v2, Q = .q)
  out <- dosim(mod,e)
  out <- docalc2(out,e,calc_sd_2cmt_linear_bolus)
  expect_true(dotest(out))
})

test_that("two-compartment, bolus, ss", {
  e <- ev(amt = 100, cmt = 2, CL=.cl, V2=.v1, V3=.v2, Q = .q, ss = 1, ii = 24)
  out <- dosim(mod,e, rtol = 1e-10)
  out <- docalc2(out,e,calc_ss_2cmt_linear_bolus, tau=24)
  expect_true(dotest(out))
})

test_that("two-compartment, bolus, first", {
  e <- ev(amt = 100, cmt = 1, CL=.cl, V2=.v1, V3=.v2, Q = .q, KA = .ka)
  out <- dosim(mod,e)
  out <- docalc2(out,e,calc_sd_2cmt_linear_oral_1, ka = .ka)
  expect_true(dotest(out))
})

test_that("two-compartment, bolus, first, ss", {
  e <- ev(amt = 100, cmt = 1, CL=.cl, V2=.v1, V3=.v2, Q = .q, KA = .ka, ss=1, ii = 24)
  out <- dosim(mod,e,rtol = 1e-10)
  out <- docalc2(out,e,calc_ss_2cmt_linear_oral_1, ka = .ka, tau=24 )
  expect_true(dotest(out))
})

test_that("two-compartment, infusion", {
  e <- ev(amt = 100, cmt = 2, CL=.cl, V2=.v1, V3=.v2, Q = .q, rate = 10)
  out <- dosim(mod,e)
  out <- docalc2(out,e,calc_sd_2cmt_linear_infusion,tinf=10)
  expect_true(dotest(out))
})

test_that("two-compartment, infusion, ss", {
  e <- ev(amt = 100, cmt = 2, CL=.cl, V2=.v1, V3=.v2, Q = .q, rate = 10, ss=1, ii = 24)
  out <- dosim(mod,e, rtol = 1e-10)
  out <- docalc2(out,e,calc_ss_2cmt_linear_infusion, tinf=10, tau = 24)
  expect_true(dotest(out))
})

context("Multiple dosing - 1 cmt")

parspo <- list(CL = .cl, V = .v, ka = .ka)
parsiv <- list(CL = .cl, V = .v)
parsivi <- list(CL = .cl, V = .v, tinf=10)

test_that("one-compartment, bolus, multiple", {
  e <- ev(amt = 100, cmt = 2, CL=.cl, V = .v, ii = 24, addl=3)
  out <- dosim(mod1,e, end = 120, delta = 1)
  calc <- pk_curve(seq(0,120), model = "1cmt_bolus", params = parsiv, dose=100, ii =24, addl=3)
  out[["CPcalc"]] <- signif(calc[["cp"]],5)
  expect_true(dotest(out))
})

test_that("one-compartment, infusion, multiple", {
  e <- ev(amt = 100, cmt = 2, CL=.cl, V=.v, ii = 24, addl=3, rate=10)
  out <- dosim(mod1,e, end = 180, delta = 0.25)
  calc <- pk_curve(seq(0,180,0.25), model = "1cmt_infusion", params = parsivi, dose=100, ii =24, addl=3)
  out[["CPcalc"]] <- signif(calc[["cp"]],5)
  expect_true(dotest(out))
})

test_that("one-compartment, oral, multiple", {
  e <- ev(amt = 100, cmt = 1, CL=.cl, V=.v, KA = .ka, ii = 24, addl=3)
  out <- dosim(mod1,e, end = 180, delta = 0.25)
  calc <- pk_curve(seq(0,180,0.25), model = "1cmt_oral", params = parspo, dose=100, ii=24, addl=3)
  out[["CPcalc"]] <- signif(calc[["cp"]],5)
  expect_true(dotest(out))
})

context("Multiple dosing - 2 cmt")

parspo <- list(CL = .cl, V1 = .v1, V2 = .v2, Q = .q, ka = .ka)
parsiv <- list(CL = .cl, V1 = .v1, V2 = .v2, Q = .q)
parsivi <- list(CL = .cl, V1 = .v1, V2 = .v2, Q = .q,tinf=10)

test_that("two-compartment, bolus, multiple", {
  e <- ev(amt = 100, cmt = 2, CL=.cl, V2=.v1, V3=.v2, Q = .q, ii = 24, addl=3)
  out <- dosim(mod,e, end = 120, delta = 1)
  calc <- pk_curve(seq(0,120), model = "2cmt_bolus", params = parsiv, dose=100, ii =24, addl=3)
  out[["CPcalc"]] <- signif(calc[["cp"]],5)
  expect_true(dotest(out))
})

test_that("two-compartment, infusion, multiple", {
  e <- ev(amt = 100, cmt = 2, CL=.cl, V2=.v1, V3=.v2, Q = .q, ii = 24, addl=3, rate=10)
  out <- dosim(mod,e, end = 180, delta = 0.25)
  calc <- pk_curve(seq(0,180,0.25), model = "2cmt_infusion", params = parsivi, dose=100, ii =24, addl=3)
  out[["CPcalc"]] <- signif(calc[["cp"]],5)
  expect_true(dotest(out))
})

test_that("two-compartment, oral, multiple", {
  e <- ev(amt = 100, cmt = 1, CL=.cl, V2=.v1, V3=.v2, Q = .q, KA = .ka, 
          ii = 24, addl=3)
  out <- dosim(mod,e, end = 180, delta = 0.25)
  calc <- pk_curve(seq(0,180,0.25), model = "2cmt_oral", params = parspo, dose=100, ii =24, addl=3)
  out[["CPcalc"]] <- signif(calc[["cp"]],5)
  expect_true(dotest(out))
})
