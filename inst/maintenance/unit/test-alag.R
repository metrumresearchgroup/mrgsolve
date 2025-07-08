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
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

code <- '
$PARAM LAG = 2.8, CL = 1, V = 20
$PKMODEL cmt = "CENT"
$MAIN
ALAG_CENT = LAG;
'
mod <- mcode("alag1", code)

context("test-alag")

test_that("Lagged bolus", {
    out <- mod %>% ev(amt=100) %>% mrgsim(delta=0.01,add=c(2.7999999,2.8000001), end=10)
    first <- with(as_tibble(out),time[which(CENT > 0)[1]])
    expect_equal(first,2.8)
})

test_that("Very small lag time doesn't crash issue-109", {

  out <- mod %>% ev(amt=100) %>% param(LAG = 1E-30) %>% mrgsim
  expect_is(out, "mrgsims")
  expect_equal(out$time[which.max(out$CENT)],0)
  
  # 1 and 3 put doses scheduled through addl before observations at the same time.
  # if the lag time is at 2, then the dose should be after the observation
  out <- mod %>% ev(amt=100) %>% param(LAG = 2) %>% mrgsim(recsort=1)
  expect_is(out, "mrgsims")
  expect_equal(out$time[which.max(out$CENT)],3)
  # 3 and 4 put those doses before padded observations at the same time. 
  # recsort 2 would be showing the dose 
  out <- mod %>% ev(amt=100) %>% param(LAG = 2) %>% mrgsim(recsort=3)
  expect_is(out, "mrgsims")
  expect_equal(out$time[which.max(out$CENT)],2)
  out <- mod %>% ev(amt=100) %>% param(LAG = 1.5) %>% mrgsim
})

test_that("Lag time on SS record - bolus", {
  e <- ev(amt=100, ii = 12, LAGT = 5, addl = 10, ss = 1)
  out <- mrgsim(mod, ev = e, obsonly = TRUE, end=96, recsort = 3)
  pick <- filter(out, time %in% seq(0,240,12))
  cent <- round(pick$CENT,3)
  expect_true(all(cent==cent[1]))
})

test_that("Lag time on SS record - infusion", {
  e <- ev(amt=100, ii = 12, LAGT = 3, addl = 10, ss = 1, rate = 100/2)
  out <- mrgsim(mod, ev = e, obsonly = TRUE, end=96, recsort = 3,ss_rtol = 1e-8)
  pick <- filter(out, time %in% seq(0,240,12))
  cent <- round(pick$CENT,4)
  expect_true(all(cent==cent[1]))
  
  pick2 <- filter(out, time %in% (7+seq(0,240,12)))
  cent2 <- round(pick2$CENT,4)
  expect_true(all(cent2==cent2[1]))
})

#No longer an error
test_that("Error lagtime >= ii for bolus", {
  e <- ev(amt = 100, ii = 12, LAG = 20, ss = 1)
  expect_is(mrgsim(mod, ev = e), "mrgsims")
})

test_that("Error lagtime+duration >= ii for infusion", {
  e <- ev(amt=100, ii = 12, LAG = 3, rate = 100/10, ss = 1)
  expect_error(mrgsim(mod, ev = e))
})

test_that("ss dose with lag time issue-484", {
  data1 <- 
    ev(amt = 100,time=240,ss=1,ii=12) %>% 
    expand_observations(c(0,seq(240,264,4)))
  data2 <- dplyr::arrange(data1, time, 1-evid)
  out1 <- mrgsim_d(mod,data1) %>% slice(-c(1,2))
  out2 <- mrgsim_d(mod,data2) %>% slice(-c(1,2))
  expect_identical(out1,out2)
})

test_that("EVID 1 or 4 at SS with lag time are identical", {
  ev1 <- ev(amt = 100, ii = 24, ss = 1, LAG = 10)
  ev4 <- mutate(ev1, evid = 4)
  
  out1 <- mrgsim_e(mod, ev1, recsort = 3, output = "df")
  out4 <- mrgsim_e(mod, ev4, recsort = 3, output = "df")
  
  expect_identical(out1, out4)
  
  ev1a <- mutate(ev1, addl = 3, LAG = 5)
  ev4a <- mutate(ev4, addl = 3, LAG = 5)
  
  out1a <- mrgsim_e(mod, ev1a, recsort = 3, output = "df")
  out4a <- mrgsim_e(mod, ev4a, recsort = 3, output = "df")
  
  expect_identical(out1a, out4a)
})


code_alag_float <- '
$PKMODEL cmt = "A,B", depot = TRUE

$PARAM CL = 1, V = 22, KA = 1.2, lag = 1.2322

$OMEGA 1.2

$MAIN
ALAG_A = exp(log(lag) + ETA(1));
'
mod <- mcode("test-alag-float", code_alag_float, end = 24*87, delta = 24)

test_that("Prevent floating point issues with TIME and random ALAG + ADDL", {
  
  expect_length(unique(stime(mod)), 88)
  
  data <- expand.ev(amt = 100, ii = 12, addl = 100, ID = 1:1000)
  
  out <- mrgsim(
    mod,
    data = data,
    carry_out = "evid", 
    obsonly = TRUE
  )     
  
  expect_length(unique(out$time), 88) # Before fix, fails with length in the 90s
})
