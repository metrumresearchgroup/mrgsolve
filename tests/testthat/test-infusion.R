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


mod <- mrgsolve:::house()

context("Infusions")

## Issue 43
test_that("Infusion with amt == 0", {
  expect_error(mod %>% ev(amt=0,rate=1000) %>% mrgsim)
})

## Issue 43
test_that("Infusion with large rate and small amount", {
  out <- try(mod %>% ev(amt=0.00000001,rate=1000000) %>% mrgsim(atol=1E-25,end=12))
  expect_is(out,"mrgsims")
  sims <- out %>% filter(time > 0)
  expect_true(all(sims$CENT > 0))
  expect_true(all(sims$GUT >  0))
})

## Issue 43
test_that("Infusion ends at the proper time", {
  out <- mod %>% ev(amt=3000, rate=30,cmt=2) %>% mrgsim(end=200, delta=0.01)
  tmax <- with(as.data.frame(out), time[which(CENT == max(CENT))])
  expect_equal(tmax,100)
})


test_that("Consecutive infusions act as one long infusion",{
  b <- ev(amt=200,cmt=2,time=0)
  i <-  ev(amt=1000, ii=100, rate=10,addl=9,cmt=2)
  d <- b+i
  out <- 
    mod %>% 
    ev(d) %>% 
    obsonly %>% 
    mrgsim(end=1000,delta=0.1,recsort=3, atol=1E-12, rtol=1E-12, digits=6)
  expect_true(all(out$CENT==200))
  
  d <- expand.ev(ID=1, time=seq(0,1000,100),rate=10,cmt=2,amt=1000) %>% mutate(ID=1)
  b <- d %>% filter(time==0) %>% mutate(amt=200,rate=0)
  data <- bind_rows(b,d)
  out <- 
    mod %>% 
    data_set(data) %>% 
    obsonly %>% 
    mrgsim(end=1000,delta=0.1,recsort=3, atol=1E-12, rtol=1E-12, digits=6)
  expect_true(all(out$CENT==200))

})

test_that("Same results from addl and explicit doses",{
  i <-  ev(amt=1000, ii=24, rate=100,addl=9,cmt=2)
  out1 <- 
    mod %>% 
    ev(i) %>% 
    obsonly %>% 
    mrgsim(end=240,delta=0.1,recsort=3, atol=1E-12, rtol=1E-12, digits=6)
  
  d <- expand.ev(ID=1, time=seq(0,240,24),rate=100,cmt=2,amt=1000) %>% dplyr::mutate(ID=1)
  out2 <- 
    mod %>% 
    data_set(d) %>% 
    obsonly %>% 
    mrgsim(end=240,delta=0.1,recsort=3, atol=1E-12, rtol=1E-12, digits=6)
  
  expect_true(all(out1$CENT==out2$CENT))
  expect_true(all(out1$RESP==out2$RESP))
})


test_that("Infusion with duration a multiple of ii", {
    out <- try(mod %>% ev(amt=100,rate=10,ii=1,addl=200,cmt=2) %>% mrgsim(delta=0.5))
    expect_is(out, "mrgsims")
})



context("Infusions that end too close to observations.")

mod  <- mrgsolve:::house()

test_that("Infusion with no obs overlap", {
  mod <- mod %>% ev(amt=6, rate=5)
  expect_is(mod %>% mrgsim, "mrgsims")
})

test_that("Infusion with obs overlap", {
  mod <- mod %>% ev(amt=6, rate=5)
  expect_is(mod %>% mrgsim(delta=0.1), "mrgsims")
  #expect_error(mod %>% update(mindt=0) %>% mrgsim(delta=0.1))
})


context("Infusion with ss flag")

test_that("Infusion executes with ss flag and ii==dur", {
  out <- 
    mod %>% 
    Req(CP) %>% obsonly %>%
    ev(amt=100,rate=100,ii=1,addl=10,ss=1) %>% 
    mrgsim(end=10,digits=5) %>% filter(time>0)
  
  expect_true(all(out$CP==100))
  
})


