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


mod <- mrgsolve:::house() %>% update(end=5, delta=1) %>% Req(CP)


context("Testing carry_out of tran / PK dosing items")

ex <- rep(0,7)

test_that("carry.out amt", {
    out <- mod %>% ev(amt=800, rate=2) %>% carry_out(amt) %>% mrgsim
    ex[2] <- 800
    expect_equal(out$amt, ex)
})

test_that("carry.out rate", {
  out <- mod %>% ev(amt=800, rate=2) %>% carry_out(amt,rate,ss) %>% mrgsim
  ex[2] <- 2
  expect_equal(out$rate, ex)
  ex[2] <- 0
  expect_equal(out$ss,ex)
})

test_that("carry.out rate", {
  out <- mod %>% ev(amt=800, rate=2) %>% carry_out(amt,rate,ss) %>% mrgsim
  ex[2] <- 2
  expect_equal(out$rate, ex)

})

test_that("carry.out mixed", {
  out <- mod %>% ev(amt=800, rate=2, ii=12, addl=22) %>% carry_out(addl,ii,amt,rate,ss) %>% mrgsim
  expect_equal(names(out), c("ID", "time","amt", "ss", "ii", "addl", "rate", "CP"))
  expect_equivalent(as.data.frame(out)[2,2:7], list(0,800,0,12,22,2))
})

mod <- ev(mod, amt=200, ii=14, rate=11, ss=1,addl=99)
test_that("carry_out mixed, rename", {
  out <- mod %>% carry.out(a=addl,i=ii,d=amt,r=rate,s=ss) %>% mrgsim
  expect_equal(names(out), c("ID", "time","d", "s", "i", "a", "r", "CP"))
  expect_equivalent(as.data.frame(out)[2,2:7], list(0,200,1,14,99,11))
})

test_that("carry_out mixed, some rename", {
  out <- mod %>% carry.out(a=addl,ii,d=amt,rate,s=ss) %>% mrgsim
  expect_equal(names(out), c("ID", "time","d", "s", "ii", "a", "rate", "CP"))
  expect_equivalent(as.data.frame(out)[2,2:7], list(0,200,1,14,99,11))
})


