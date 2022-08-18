# Copyright (C) 2013 - 2020  Metrum Research Group
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

# Issue 299
context("test-evid4")

mod <- mrgsolve::house()

test_that("evid4 bolus dosing is the same as evid1", {
  e1 <- ev(amt = 100, ii = 24, addl = 3)
  e4 <- ev(amt = 100, ii = 24, addl = 3, evid = 4)
  out1 <- mrgsim(mod, events = e1) %>% as.data.frame
  out4 <- mrgsim(mod, events = e4) %>% as.data.frame
  expect_identical(out1, out4)
})

test_that("evid4 infusion dosing is the same as evid1", {
  e1 <- ev(amt = 100, ii = 24, addl = 3, rate = 10)
  e4 <- ev(amt = 100, ii = 24, addl = 3, rate = 10, evid = 4)
  out1 <- mrgsim(mod, events = e1) %>% as.data.frame
  out4 <- mrgsim(mod, events = e4) %>% as.data.frame
  expect_identical(out1, out4)
})

test_that("evid4 reset with infusion", {
  dose1 <- ev(amt = 100, rate = 1)
  dose2 <- ev(amt = 200, time = 3, evid = 4, rate = 1)
  dose <- c(dose1,dose2)
  out <- mrgsim_df(mod,dose, end = 4,carry_out = "evid",delta=1)
  expect_identical(out$evid[c(1,2,6)],c(0,1,4))
  expect_identical(out$GUT[6],0)
  expect_identical(out$CENT[6],0)
  expect_identical(out$DV[6],0)
  expect_identical(out$RESP[6],mod$KIN/mod$KOUT)
})

test_that("evid==4 with ss==1 [SLV-TEST-0019]", {
  mod <- house()
  dat <- as_data_set(
    evd(amt = 100, evid = 4, ss = 1, ii = 12), 
    evd(amt = 100, evid = 1, ss = 1, ii = 12),
    evd(amt = 100, evid = 4, ss = 0, ii = 0),
    evd(amt = 100, evid = 1, ss = 0, ii = 0)
  )
  sim <- mrgsim(mod, dat, end = 5, output = "df")
  expect_true(all(sim$CP[sim$ID==1] == sim$CP[sim$ID==2]))
  expect_true(all(sim$CP[sim$ID==3] == sim$CP[sim$ID==4]))
})
