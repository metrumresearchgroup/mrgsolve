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

# Issue 299
context("test-evid4")

mod <- mrgsolve:::house()

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



