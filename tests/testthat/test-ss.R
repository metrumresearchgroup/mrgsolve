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

context("test-ss")

test_that("ss_n and ss_fixed issue-533", {
  mod <- mrgsolve::house(end = 72,delta=4) %>% param(VC = 50)
  dose <- ev(amt = 100, ii = 24, ss=1, cmt=2, addl=2, ID=123)
  out <- mrgsim_e(mod,dose,recsort=3)
  expect_is(out,"mrgsims")
  expect_warning(out2 <- mrgsim_e(mod,dose, ss_n = 3,recsort=3), 
                 "\\[steady_bolus\\] ID 123 failed to reach steady state")
  expect_true(all(out2$CP != out$CP))
  expect_silent(out3 <- mrgsim_e(mod,dose, ss_n = 3, ss_fixed=TRUE, recsort=3))
  expect_true(all(out3$CP != out$CP))
  e <- ev(amt = 100, ii = 12, ss=1, cmt=2, rate = 5, ID = 321)
  expect_warning(mrgsim_e(mod,e, ss_n=3),
                 "\\[steady_infusion\\] ID 321 failed to reach steady state")
  e <- ev(amt = 0, ii = 12, ss = 1, rate = 5, ID = 246)
  expect_warning(mrgsim_e(mod,e, ss_n=3), 
                 "\\[steady_zero\\] ID 246 failed to reach steady state")
})
