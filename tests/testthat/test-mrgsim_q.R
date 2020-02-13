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

mod <- mrgsolve::house() %>% update(end=240)
cap <- as.list(mod)$capture

context("test-mrgsim_q")

data(exTheoph)
temp <- expand.ev(amt = c(100,300,100), rate = 100, F1 = 0.2, WT = 120)

test_that("simulation with a complete data set", {
  out1 <- mrgsim_d(mod,exTheoph)@data
  out2 <- mrgsim_q(mod,exTheoph)@data
  expect_identical(out1,out2)
})

test_that("simcall=1 is deprecated", {
  expect_error(mrgsim_q(mod,temp,stime=stime(mod),simcall=1))
})

test_that("qsim issue-490", {
  out1 <- mrgsim_df(mod,temp)
  out2 <- qsim(mod,temp,output="df")
  expect_identical(out1,out2)
})

