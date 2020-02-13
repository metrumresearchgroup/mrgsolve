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

mod <- mrgsolve::house() %>% update(end = -1) %>% param(CL = 0, KA = 0)
mod <- carry_out(mod, evid)

# record sorting flag. Default value is 1. Possible values are 1,2,3,4: 

# 1 and 2
# put doses in a data set after padded observations at the same time; 
# 3 and 4
# put those doses before padded observations at the same time. 
# 2 and 4 will put
# doses scheduled through addl after observations at the same time; 
# 1 and 3 put
# doses scheduled through addl before observations at the same time. recsort
# will not change the order of your input data set if both doses and
# observations are given.

context("test-recsort")

# 1 and 2
# put doses in a data set after padded observations at the same time; 
test_that("recsort 1 and 2, data", {
  mod <- update(mod, add = c(1,1.0001), end = -1)
  out <- mrgsim_df(mod, ev(amt = 1, time = 1),recsort = 1)
  expect_equal(out$evid, c(0,1,0))
  expect_equal(out$GUT, c(0,1,1))
  out2 <- mrgsim_df(mod, ev(amt = 1, time = 1),recsort = 2)
  expect_identical(out2,out)
})

# 3 and 4
# put those doses before padded observations at the same time.
test_that("recsort 3 and 4, data", {
  mod <- update(mod, add = c(1,1.0001), end = -1)
  out <- mrgsim_df(mod, ev(amt = 1, time = 1), recsort = 3)  
  expect_equal(out$evid, c(1,0,0))
  expect_equal(out$GUT, c(1,1,1))
  out2 <- mrgsim_df(mod, ev(amt = 1, time = 1), recsort = 4)  
  expect_identical(out2,out)
})

# 2 and 4 will put
# doses scheduled through addl after observations at the same time; 
test_that("recsort 2 and 4, addl", {
  mod <- update(mod, add = c(1,1.0001))
  e <- ev(amt = 1, ii=0.5, addl = 1, time = 0.5)
  out <- mrgsim_df(mod,ev = e, recsort=2)
  expect_equal(out$evid, c(1,0,0))
  expect_equal(out$GUT, c(1,1,2))
  out2 <- mrgsim_df(mod, ev = e,recsort = 4)
  expect_identical(out2,out)
})

# 1 and 3 put
# doses scheduled through addl before observations at the same time. 
test_that("recsort 1 and 3, addl", {
  mod <- update(mod, add = c(0.999,1,1.0001))
  e <- ev(amt = 1, ii=0.5, addl = 1, time = 0.5)
  out <- mrgsim_df(mod,ev = e, recsort=1)
  expect_equal(out$evid, c(1,0,0,0))
  expect_equal(out$GUT, c(1,1,2,2))
  out2 <- mrgsim_df(mod, ev = e,recsort = 3)
  expect_identical(out2,out)
})
