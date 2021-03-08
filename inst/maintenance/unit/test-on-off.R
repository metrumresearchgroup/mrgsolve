# Copyright (C) 2013 - 2021  Metrum Research Group
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

context("test-on-off")

ode_on_off_1 <- '
[ param ] R = 1, F1 = 1
[ cmt   ] A
[ main  ] F_A = F1; 
[ ode   ] dxdt_A = R;
'

test_that("compartment is turned on when F is zero", {
  data <- c(
    ev(amt = 0, cmt = -1, evid = 2, time = 4,  F1 = 1), 
    ev(amt = 1, cmt =  1, evid = 1, time = 6,  F1 = 0)
  ) 
  # on until time ==4
  # off until time==6
  # run out to 24 hours
  # with events at time 4 and 6, there would be duplicate records at 
  # those times
  mod <- mcode("ode_on_off_1", ode_on_off_1)  
  out <- mrgsim(mod, data)
  ans <- out$A
  time <- out$time
  t1 <- 4
  t2 <- 6
  expect_equal(time, c(seq(0,4), 4, 5, 6, 6, seq(7,24)))
  expect_equal(ans[1:5], seq(1,5)-1)
  expect_equal(ans[6], 4)
  expect_equal(ans[7:9], c(0,0,0))
  expect_equal(ans[10:27], seq(1,24-t2))
})
