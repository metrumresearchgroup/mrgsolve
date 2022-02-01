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
[ capture ] NEWIND
'
mod <- mcode("ode_on_off_1", ode_on_off_1)

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

test_that("compartment with active infusion can be turned off", {
  mod <- house(end = 96)
  data1 <- c(
    # start a bunch of infusions into 2nd cmt
    # then off at time 20
    # then dose again 
    ev(amt = 100, rate = 1,  cmt =  2), 
    ev(amt = 500, rate = 1,  cmt =  2, time = 2),
    ev(amt = 50,  rate = 1,  cmt =  2, time = 5),
    ev(amt = 5,   rate = 1,  cmt =  2, time = 10),
    ev(amt = 0,   evid = 2,  cmt = -2, time = 20),
    ev(amt = 10,  time = 40, cmt =  2, rate = 10)
  ) %>% mutate(ID = 1) %>% as.data.frame()
  data2 <- c(
    # same thing, just one infusion
    ev(amt = 100, rate = 1,  cmt =  2), 
    ev(amt = 0,   evid = 2,  cmt = -2, time = 20), 
    ev(amt = 10,  time = 40, cmt =  2, rate = 10)
  ) %>% mutate(ID = 2) %>% as.data.frame()
  data3 <- c(
    # same thing, just do the test dose
    ev(amt = 10,  time = 40, cmt =  2, rate = 10)
  ) %>% mutate(ID = 3) %>% as.data.frame()
  data <- bind_rows(data1,data2,data3)
  out <- mrgsim_df(mod, data, end = 60) 
  ans <- filter(out, time >= 40)
  id <- ans$ID
  ans$ID <- NULL
  comp <- split(ans, id)
  for(i in 1:3) {
    rownames(comp[[i]]) <- NULL  
  }
  expect_equal(comp[[1]], comp[[2]])
  expect_equal(comp[[1]], comp[[3]])
})

test_that("evid 3 doesn't change NEWIND", {
  dose <- ev(amt = 0, evid = 3, cmt = 1, time = 5)
  out <- mrgsim(mod, dose, output = "df")
  expect_equal(out$NEWIND[1], 0)
  out <- out[-1,]
  expect_true(all(out$NEWIND==2))
  expect_equal(out$A[5], 5)
  expect_equal(out$A[6], 0)
  expect_equal(out$A[7], 1)
})
