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

context("test-tgrid")

mod <- house()

test_that("tgrid", {
  x <- tgrid(0,24,1)
  expect_is(x,"tgrid")
  expect_length(stime(x),25)
  x <- c(x,x+100)
  expect_is(x,"tgrids")
  expect_length(stime(x),50)
  x <- c(1,2,3,4,5)
  expect_identical(stime(x),x)
})

test_that("stime can render length 0", {
  expect_length(stime(numeric(0)), 0)
  mod <- update(mod, end = -1, add = numeric(0))
  expect_length(stime(mod),0)
})

test_that("no extra time 0 record when no observations", {
  data <- ev(amt = 0, ii = 24, addl = 5) %>% realize_addl()
  out <- mrgsim(house(),data=data, end = -1)
  expect_identical(data$time,out$time)
  data <- filter(data, time !=0)
  out <- mrgsim(mod, data=data, end = -1)
  expect_identical(data$time,out$time)
})

