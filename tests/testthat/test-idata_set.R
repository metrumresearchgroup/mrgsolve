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

mod <- mrgsolve:::house() %>% update(end=240)

context("test-data_set")

test_that("event with idata set", {
  idata <- data.frame(ID = c(11,33,55), CL = 2)
  e <- ev(amt = 100)
  out <- mrgsim(mod, idata = idata, events = e)
  expect_identical(unique(idata$ID), unique(out$ID))
})

test_that("data set with idata", {
  data(exTheoph)
  idata <- exTheoph %>% distinct(ID,WT)
  data <- mutate(exTheoph, WT = NULL, amt = ifelse(evid==1,4, 0))
  data <- filter(data, evid==1)
  out <- mrgsim(mod, data = data) %>% filter(time ==10)
  expect_true(all(out[["CENT"]] == out[["CENT"]][1]))
  
  dat <- filter(data, ID %in% c(1,8,12)) %>% mutate(WT = c(40,80,120))
  out <- mrgsim(mod, data = dat, idata = idata) %>% filter(time==20)
  expect_true(all(out[["CP"]][c(2,3)] != out[["CP"]][1]))
})












