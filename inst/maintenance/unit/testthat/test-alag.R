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

code <- '
$PARAM LAG = 2.8
$CMT CENT
$MAIN
ALAG_CENT = LAG;
'
mod <- mcode("alag1", code)


context("Lagged doses")

test_that("Lagged bolus", {
    out <- mod %>% ev(amt=100) %>% mrgsim(delta=0.01,add=c(2.7999999,2.8000001), end=10)
    first <- with(as.tbl(out),time[which(CENT > 0)[1]])
    expect_equal(first,2.8)
})

## Issue #109
test_that("Very small lag time doesn't crash", {

  out <- mod %>% ev(amt=100) %>% param(LAG = 1E-30) %>% mrgsim
  expect_is(out, "mrgsims")
  expect_equal(out$time[which.max(out$CENT)],0)
  
  out <- mod %>% ev(amt=100) %>% param(LAG = 2) %>% mrgsim
  expect_is(out, "mrgsims")
  expect_equal(out$time[which.max(out$CENT)],2)
  
  out <- mod %>% ev(amt=100) %>% param(LAG = 1.5) %>% mrgsim
  
})














