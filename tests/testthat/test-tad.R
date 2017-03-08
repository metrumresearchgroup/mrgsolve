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

mod <- mrgsolve:::house(end=5, delta=1) %>% Req() 

test_that("tad", {
  out <- mrgsim(mod)
  outt <- mrgsim(mod, tad=TRUE)
  
  expect_identical(names(out), c("ID", "time"))
  expect_identical(names(outt), c("ID", "time", "tad"))
  
  e <- ev(amt=100, time=3)
  
  out <- mrgsim(mod,events=e,tad=TRUE)
  expect_identical(out$tad, c(-3,-2,-1,0,0,1,2))
  
  out <- mrgsim(mod,events=e,tad=TRUE,obsonly=TRUE)
  expect_identical(out$tad, c(-3,-2,-1,0,1,2))
  expect_identical(out$time, c(0,1,2,3,4,5))
  
  e <- ev(amt=100, time=3, ii=3, addl=2)
  out <- mrgsim(mod, events=e, end=11, obsonly=TRUE,tad=TRUE)
  expect_equal(out$time, seq(0,11))
  expect_equal(out$tad,c(-3,-2,-1,0,1,2,0,1,2,0,1,2))
})

