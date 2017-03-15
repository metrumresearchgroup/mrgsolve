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


code1 <- '
$SET request="A,B,C", end=2, delta=1
$CMT A B C
$PARAM a = 0, b = 0, c = 0, IFLAG = 0

$MAIN
if(IFLAG > 0) {
  A_0 = a;
  B_0 = b;
  C_0 = c;
}

' 

mod <- mcode("test-init", code1, warn=FALSE)

context("test-initials")


test_that("Set initials via init", {
  out <- mod %>% init(A=1, B=2, C=3) %>% mrgsim %>% filter(time==0)
  expect_equal(out$A,1)
  expect_equal(out$B,2)
  expect_equal(out$C,3)
  
})


test_that("Set initials via $MAIN", {
  
  out <- mod %>% param(a=1, b=2, c=3) %>% mrgsim %>% filter(time==0)
  expect_equal(out$A,0)
  expect_equal(out$B,0)
  expect_equal(out$C,0)

  out <- mod %>% param(a=11, b=22, c=33,IFLAG=1) %>% mrgsim %>% filter(time==0)
  expect_equal(out$A,11)
  expect_equal(out$B,22)
  expect_equal(out$C,33)

  out <- mod %>% param(a=11, b=22, c=33, IFLAG=1) %>% 
    ev(amt=100,cmt=2) %>%
    mrgsim %>% filter(time==0)
  
  expect_equal(out$A,c(11,11))
  expect_equal(out$B,c(22,122))
  expect_equal(out$C,c(33,33))
  
})


test_that("Set initials via idata", {
  id <- dplyr::data_frame(ID=1:3, C_0 = c(99,88,77), B_0 = c(6,7,8))
  out <- mod %>% idata_set(id) %>% mrgsim %>% filter(time==0)
  expect_equal(out$B,c(6,7,8))
  expect_equal(out$C,c(99,88,77))
})





