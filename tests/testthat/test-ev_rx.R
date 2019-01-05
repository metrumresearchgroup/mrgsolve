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

context("test-ev_rx")

all.equal.ev <- function(a,b) {
  a <- a@data
  b <- b@data
  if(!identical(sort(names(a)),sort(names(b)))) {
    return(FALSE)
  }
  a <- a[,names(b)]
  return(identical(a,b))
}

test_that("parse dose only - bolus", {
  a <- ev_rx("100")
  b <- ev(amt = 100)
  expect_identical(a,b)
})

test_that("parse dose only - infusion", {
  a <- ev_rx("100 over 2")
  b <- ev(amt = 100, rate = 100/2)
  expect_identical(a,b)

  a <- ev_rx("100 ov 2")
  expect_identical(a,b)
})

test_that("parse dose plus additional - bolus", {
  a <- ev_rx("100 q 12 x 3")
  b <- ev(amt = 100,  ii = 12, addl = 2)
  expect_identical(a,b)
})

test_that("parse dose plus additional - infusion", {
  a <- ev_rx("100 over 10 q 12 x 3")
  b <- ev(amt = 100,  ii = 12, addl = 2, rate = 100/10)
  expect_identical(a,b)
})

test_that("parse multiple - infusion / bolus", {
  a <- ev_rx("100 over 10 q 12 x 3 then 200 q 24 x 2")
  b <- ev(amt = 100, ii = 12, addl = 2, rate = 100/10, )
  c <- ev(amt = 200, ii = 24, addl = 1)
  d <- ev_seq(b,c)
  expect_identical(a,d)

  a <- ev_rx("100 over 10 q 12 x 3 ,  200 q 24 x 2")
  expect_identical(a,d)
})

test_that("parse dose into compartment", {
  a <- ev_rx("100 over 10 in 4 q 12 x 3")
  b <- ev(amt = 100, cmt = 4, ii = 12, addl = 2,  rate = 100/10, )
  expect_identical(a,b)
})

test_that("dose can be in decimal or scientific", {
  a <- ev_rx("1.23E4")
  b <- ev(amt = 1.23E4)
  expect_identical(a,b)

  a <- ev_rx("1.23e-4")
  b <- ev(amt = 1.23e-4)
  expect_identical(a,b)

  a <- ev_rx("1.23E+4")
  b <- ev(amt = 1.23E+4)
  expect_identical(a,b)
})

test_that("infusion duration can be decimal", {
  a <- ev_rx("1000 over 1.3")
  b <- ev(amt = 1000, rate = 1000/1.3)
  expect_identical(a,b)
})

test_that("infusion duration can be decimal", {
  a <- ev_rx("1000 over 1.3")
  b <- ev(amt = 1000, rate = 1000/1.3)
  expect_identical(a,b)
})

test_that("after parameter can be decimal", {
  a <- ev_rx("1000 after 2")
  b <- ev(amt = 1000, time =2)
  expect_identical(a,b)

  a <- ev_rx("1000 after 2.93")
  b <- ev(amt = 1000, time=2.93)
  expect_identical(a,b)
})


