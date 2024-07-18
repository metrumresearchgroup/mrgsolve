# Copyright (C) 2013 - 2024  Metrum Research Group, LLC
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

context("test-as_data_set")


test_that("as_data_set basic", {
  e1 <- ev(amt = 100, ID = 1:2)
  e2 <- ev(amt = 200, ID = 1:3)
  data <- as_data_set(e1,e2)
  
  expect_equal(data$ID, c(1,2,3,4,5))
  expect_equal(data$amt, c(rep(100,2),rep(200,3)))
  
  data <- as_data_set(as.data.frame(e1),as.data.frame(e2))
  expect_equal(data$ID, c(1,2,3,4,5))
  expect_equal(data$amt, c(rep(100,2),rep(200,3)))
})

test_that("as_data_set with leading data frame", {
  sort_lower <- sort(c("ID", "time", "amt", "cmt", "evid"))
  sort_upper <- sort(toupper(sort_lower))
  
  # both lower case
  e1 <- as.data.frame(ev(amt = 100, ID = 1:2))
  e2 <- as.data.frame(ev(amt = 200, ID = 1:2))
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_lower)
  expect_identical(data$amt, c(100,100,200,200))
  expect_false(anyNA(data))

  # leading upper case
  e1 <- as.data.frame(evd(amt = 100, ID = 1:2))
  e2 <- as.data.frame(ev(amt = 200, ID = 1:2))
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_upper)
  expect_identical(data$AMT, c(100,100,200,200))
  expect_false(anyNA(data))
  
  # leading lower case
  e1 <- as.data.frame(ev(amt = 100, ID = 1:2))
  e2 <- as.data.frame(evd(amt = 200, ID = 1:2))
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_lower)
  expect_identical(data$amt, c(100,100,200,200))
  expect_false(anyNA(data))
})

test_that("as_data_set with data frame then event", {
  sort_lower <- sort(c("ID", "time", "amt", "cmt", "evid"))
  sort_upper <- sort(toupper(sort_lower))
  
  # both lower case
  e1 <- as.data.frame(ev(amt = 100, ID = 1:2))
  e2 <- ev(amt = 200, ID = 1:2)
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_lower)
  expect_identical(data$amt, c(100,100,200,200))
  expect_false(anyNA(data))
  
  # leading upper case
  e1 <- as.data.frame(evd(amt = 100, ID = 1:2))
  e2 <- ev(amt = 200, ID = 1:2)
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_upper)
  expect_identical(data$AMT, c(100,100,200,200))
  expect_false(anyNA(data))
  
  # leading lower case
  e1 <- as.data.frame(ev(amt = 100, ID = 1:2))
  e2 <- evd(amt = 200, ID = 1:2)
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_lower)
  expect_identical(data$amt, c(100,100,200,200))
  expect_false(anyNA(data))
})

test_that("as_data_set with event then data frame", {
  sort_lower <- sort(c("ID", "time", "amt", "cmt", "evid"))
  sort_upper <- sort(toupper(sort_lower))
  
  # both lower case
  e1 <- ev(amt = 100, ID = 1:2)
  e2 <- as.data.frame(ev(amt = 200, ID = 1:2))
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_lower)
  expect_identical(data$amt, c(100,100,200,200))
  expect_false(anyNA(data))
  
  # leading upper case
  e1 <- as.data.frame(evd(amt = 100, ID = 1:2))
  e2 <- ev(amt = 200, ID = 1:2)
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_upper)
  expect_identical(data$AMT, c(100,100,200,200))
  expect_false(anyNA(data))
  
  # leading lower case
  e1 <- ev(amt = 100, ID = 1:2)
  e2 <- as.data.frame(evd(amt = 200, ID = 1:2))
  data <- as_data_set(e1,e2)
  expect_identical(sort(names(data)), sort_lower)
  expect_identical(data$amt, c(100,100,200,200))
  expect_false(anyNA(data))
})

test_that("warning if both upper and lower case names", {
  # the warning comes from lctran in this case
  d1 <- data.frame(amt = 100, CMT = 5, RATE = 1, rate = 2, ii = 12)
  e1 <- ev(amt = 100)
  expect_warning(as_data_set(d1), "both upper and lower")
  expect_warning(as_data_set(d1,e1), "both upper and lower")
  expect_warning(as_data_set(d1,e1), "missing values")
})
