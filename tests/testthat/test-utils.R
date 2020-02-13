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

context("test-utils")


a <- list(a = 1, b = 2, c = 3)
b <- list(b = 4, c = 5, d = 6)

test_that("merge two lists, open", {
  ans <- merge(a,b, open = TRUE)  
  expect_equal(names(ans), c("a", "b", "c", "d"))
  expect_equal(ans$b,4)
  expect_equal(ans$d, 6)
})

test_that("merge two lists, closed", {
  ans <- merge(a,b, open = FALSE)  
  expect_equal(names(ans), c("a", "b", "c"))
  expect_equal(ans$b,4)
  expect_equal(ans$c, 5)
})

test_that("combine_list", {
    ans <- merge(a,b, open = TRUE)
    ans_combined <- mrgsolve:::combine_list(a,b)
    expect_identical(ans,ans_combined)
    
    ans <- merge(b,a, open = TRUE)
    ans_combined <- mrgsolve:::combine_list(b,a)
    expect_identical(ans,ans_combined)
    
    ans <- merge(a,list(), open = TRUE)
    ans_combined <- mrgsolve:::combine_list(a,list())
    expect_identical(ans,ans_combined)
    
    ans <- merge(list(),b, open = TRUE)
    ans_combined <- mrgsolve:::combine_list(list(),b)
    expect_identical(ans,ans_combined)
})




mod <- mrgsolve::house()
out <- mrgsim(mod)

test_that("Corecing simulated output to data.frame", {
  expect_is(as.data.frame(out), "data.frame")
})
test_that("Corecing simulated output to matrix", {
  expect_is(as.matrix(out), "matrix")
})
test_that("Corecing parameters to list", {
  expect_is(as.list(param(mod)), "list")
})
test_that("Corecing parameters to numeric", {
  expect_is(as.numeric(param(mod)), "numeric")
})
test_that("Corecing initials to list", {
  expect_is(as.list(init(mod)), "list")
})
test_that("Corecing initials to numeric", {
  expect_is(as.numeric(init(mod)), "numeric")
})
test_that("Corecing parameters to data.frame", {
  expect_is(as.data.frame(param(mod)), "data.frame")
  
})
test_that("Corecing initials to data.frame", {
  expect_is(as.data.frame(init(mod)), "data.frame")
})
test_that("stime correctly generates simulation times", {
  expect_equal(stime(mod), seq(0,mod@end, mod@delta))
  expect_equal(stime(update(mod, end=-1, add=c(1,6,9))), c(1,6,9))
  expect_error(stime(update(mod, end=-1, add=c())))
})
test_that("Negative end time gives simulations at add only", {
  expect_equal(stime(update(mod, end=-1, add=c(1,6,9))), c(1,6,9))
  expect_error(stime(update(mod, end=-1, add=c())))
})
test_that("If no simulation times can be rendered time=0 only is simulated", {
  out <- mrgsim(mod, end=-1, add=numeric(0))
  expect_equal(unique(out$time),0)
})

test_that("expand.ev issue-513", {
  df <- expand.ev(amt = c(1,2,3))
  expect_is(df,"data.frame")
  expect_identical(df[["amt"]],c(1,2,3))
  df <- expand.ev(amt = 100, ii = 24, total = 20)
  expect_identical(df[["addl"]],19)
  df <- expand.ev(amt = 100, tinf = 2)
  expect_identical(df[["rate"]], 100/2)
  df <- expand.ev(amt = 100, ii = 24, until = 168)
  expect_identical(df[["addl"]], 6)
})

test_that("tovec", {
  x <- mrgsolve:::tovec("a,b,c")
  expect_identical(x, c("a", "b", "c"))
  x <- mrgsolve:::tovec("1,2,3,4")
  expect_identical(x, c(1L,2L,3L,4L))
})

test_that("cvec", {
  expect_identical(cvec("a, b  , c"),letters[1:3])  
})

