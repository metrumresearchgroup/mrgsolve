# Copyright (C) 2013 - 2019  Metrum Research Group
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

context("test-ev")

test_that("observations are not allowed", {
  expect_error(ev(amt=100, evid=0))
})

test_that("doses are required", {
  expect_error(ev(time=24), "amt")
})

test_that("ev.ev", {
  x <- ev(ev(amt=100))
  expect_is(x,"ev")
})

test_that("event requirements and defaults", {
  expect_is(ev(amt=100), "ev")
  df <- as.data.frame(ev(amt=100))
  expect_equal(df$time, 0)
  expect_equal(df$evid,1)
  expect_equal(df$cmt,1)
})

test_that("collection of events", {
  e1 <- ev(amt=200)
  e2 <- ev(amt=100,time=1)
  e <- c(e1,e2)
  expect_is(e,"ev")
  e <- as.data.frame(e)
  expect_equal(e$time, c(0,1))
  expect_equal(e$amt, c(200,100))
})

test_that("realized events", {
  e <- as.data.frame(ev(amt=100, ii=24, addl=4))
  expect_equal(nrow(e),1)
  e <- as.data.frame(ev(amt=100, ii=24, addl=4,realize=TRUE))
  expect_equal(nrow(e),5)
  expect_true(all(e$amt==100))
  expect_true(all(e$ii==0))
  expect_true(all(e$addl==0))

  e1 <- ev(amt=100, ii=24, addl=1)
  e2 <- ev(amt=200, ii=24, addl=3, time=1)
  e <- as.data.frame(ev(e1+e2,realize_addl=TRUE))
  expect_equal(e$time, c(0,1,24,25,49,73))
})

test_that("realized event error", {
  expect_error(ev(amt=100, addl=24, realize_addl=TRUE))
})

test_that("sequence of event objects", {

  e1 <- ev(amt=1, ii=24, addl=3)
  e2 <- ev(amt=2, ii=24, addl=1)
  e3 <- ev(amt=3, ii=12, addl=4)
  e4 <- mutate(e2, amt = 4)

  e <- as.data.frame(ev_seq(e1,e2,e3))
  expect_equal(nrow(e), 3)
  expect_equal(e$time, c(0,96, 144))

  e <- ev_seq(e1, wait = 20, e2, wait= -10, e3)
  e <- as.data.frame(e)
  expect_equal(nrow(e), 3)
  expect_equal(e$time, c(0,116,154))
  expect_is(ev_seq(e2, e1, wait=2, e1),"ev")
  
  a <- ev_seq(e2, wait = 24, e4)
  b <- ev_seq(e2, ii   = 48, e4)
  expect_identical(a, b)

  # these are equivalent; result is sorted by time
  c <- ev_seq(e2, wait =  -72, e4)
  d <- ev_seq(e2, ii   =  -48, e4)
  expect_identical(c, d)
  expect_equal(d$amt, c(4, 2))
})

test_that("ev_seq requires event objects or spacer", {
  e1 <- ev(amt = 100)
  expect_error(
    ev_seq(e1, iii = 4, e1),
    regexp = "found object with class: numeric", 
    fixed = TRUE
  )
  expect_error(
    ev_seq(e1, as.data.frame(e1), e1),
    regexp = "please coerce to event object with `as.ev()`", 
    fixed = TRUE
  )
})

test_that(".ii is deprecated", {
  e1 <- ev(amt = 100, ii = 24, addl = 1)
  expect_warning(
    ev_seq(e1, .ii = 12, e1), 
    regexp = "has been renamed to", 
    fixed = TRUE
  )
})

test_that("replicate an event object", {
  e1 <- ev(amt=1, ii=24, addl=3)
  df <- ev_rep(e1, 11:14)
  expect_is(df, "data.frame")
  expect_equal(df$ID, 11:14)
})

test_that("events with without rate" , {
  e1 <- ev(amt=1, ii=12)
  e2 <- ev(amt=2, ii=24, rate=1)
  e <- ev_seq(e1,e2)
  expect_is(e, "ev")
  e <- as.data.frame(e)
  expect_equal(e$rate,c(0,1))
})

test_that("coerce to data frame", {
  e <- ev(amt = 100)

  ans <- as.data.frame(e)
  expect_is(ans, "data.frame")
  expect_false(mrgsolve:::has_ID(ans))

  ans <- as.data.frame(e, add_ID = 2)
  expect_is(ans, "data.frame")
  expect_true(mrgsolve:::has_ID(ans))
  expect_equal(ans$ID, 2)

  e <- ev(amt = 100, ID = 4)
  ans <- as.data.frame(e, add_ID = 2)
  expect_equal(ans$ID, 4)
})

test_that("get names", {
  e <- ev(amt = 100, ii = 12, addl = 24)
  expect_equal(
    names(e),
    c("time", "amt", "ii", "addl", "cmt", "evid")
  )
})

test_that("mutate an ev object", {
  e <- ev(amt = 100, cmt = 1)
  e2 <- mutate(e, cmt = 2)
  expect_is(e2, "ev")
  df <- as.data.frame(e2)
  expect_equal(df$cmt, 2)
})

test_that("filter an ev object", {
  e <- ev(amt = 100, cmt = 1, ii = 24, addl = 13)
  e <- realize_addl(e)
  e2 <- filter(e, time > 100) %>% as.data.frame()
  expect_true(all(e2[["time"]] > 100))
})

test_that("misc methods", {
  e <- ev(amt = 100)
  expect_true(mrgsolve:::is.ev(e))
  expect_false(mrgsolve:::is.ev(as.data.frame(e)))
})

test_that("as.ev", {
  df <- tibble(amt = 100, foo = 5)
  d <- as.data.frame(as.ev(df))
  expect_equal(d$cmt,1)
  expect_equal(d$amt,100)
  expect_equal(d$evid,1)
  expect_equal(d$time,0)
  expect_equal(d$foo,5)

  df <- tibble(amt = 200, evid = 1)
  obs <- mutate(df, evid = 0)
  df <- bind_rows(df,obs)
  d <- as.data.frame(as.ev(df))

  expect_equal(d$evid,1)

})

test_that("ev_repeat", {
  n <- 3
  e <- ev(amt = 100, ii = 24, addl = 9)
  e <- ev_repeat(e,n) %>% realize_addl()
  expect_equal(nrow(e),n*10) 
})

test_that("create ev with evaluation issue-512", {
  a <- ev(amt = 100, rate = amt/10) %>% as.data.frame()
  expect_true(exists("rate", a))
  expect_identical(a[["rate"]], 10)
  b <- ev(amt = 100, foo = amt/c(10,20,50)) %>% as.data.frame()
  expect_identical(b[["foo"]], 100/c(10,20,50))
  x <- 200
  c <- ev(amt = 100, foo = amt/x) %>% as.data.frame()
  expect_identical(c[["foo"]], 100/x)
})

test_that("tinf issue-513", {
  e <- ev(amt = 100, tinf = 10)
  expect_identical(e$rate, 10)
  e <- ev(amt = 100, tinf = 0)
  expect_identical(e$rate, 0)
  expect_error(ev(amt = 100, tinf = -1))
  e <- ev(amt = 100)
  e <- mutate(e, tinf = 20)
  expect_identical(e$rate,5)
  expect_error(ev(amt=100,tinf=2,rate=5), "input can include either")
  expect_error(mutate(e,rate=5), "cannot set rate when tinf")
  expect_silent(mutate(ev(amt=100,tinf=5),tinf=NULL,rate=2))
})

test_that("total  issue-513", {
  e <- ev(amt = 100, ii=2, total = 10)
  expect_identical(e$addl, 9)
  expect_error(ev(amt = 100, total = 0))
  e <- ev(amt = 100, total = 1)
  expect_identical(e$addl, NULL)
  e <- ev(amt = 100, ii = 12)
  e <- mutate(e, total = 20)
  expect_identical(e$addl,19)
  expect_error(ev(amt=100,addl=5,total=4), "input can include either")
  expect_silent(mutate(ev(amt = 100, total = 10),total=NULL,addl=4))
})

test_that("until  issue-513", {
  e <- ev(amt = 100, ii=24, until = 168)
  expect_identical(e$addl, 6)
  expect_error(ev(amt = 100, until = 24))
  e <- ev(amt = 100)
  e <- mutate(e, ii = 24, until = 168)
  expect_identical(e$addl,6)
  expect_error(ev(amt=100,addl=5,until=100), "input can include either")
  expect_silent(mutate(ev(amt=100,ii=2,until=168),until=NULL,addl=5))
})
