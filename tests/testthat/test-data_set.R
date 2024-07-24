# Copyright (C) 2013 - 2022  Metrum Research Group
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

mod <- mrgsolve::house() %>% update(end=240)

context("test-data_set")

data(extran1)
lo <- extran1
up <- extran1
names(up) <- toupper(names(up))

test_that("Same result from upper and lower case names", {
  what <- c(1,5:7)
  a <- mod %>% data_set(up) %>% mrgsim
  b <- mod %>% data_set(lo) %>% mrgsim
  expect_identical(as.matrix(a)[,what],as.matrix(b)[,what])
  expect_identical(a$TIME,b$time)
  expect_is(plot(a),"trellis")
  expect_is(plot(b),"trellis")
  
  expect_is(plot(a, CP~.),"trellis")
  expect_is(plot(a, CP~TIME),"trellis")
  expect_is(plot(b, CP~.),"trellis")
  expect_is(plot(b, CP~time),"trellis")
  expect_error(plot(b, CP~TIME))
  
  ldd <- expand.ev(amt=100, ii=24, addl=2, ss=1)
  udd <- ldd
  names(udd) <- toupper(names(udd))
  a <- mod %>% data_set(ldd) %>% mrgsim
  b <- mod %>% data_set(udd) %>% mrgsim
  expect_identical(as.matrix(a)[,what],as.matrix(b)[,what])
})

test_that("Warning is generated when mixed upper/lower names", {
  mix <-  dplyr::rename(lo, EVID = evid) 
  expect_warning(mrgsim(data_set(mod,mix)))
})

test_that("Include TIME and time when checking for mixed upper/lower case", {
  expect_warning(
    mrgsim(house(), data = data.frame(ID = 1, TIME = 0, amt = 0, cmt = 1)), 
    "Both lower- & upper-case", 
    fixed = TRUE
  )
  expect_warning(
    mrgsim(house(), data = data.frame(ID = 1, time = 0, AMT = 0, CMT = 1)), 
    "Both lower- & upper-case", 
    fixed = TRUE
  )
})

test_that("Filter out ID", {
  out <- mod %>% data_set(up, ID > 4) %>% mrgsim
  expect_true(all(out$ID > 4))
})

test_that("ID is required", {
  df <- expand.ev(amt=100,ii=12,addl=2) %>% dplyr::select(-ID)
  expect_error(mod %>% data_set(df) %>% mrgsim)
})

test_that("cmt is required", {
  df <- expand.ev(amt=100,ii=12,addl=2) %>% dplyr::select(-cmt)
  expect_error(mod %>% data_set(df) %>% mrgsim)
})

test_that("time is required", {
  df <- expand.ev(amt=100,ii=12,addl=2) %>% dplyr::select(-time)
  expect_error(mod %>% data_set(df) %>% mrgsim)
})

mod <- mrgsolve::house()

data(exTheoph)

dd1 <- exTheoph %>% bind_rows(., group_by(.,ID) %>% slice(5)) %>% arrange(ID) 
dd2 <- dd1 %>% arrange(ID,time)


test_that("Improperly sorted records produces error", {
  expect_error(mod %>% data_set(dd1) %>% mrgsim)
})

test_that("Properly sorted records produces no error", {
  expect_is((mod %>% data_set(dd2) %>% mrgsim),"mrgsims")
})

test_that("Data set column order gives same answer", {
  mod <- mrgsolve::house() %>% omat(dmat(1,1,1,1))
  data(extran3)
  set.seed(9923403)
  extran3 <- extran3 %>% mutate(a=-10, Z = -11, M = -22, h = -33)
  extran3b <- extran3[,sample(seq_along(names(extran3)))]
  
  set.seed(22930)
  out1 <- mod %>% carry_out(a,m,M,h) %>% data_set(extran3) %>% mrgsim
  set.seed(22930)
  out2 <- mod %>% carry_out(a,m,M,h) %>% data_set(extran3b) %>% mrgsim
  expect_identical(as.matrix(out1),as.matrix(out2))
  expect_false(all(names(extran3)==names(extran3b)))
  rm(mod)
})

test_that("numerics_only", {
  n <- 10
  data <- tibble(
    ID = as.numeric(seq(n)), 
    EYES = "black", 
    DATETM = as.POSIXct("2018-01-01 08:00:00"), 
    DATE = as.Date(DATETM),
    INT = as.integer(ID),
    BOOL = INT > 6
  )
  df <- numerics_only(data, convert_lgl=TRUE)
  expect_equal(names(df), c("ID", "INT", "BOOL"))
  expect_true(all(mrgsolve:::numeric_columns(df)))
  expect_message(numerics_only(data))
  df <- numerics_only(data,convert_lgl = FALSE)
  expect_equal(names(df), c("ID", "INT"))
  expect_true(all(mrgsolve:::numeric_columns(df)))
  expect_silent(numerics_only(data,quiet=TRUE))
})

test_that("missing value in param column is message", {
  data <- expand.ev(amt = 100, ii = 24, addl = 2, WT = NA_real_, FOO = 2)
  expect_warning(valid_data_set(data,mod), 
                 regexp="Parameter column WT must not contain missing values.")
  
  idata <- dplyr::select(data, ID,WT)
  expect_warning(valid_idata_set(idata,mod), 
                 regexp="Parameter column WT must not contain missing values.")
})

test_that("missing value in time/rate/ID is error", {
  data <- data.frame(ID = 1, time = NA_real_, amt = 100, cmt = 1, evid=1)
  expect_error(mrgsim_d(mod,data), 
               regexp="Found missing values in input data.")
  
  data <- data.frame(ID = NA_real_, rate = 100, amt = 100, cmt = 1, evid=1,time=0)
  expect_error(mrgsim_d(mod,data), 
               regexp="Found missing values in input data.")
})

test_that("observations expand", {
  e <- ev(amt = 100)
  dat <- expand_observations(e, c(1,2,3))
  dose <- filter(dat,evid==1)
  expect_equal(nrow(dose),1)
  obs <- filter(dat, evid==0)
  expect_equal(nrow(obs),3)
  expect_equal(obs[["time"]],c(1,2,3))
})

test_that("expand observations bug issue-563", {
  e1 <- ev(amt = 100, cmt=1,LAGT=1,ID=15)
  e2 <- ev(time=4,amt=50,cmt=1,LAGT=0,rate=24,addl=2,ii=24,ID=15)
  e <- c(e1,e2)
  expect_silent(d <- expand_observations(e,seq(1,10)))
  expect_is(d,"data.frame")
})

test_that("add position argument to expand observations issue-565", {
  e1 <- ev(amt = 100, ii = 4, addl = 1) %>% realize_addl
  dat1 <- expand_observations(e1,c(0,4))
  dat2 <- expand_observations(e1,c(0,4),obs_pos=13)
  expect_equal(dat1$amt, c(0,100,0,100))
  expect_equal(dat2$amt, c(100,0,100,0))
  expect_equal(dat1$time,c(0,0,4,4))
  expect_equal(dat1$time,dat2$time)
})

test_that("Convert names to lower case with lctran [SLV-TEST-0004]", {
  data <- data.frame(time = 1, EVID = 2, ss = 2, foo = 5, BAR = 2)
  ans <- lctran(data)
  expect_equal(
    names(ans), 
    c("time", "evid", "ss", "foo", "BAR")
  )
  data <- data.frame(time = 1, EVID = 2, ss = 2, TIME = 5, BAR = 2)
  expect_warning(
    lctran(data), 
    regexp = "There are both upper and lower case"
  )
})

test_that("Convert names to upper case with uctran  [SLV-TEST-0005]", {
  data <- data.frame(time = 1, EVID = 2, ss = 2, foo = 5, BAR = 2)
  ans <- uctran(data)
  expect_equal(
    names(ans), 
    c("TIME", "EVID", "SS", "foo", "BAR")
  )
  data <- data.frame(time = 1, EVID = 2, ss = 2, TIME = 5, BAR = 2)
  expect_warning(
    uctran(data), 
    regexp = "There are both upper and lower case"
  )
})

test_that("Convert event to upper or lower case  [SLV-TEST-0006]", {
  x <- uctran(ev(amt = 100, ii = 5))
  expect_equal(x@case, 1L)
  expect_is(x, "ev")
  x <- lctran(ev(amt = 100, ii = 5))
  expect_equal(x@case, 0L)
  expect_is(x, "ev")
})
