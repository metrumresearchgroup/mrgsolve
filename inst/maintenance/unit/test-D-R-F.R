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


context("test-D-R-F")

code <- '
$PARAM D1 = 2, F1=1, LAGT = 0, R1 = 1, mode = 0
$MAIN 
ALAG_CENT = LAGT;
F_CENT = F1;

if(mode==1) R_CENT = R1;
if(mode==2) D_CENT = D1;

$CMT CENT GUT
$ODE dxdt_CENT = GUT-0.1*CENT; dxdt_GUT = -1*GUT;
'

mod <-
  mcode("testinfusion",code) %>%
  update(end=72) %>% obsonly


data <- expand.ev(ID=1:2, rate=c(1,5,10,50), amt=100)
set.seed(1212)
out0 <- mod %>% data_set(data) %>% mrgsim


data <- data %>% mutate(R1 = rate, rate=-1, mode = 1)
set.seed(1212)
out <- mod %>% data_set(data) %>% mrgsim

test_that("Infusion rate is set by R_CMT", {
    expect_identical(out0$CENT, out$CENT)
})

data$rate <- -2
test_that("Error when rate = -2 and R_CMT set instead of D_CMT", {
    expect_error(mod %>% data_set(data) %>% mrgsim)
})

data0 <- expand.ev(ID=1:3,dur=c(2,5,10,50), amt=100) %>%
  mutate(rate = amt/dur, mode = 1)

set.seed(1212)
out0 <- mod %>% data_set(data0) %>% mrgsim

data <- data0 %>% mutate(D1 = dur, rate=-2, mode = 2)
set.seed(1212)
out <- mod %>% data_set(data) %>% mrgsim

test_that("Infusion rate is set by D_CMT", {
  expect_identical(out0$CENT, out$CENT)
})

data$rate <- -1
test_that("Error when rate = -1 and D_CMT set instead of R_CMT", {
  expect_error(mod %>% data_set(data) %>% mrgsim)
})


out <-  mod %>% ev(amt=1000,  rate=-2, mode = 2) %>% mrgsim
out2 <- mod %>% ev(amt=1000, rate=-2, F1=0.5, mode = 2) %>% mrgsim
out3 <- mod %>% ev(amt=1000, rate=-2, D1 = 10, mode = 2) %>% mrgsim
out4 <- mod %>% ev(amt=1000, rate=-2, D1 = 10,F1=1.5, mode = 2) %>% mrgsim

test_that("Infusion duration when D_CMT and F_CMT are set", {
  expect_true(out$time[which.max(out$CENT)] ==2)
  expect_true(out2$time[which.max(out2$CENT)]==2)
  expect_true(round(max(out$CENT)/max(out2$CENT),3) == 2)
  expect_true(out3$time[which.max(out3$CENT)]==10)
  expect_true(out4$time[which.max(out4$CENT)]==10)
  expect_true(round(max(out3$CENT)/max(out4$CENT),3) == 0.667)
})


## Issue 267
test_that("Infusion duration (D_) with lag", {
  mod <- param(mod, mode = 2)
  out <- 
    mod %>% 
    ev(amt=1000, rate=-2) %>%
    mrgsim(end = 24)
  
  tmax <- out$time[which.max(out$CENT)]
  expect_equal(tmax,2)
  
  out <- 
    mod %>% 
    param(LAGT = 5) %>%
    ev(amt=1000, rate=-2) %>%
    mrgsim(end = 24)
  
  tmax <- out$time[which.max(out$CENT)]
  expect_equal(tmax,7)
  
  out <- 
    mod %>% 
    param(LAGT = 12, D1 = 10) %>%
    ev(amt=1000, rate=-2) %>%
    mrgsim(end = 24)
  
  tmax <- out$time[which.max(out$CENT)]
  expect_equal(tmax,22)
  
})

## Issue 267
test_that("Infusion duration (D_) with lag, multiple", {
  mod <- param(mod, mode = 2)
  out <- 
    mod %>% 
    param(LAGT = 4, D1 = 10) %>%
    ev(amt=1000, rate = -2, ii=24, addl=3) %>% 
    mrgsim(end=97, obsonly = TRUE) 
  
  # To simplify, correct for LAGT and then analyze
  out <- mutate(out, time = time - 4, DAY = 1+floor(time/24))
  out <- filter(out, DAY >= 1)
  sum <- group_by(out, DAY) 
  sum <- summarise(sum, tmax = time[which.max(CENT)], 
                   tmin = time[which.min(CENT)])
  
  expect_equal(sum$tmax[1], 10)
  expect_equal(sum$tmax, 10 + c(0, 24, 48, 72))
  expect_equal(sum$tmin, seq(0,72,24))
  
})

test_that("Infusion duration (D_) with lag and F", {
  mod <- param(mod, LAGT = 5, D1 = 10, F1 = 0.5, mode = 2)
  out <- 
    mod %>% 
    ev(amt=1000, rate = -2, ii=24, addl=3) %>% 
    mrgsim(end=97, obsonly = TRUE) 
  
  # To simplify, correct for LAGT and then analyze
  out <- mutate(out, time = time - mod$LAGT, DAY = 1+floor(time/24))
  out <- filter(out, DAY >= 1)
  sum <- group_by(out, DAY) 
  sum <- summarise(sum, tmax = time[which.max(CENT)], 
                   tmin = time[which.min(CENT)])
  
  expect_equal(sum$tmax[1], 10)
  expect_equal(sum$tmax, 10 + c(0, 24, 48, 72))
  expect_equal(sum$tmin, seq(0,72,24))
  
})

mod <- update(mod, delta=0.1)
out <-  mod %>% ev(amt=1000, rate = -1, R1 = 100, mode = 1) %>% mrgsim
out2 <- mod %>% ev(amt=1000, rate = -1, R1 = 100, F1 = 0.5, mode = 1) %>% mrgsim
out3 <- mod %>% ev(amt=1000, rate = -1, R1 = 50, mode = 1) %>% mrgsim
out4 <- mod %>% ev(amt=1000, rate = -1, R1 = 200, F1 = 1.5, mode = 1) %>% mrgsim

test_that("Infusion duration when R_CMT and F_CMT are set", {
  expect_true(out$time[which.max(out$CENT)] == 10)
  expect_true(out2$time[which.max(out2$CENT)]==5)
  expect_true(round(max(out$CENT)/max(out2$CENT),3) > 1)
  expect_true(out3$time[which.max(out3$CENT)]==20)
  expect_true(out4$time[which.max(out4$CENT)]==7.5)
  expect_true(round(max(out3$CENT)/max(out4$CENT),3) < 1)
})

## Issue 267
test_that("Infusion duration (R_) lag time, multiple", {
  mod <- param(mod, mode = 1)
  out <- 
    mod %>% 
    ev(amt=1000, rate = -1, ii=24, addl=3) %>% 
    param(LAGT = 4, R1 = 1000/10) %>%
    mrgsim(end=97, obsonly = TRUE) 
  
  # To simplify, correct for LAGT and then analyze
  out <- mutate(out, time = time - 4, DAY = 1+floor(time/24))
  out <- filter(out, DAY >= 1)
  sum <- group_by(out, DAY) 
  sum <- summarise(sum, tmax = time[which.max(CENT)], 
                   tmin = time[which.min(CENT)])
  
  expect_equal(sum$tmax[1], 10)
  expect_equal(sum$tmax, 10 + c(0, 24, 48, 72))
  expect_equal(sum$tmin, seq(0,72,24))
  
})

test_that("Infusion duration (R_) with lag time and F", {
  mod <- param(mod, LAGT = 5, R1 = 1000/10, F1 = 0.5, mode = 1)
  out <- 
    mod %>% 
    ev(amt=1000, rate = -1, ii=24, addl=3) %>% 
    mrgsim(end=97, obsonly = TRUE) 
  
  # To simplify, correct for LAGT and then analyze
  out <- mutate(out, time = time - mod$LAGT, DAY = 1+floor(time/24))
  out <- filter(out, DAY >= 1)
  sum <- group_by(out, DAY) 
  sum <- summarise(sum, tmax = time[which.max(CENT)], 
                   tmin = time[which.min(CENT)])
  
  expect_equal(sum$tmax[1],  unname(10*mod$F1))
  expect_equal(sum$tmax, 5 + c(0, 24, 48, 72))
  expect_equal(sum$tmin, seq(0,72,24))
  
})

# Issue #991
test_that("tad is correctly calculated for addl doses with lag", {
  # the start time is after the first dose record 
  # so all dosing records are additional doses
  mod <- param(mod, LAGT = 1)
  dose <- ev(amt = 100, ii = 10, addl = 10,  time = 160) 
  doses <- seq(200, 240, 10) 
  
  # With recsort = 1
  out <- mrgsim(
    mod, dose, 
    delta = 1, recsort = 1, 
    tad = TRUE, 
    start = 200,  end = 224, add = 199.999, 
    output = "df"
  )
  # there was a dose at 160, 170, ... [200, 210, 220, 230, 240]
  # and observation records every 2 hours 
  dose_rec <- which(out$time %in% doses)
  expect_true(all(out$tad[dose_rec]==10))
  expect_true(all(out$tad[dose_rec+1]==1))
  expect_true(all(out$tad[dose_rec+3]==3))
  
  # The most recent dose when we started was  at 190
  # The first observation time was at 199.999
  expect_true(abs(out$tad[1] - 9.999) < 1e-10)

  # With recsort = 4
  out <- mrgsim(
    mod, dose, 
    delta = 1, recsort = 4, 
    tad = TRUE, 
    start = 200,  end = 224, add = 199.999, 
    output = "df"
  )
  # there was a dose at 160, 170, ... [200, 210, 220, 230, 240]
  # and observation records every 2 hours 
  dose_rec <- which(out$time %in% doses)
  expect_true(all(out$tad[dose_rec]==0))
  
})

test_that("Detect dosing compartments", {
  cmts <- c("GUT", "CENT", "ABS_RAPID_FORM2")
  code <- c("CL = THETA(1);", "F_ABS_RAPID_FORM2 = 1.2;", "S2 = V2;")
  ans <- mrgsolve:::dosing_cmts(code, cmts)
  expect_identical(ans, cmts[3])
  
  code <- '
  $CMT ABS_RAPID_FORM2 CENT
  $PK F_ABS_RAPID_FORM2 = 0.2;
  '
  mod <- mcode("test-detect-dosing", code, compile = FALSE)
  f <- list.files(
    dirname(mod@shlib$source), 
    pattern = "header", 
    full.names = TRUE
  )
  txt <- readLines(f)
  expect_match(txt, "define F_ABS_RAPID_FORM2", all = FALSE)
})
