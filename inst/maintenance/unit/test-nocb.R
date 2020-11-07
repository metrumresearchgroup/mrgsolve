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

context("test-nocb")

code <- '
$PARAM ke = 0
$ODE
dxdt_foo = -ke*foo;

$CAPTURE ke
$CMT foo
'

e1 <- ev(amt = 0, evid = 2, ke = 0E-12)
e2 <- ev(amt = 100, ke = 0E-12)
e3 <- ev(amt = 0, evid = 1, ke = 1, time = 20)
e4 <- as.ev(data.frame(amt = 0, evid = 2, ke = 1, time = c(21,22,23), cmt = 1))
e <- as.data.frame(c(e1,e2, e3, e4)) %>% mutate(ID = 1)

mod <- mcode("mypk",code)

test_that("simulation with nocb", {
  out <- mod %>% mrgsim(data = e, nocb = TRUE, end = -1)
  expect_true(out$foo[3] < 1)
  expect_true(out$foo[2] == 100)
})

test_that("simulation with locf", {
  out <- mod %>% mrgsim(data = e, nocb = FALSE, end = -1)
  expect_true(out$foo[3] == 100)
  expect_true(out$foo[4] < 50)
})

test_that("correct update with infusion #741", {
  a <- ev(amt = 100)
  b <- ev(amt = 100, cmt = 2, tinf = 3)
  data1 <- as.data.frame(c(a,b), add_ID = 1)
  data1$KA <- 0.02
  data2 <- data1
  data2$KA <- 0.04
  data2$time <- 24
  data3 <- slice(data2, 2)
  data3$time <- 49
  data3$amt <- data3$rate <- data3$evid <- 0
  data <- rbind(data1,data2,data3)
  bol <- filter(data, cmt==1 | evid==0)
  mod <- house()
  out1 <- mrgsim(mod, data)
  out2 <- mrgsim(mod, bol)
  expect_equal(unique(out1$GUT), out2$GUT)
})

test_that("test-nocb: time-varying covariates #741", {
  code <- '
$PARAM TVKA = 0.05, CL = 1, VC = 10, COV = 100
$TABLE
capture DV  = CENTRAL / VC;
$MAIN
double V = VC;
capture KA = TVKA * (COV / 100) ;
ALAG_DEPOT = 0.52340;
$PKMODEL cmt = "DEPOT CENTRAL", depot = TRUE
'
  
  mod <- mcode("foo2934934", code)
  
  id1 <- tibble::tribble(
    ~ID, ~TIME, ~EVID, ~CMT, ~AMT, ~RATE,
    1,  0, 1, 1, 80, 8,
    1, 24, 1, 2, 60, 0, 
    1, 48, 1, 2, 40, 8,
    1, 72, 1, 1, 60, 0
  )
  id2 <- dplyr::mutate(id1, ID = 2)
  data <- dplyr::bind_rows(id1,id2)
  data2 <- expand_observations(data, c(12,22.2,36.234,42.5,55,60.001))
  data2 <- dplyr::mutate(data2, COV = ifelse(TIME <= 48, 80, 60))
  data2 <- dplyr::mutate(data2, COV = ifelse(TIME <= 24, 40, COV))
  data1 <- dplyr::filter(data2, EVID==1)
  set.seed(11010)
  x <- mrgsim(mod,data2) 
  set.seed(11010)
  b <- mrgsim(mod,data1,end=-1)
  a <- filter_sims(x, TIME %in% b$TIME)
  expect_true(all.equal(a,b))
})

test_that("loc:unit carry_out with obsaug", {
  code <- "
$PARAM CL0 = .1, V = 10, COV = 100
$CMT CENT
$PKMODEL ncmt = 1
$TABLE
double DV  = CENT / V  ;
$MAIN
double CL = CL0 / V * COV ;
$CAPTURE DV, CL, COV2 = COV
"
  modelobsaug <- mcode("testobsaug", code)
  
  dat1 <- data.frame(
    ID = 1, 
    time = c(0, 12.1, 24),
    evid = c(1, 0, 0),
    amt = c(100, 0, 0), 
    cmt = 1, 
    COV = c(100, 100, 50)
  )
  
  dat2 <- mutate(dat1, ID = 2)
  data <- bind_rows(dat1,dat2)
  
  out <- 
    modelobsaug %>%
    obsaug() %>% 
    data_set(data) %>%
    mrgsim_df(carry_out = "COV") 
  
  expect_true(all(out$COV==out$COV2))
  
  out <- 
    modelobsaug %>%
    obsaug() %>% 
    data_set(data) %>%
    mrgsim_df(carry_out = "COV", nocb = FALSE)
  
  expect_true(all(out$COV==out$COV2))
  
})


