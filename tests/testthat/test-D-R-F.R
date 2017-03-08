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

project <- file.path(system.file(package="mrgsolve"), "models")

Sys.sleep(1)

context("Testing infusion inputs")
# 
# code <-'
# 
# $PARAM CL=1, VC=30, KA=1.2, SET=7
# $CMT GUT CENT
# $MAIN  R_GUT =  SET;
# $OMEGA 0
# $ODE
# dxdt_GUT = -KA*GUT;
# dxdt_CENT = KA*GUT - (CL/VC)*CENT;
# '
# 
# mod <- 
#   mcode("testinfusion",code) %>%
#   update(end=72) %>% obsonly
# 
# 
# data <- expand.ev(ID=1:5,rate=c(1,5,10,50), amt=100)
# set.seed(1212)
# out0 <- mod %>% data_set(data) %>% mrgsim
# 
# 
# data <- data %>% mutate(SET = rate, rate=-1)
# set.seed(1212)
# out <- mod %>% data_set(data) %>% mrgsim
# 
# 
# test_that("Infusion rate is set by R_CMT", {
#     expect_identical(out0$CENT, out$CENT)
# })
# 
# data$rate <- -2
# test_that("Error when rate = -2 and R_CMT set instead of D_CMT", {
#     expect_error(mod %>% data_set(data) %>% mrgsim)
# })
# 
# 
# 
# code <-'
# 
# $PARAM CL=1, VC=30, KA=1.2, SET=0
# $CMT GUT CENT
# $MAIN  D_GUT =  SET;
# $OMEGA 0
# $ODE
# dxdt_GUT = -KA*GUT;
# dxdt_CENT = KA*GUT - (CL/VC)*CENT;
# '
# 
# 
# mod <- mcode("testDGUT",code) %>%
#   update(end=120) %>% obsonly
# 
# 
# data0 <- expand.ev(ID=1:3,dur=c(2,5,10,50), amt=100) %>%
#   mutate(rate = amt/dur)
# 
# set.seed(1212)
# out0 <- mod %>% data_set(data0) %>% mrgsim
# 
# data <- data0 %>% mutate(SET = dur, rate=-2)
# set.seed(1212)
# out <- mod %>% data_set(data) %>% mrgsim
# 
# test_that("Infusion rate is set by D_CMT", {
#   expect_identical(out0$CENT, out$CENT)
# })
# 
# data$rate <- -1
# test_that("Error when rate = -1 and D_CMT set instead of R_CMT", {
#   expect_error(mod %>% data_set(data) %>% mrgsim)
# })
# 
# 
# code <- '
# $PARAM D1 = 2, F1=1
# $MAIN D_CENT = D1; F_CENT = F1;
# $CMT CENT
# $ODE dxdt_CENT = -0.1*CENT;
# '
# mod <- mcode("test11DFbsz",code)
# 
# out <-  mod %>% ev(amt=1000,  rate=-2) %>% mrgsim
# out2 <- mod %>% ev(amt=1000, rate=-2, F1=0.5) %>% mrgsim
# out3 <- mod %>% ev(amt=1000, rate=-2, D1 = 10) %>% mrgsim
# out4 <- mod %>% ev(amt=1000, rate=-2, D1 = 10,F1=1.5) %>% mrgsim
# 
# test_that("Correct infusion duration when D_CMT and F_CMT are set", {
#   expect_true(out$time[which.max(out$CENT)] ==2)
#   expect_true(out2$time[which.max(out2$CENT)]==2)
#   expect_true(round(max(out$CENT)/max(out2$CENT),3) == 2)
#   expect_true(out3$time[which.max(out3$CENT)]==10)
#   expect_true(out4$time[which.max(out4$CENT)]==10)
#   expect_true(round(max(out3$CENT)/max(out4$CENT),3) == 0.667)
# })
# 
# 
# 
# code <- '
# $PARAM R1 = 100, F1=1
# $MAIN R_CENT = R1; F_CENT = F1;
# $CMT CENT
# $ODE dxdt_CENT = -0.1*CENT;
# '
# 
# mod <- mcode("test11RF",code) %>% update(delta=0.1)
# 
# out <- mod %>% ev(amt=1000,  rate=-1) %>% mrgsim
# out2 <- mod %>% ev(amt=1000, rate=-1, F1=0.5) %>% mrgsim
# out3 <- mod %>% ev(amt=1000, rate=-1, R1 = 50) %>% mrgsim
# out4 <- mod %>% ev(amt=1000, rate=-1, R1 = 200, F1=1.5) %>% mrgsim
# 
# test_that("Correct infusion duration when R_CMT and F_CMT are set", {
#   expect_true(out$time[which.max(out$CENT)] ==10)
#   expect_true(out2$time[which.max(out2$CENT)]==5)
#   expect_true(round(max(out$CENT)/max(out2$CENT),3) > 1)
#   expect_true(out3$time[which.max(out3$CENT)]==20)
#   expect_true(out4$time[which.max(out4$CENT)]==7.5)
#   expect_true(round(max(out3$CENT)/max(out4$CENT),3) < 1)
# })
# 
# 
# 
# 
# 
