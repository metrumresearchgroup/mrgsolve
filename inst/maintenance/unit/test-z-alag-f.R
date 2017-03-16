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

lim <- function(x,...) x %>% dplyr::filter(...) %>% as.data.frame


context("Set F via F_CMT")

code <- '
$PARAM F1=1, ALAG1=0, F2=1, ALAG2=0

$CMT CENT DEPOT

$MAIN
ALAG_CENT = ALAG1;
F_CENT = F1;

ALAG_DEPOT = ALAG2;
F_DEPOT = F2;

'

ev1 <- ev(amt=100, cmt=1)
ev2 <- ev(amt=100, cmt=2,time=1)

mod <- 
  mcode("f_alag_model",code,warn=FALSE) %>% 
  carry_out(evid) %>% 
  update(end=2)

mod1 <- mod %>% ev(ev1)
mod2 <- mod %>% ev(ev2)

out10 <- mod1 %>% mrgsim(recsort=1)
out11 <- mod1 %>% param(F1 = 0.2) %>% mrgsim(recsort=1) 
out12 <- mod1 %>% param(F1 = 0.8) %>% mrgsim(recsort=1)
out13 <- mod1 %>% param(ALAG1 = 1) %>% mrgsim(recsort=1)
out14 <- mod1 %>% param(ALAG1 = 0.5) %>% mrgsim(recsort=1, add=c(0.49999,0.5))

out20 <- mod2 %>% mrgsim(recsort=1) 
out21 <- mod2 %>% param(F2 = 0.3) %>% mrgsim(recsort=1)
out22 <- mod2 %>% param(F2 = 0.1) %>% mrgsim(recsort=1)
out23 <- mod2 %>% param(ALAG2 = 0.3) %>% mrgsim(recsort=1,add=1.3)
out24 <- mod2 %>% param(ALAG2 = 2) %>% mrgsim(end=5,recsort=1)


test_that("F is set for compartment 1 and 2", {
  expect_true(lim(out10,time==2)$CENT==100)
  expect_true(lim(out11,time==2)$CENT==20)
  expect_true(lim(out12,time==2)$CENT==80)
  
  expect_true(lim(out20,time==2)$DEPOT==100)
  expect_true(lim(out21,time==2)$DEPOT==30)
  expect_true(lim(out22,time==2)$DEPOT==10)
})


context("Set ALAG via ALAG_CMT")
test_that("ALAG is set for compartment 1 and 2", {
  
  expect_true(lim(out10, CENT>0)$time[1]==0)
  expect_true(lim(out13, CENT>0)$time[1]==1)
  expect_true(lim(out14, CENT>0)$time[1]==0.5)
  
  expect_true(lim(out20, DEPOT>0)$time[1]==1)
  expect_true(lim(out23, DEPOT>0)$time[1]==1.3)
  expect_true(lim(out24, DEPOT>0)$time[1]==3)
})



test_that("F is set for multiple doses", {
  out1 <- 
    mod1 %>% ev(amt=100, cmt=1, addl=3, ii=1) %>% 
    param(F1 = 1) %>% 
    mrgsim(end=3,recsort=2)
  
  out2 <- 
    mod1 %>% ev(amt=100, cmt=1, addl=3, ii=1) %>% 
    param(F1 = 0.2) %>% 
    mrgsim(end=3,recsort=2)
  
  expect_equivalent(lim(out1, time > 0)$CENT, c(100,200,300))
  expect_equivalent(lim(out2, time > 0)$CENT, c(20,40,60))
})



test_that("F and ALAG are set from idata", {
  idata <- mrgsolve:::expand.idata(ID=1:3, F1=c(0.2, 0.5), ALAG1=c(0.2, 0.5,0.7,0.99))
  out1 <- mod1 %>% ev(amt=100, cmt=1, time=1) %>% idata_set(idata) %>% mrgsim()
  out2 <- mod1 %>% ev(amt=100, cmt=1, time=1) %>% idata_set(idata) %>% mrgsim(add=1+idata$ALAG1,recsort=1)
  out2 <- out2 %>% lim(CENT > 0) %>% as.tbl %>% group_by(ID)%>% slice(1)
  
  expect_equivalent(lim(out1, time==2)$CENT, 100*idata$F1)
  expect_equivalent(out2$time, 1+idata$ALAG1)
})

data(exTheoph)
exTheoph$FORM <- as.integer(exTheoph$ID >5)
exTheoph$F1 <- mrgsolve:::mapvalues(exTheoph$FORM, c(0,1), c(0.8, 0.3))
exTheoph <- exTheoph %>% group_by(ID) %>% mutate(ALAG1 = round(runif(1,1,3),3))
doses <- subset(exTheoph, evid==1)


test_that("F  is set from data", {
  out1 <- mod %>% data_set(exTheoph) %>% mrgsim() 
  expect_equivalent(lim(out1, !duplicated(ID, fromLast=TRUE))$CENT, doses$amt*doses$F1)
})


test_that("ALAG is set from data", {
  
  out2 <- 
    mod %>% 
    data_set(exTheoph) %>% 
    mrgsim(recsort=1,add=c(doses$ALAG1),obsaug=TRUE) 
  
  out2 <- 
    out2 %>% 
    dplyr::filter(CENT > 0) %>% 
    group_by(ID) %>% slice(1)
  
  expect_equivalent(out2$time, doses$ALAG1)
  
})

