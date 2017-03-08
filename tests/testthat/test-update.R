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
$SET request=""
$CMT CM
$PARAM A = 0, B = 0, C = 0
$CAPTURE A B C
' 

mod1 <- mcode("code1eekd", code1, warn=FALSE)

context("test-update")


test_that("Update parameter - via param", {
  expect_equal(param(param(mod1,B = 2))$B,2)
  expect_equal(as.list(param(param(mod1,A=11,C=22))),list(A=11,B=0,C=22))

  out <- mod1 %>% mrgsim(end=8)
  expect_true(all(c(out$A==0,out$B==0,out$C==0)))
  
  out <- mod1 %>% param(A=3, B=2, C=1) %>% mrgsim
  expect_true(all(c(out$A==3,out$B==2,out$C==1)))
  
  expect_error(mod1 %>% param(FAKE = 5))
  expect_error(mod1 %>% param(list(FAKE=3),strict=TRUE))
  #expect_warning(mod1 %>% param(list(FAKE=3)))
  expect_is(mod1 %>% param(list(FAKE=3,B=2)), "mrgmod")
})



test_that("Update parameter - via idata", {
  idata <- expand.idata(ID=1, A=c(4,5,6),B=c(7,8,9),C=c(11,12,13,14))  
  out <- mod1 %>% idata_set(idata) %>% mrgsim %>% as.tbl %>% distinct(ID,A,B,C)
  expect_equal(unlist(out),unlist(idata))
  expect_identical(param(param(mod1,B = 2))$B,2)
})


test_that("Update parameter - via data, not-time-varying", {
  data <- expand.ev(ID=1, A=c(4,5,6),B=c(7,8,9),C=c(11,12,13,14),amt=2)
  out <- mod1 %>% data_set(data) %>% carry_out(amt,evid,cmt,time) %>%
    mrgsim() %>% as.tbl %>% filter(evid==1) %>% mutate(CM=NULL)
  expect_equal(unlist(out),unlist(data[,names(out)]))
  
})

test_that("Update parameter - via data, time-varying", {
  ## data with time-varying covariate
  data <- 
    bind_rows(
      data_frame(ID=1, time=seq(0,10,1), A = 2*time, B = 1.1*time),
      data_frame(ID=2, time=seq(0,15,1), A = 22*time, B = 11.1*time)
    ) %>% mutate(evid=ifelse(time==1,1,0),cmt=1)
    
  out <- mod1 %>% data_set(data)  %>% mrgsim()
  
  expect_true(all(c(data$A==out$A,data$B==out$B)))
  
})


## Make some updates to parameters and simulation times
project <- file.path(system.file(package="mrgsolve"), "models")

mod <- mread("pk1cmt", modlib(),atol=1E-20, rtol=1E-12, digits=8)
mod1 <- update(mod %>% param(CL=12, VC=220), delta=33, end=222)
mod2 <- update(mod, param=list(CL=12, VC=220), delta=33, end=222)
x <- sort(runif(100, 0,300))
mod3 <- update(mod, add=x, end=1000, delta=20)
mod4 <- update(mod, init=list(CENT=111))
mod5 <- mod %>% param(VC=999)
mod6 <- mod %>% init(EV1=5566)
mod7 <- update(mod, hmin=111, hmax=222, maxsteps=333, ixpr=444, mxhnil=555, atol=1E-99, rtol=1E-88)
mod8 <- mrgsolve:::mod(mrgsim(mod, delta=33, end=222))
mod9 <- update(mod, delta=33, end=222)
mod10 <- mrgsolve:::mod(mrgsim(mod, param=list(CL=12, VC=220)))
mod11 <- mrgsolve:::mod(mrgsim(mod  %>% param(CL=12, VC=220)))



context("Updates: general and simulation times")

test_that("Model object updates through update and %>% operator", {
  expect_true(identical(mod1, mod2))
  expect_true(!identical(mod1, mod))
})

test_that("Simulation times update properly via update",{
  expect_equal(stime(mod1), seq(0,mod1@end, mod2@delta))
  expect_equal(stime(mod3), sort(unique(c(x,seq(0,mod3@end, mod3@delta)))))
})
test_that("Simulation times update properly when passed into mrgsim",{
  expect_identical(mod8,mod9)
})


context("Test updates: parameters and initials")


test_that("Parameters update properly via %>% operator",{
  expect_equal(param(mod1)$CL, 12)
})


test_that("Parameters update properly via update()",{
  expect_equal(param(mod2)$VC, 220)
})

test_that("A parameter that isn't updated remains the same",{
  expect_equal(param(mod2)$KA, param(mod)$KA)
})

test_that("Parameter updates properly when passed to mrgsim",{
  expect_equal(param(mod10)$CL, param(mod2)$CL)
  expect_equal(param(mod10)$VC, param(mod2)$VC)
})

test_that("Parameter updates properly when added inside mrgsim call",{
  expect_equal(param(mod11)$CL, param(mod1)$CL)
  expect_equal(param(mod11)$VC, param(mod1)$VC)
})

rm(mod8,mod9,mod10,mod11)


test_that("Parameters update via param and list",{
  
  mod1 <- mod %>% param(list(CL=123,VC=456))
  expect_equal(param(mod1)$CL, 123)
  expect_equal(param(mod1)$VC, 456)
})

test_that("Parameters update via param ",{
  
  mod1 <- mod %>% param(CL=1123, VC=1456)
  expect_equal(param(mod1)$CL, 1123)
  expect_equal(param(mod1)$VC, 1456)
})


test_that("Parameters update via param and data.frame ",{
  
  mod1 <- mod %>% param(data.frame(CL=987,VC=654))
  expect_equal(param(mod1)$CL, 987)
  expect_equal(param(mod1)$VC, 654)
})

test_that("Initials update via init and list",{
  
  mod1 <- mod %>% init(list(CENT=123,EV1=456))
  expect_equal(init(mod1)$CENT, 123)
  expect_equal(init(mod1)$EV1, 456)
})

test_that("Initials update via init ",{
  
  mod1 <- mod %>% init(CENT=1123, EV1=1456)
  expect_equal(init(mod1)$CENT, 1123)
  expect_equal(init(mod1)$EV1, 1456)
})


test_that("Initials update via init and data.frame ",{
  
  mod1 <- mod %>% init(data.frame(CENT=987,EV1=654))
  expect_equal(init(mod1)$CENT, 987)
  expect_equal(init(mod1)$EV1, 654)
})

test_that("Initial conditions update properly via update()",{
  expect_equal(init(mod6)$EV1,5566)
})

context("Test updates: solver settings")

test_that("Solver setting hmin updates properly", {
  expect_equal(mod7@hmin,111)
})
test_that("Solver setting hmax updates properly", {
  expect_equal(mod7@hmax,222)
})
test_that("Solver setting maxsteps updates properly", {
  expect_equal(mod7@maxsteps,333)
})
test_that("Solver setting ixpr updates properly", {
  expect_equal(mod7@ixpr,444)
})
test_that("Solver setting mxhnil updates properly", {
  expect_equal(mod7@mxhnil,555)
})
test_that("Solver setting atol updates properly", {
  expect_equal(mod7@atol,1E-99)
})
test_that("Solver setting rtol updates properly", {
  expect_equal(mod7@rtol,1E-88)
})


context("Test updates: events")
myev <- ev(amt=111, time=222, ii=333, addl=444, cmt=999)

test_that("Events update properly through %>% operator",{
  expect_equivalent(myev, events(mod %>% ev(myev)))
})
test_that("Events update properly through update()",{
  expect_equivalent(myev, events(update(mod, events=myev)))
})

ev1 <- ev(amt=555,cmt=1,rate=33)
ev2 <- ev(amt=444,cmt=5,rate=22)
ev12 <- ev1+ev2

test_that("Events update properly through %>%  and  + operator", {
  expect_equivalent(ev12, events(mod %>% ev(ev1 + ev2)))
})




context("Testing parameter updates via data")
mod <- mrgsolve:::house()

context("Testing parameter updates via idata")
CL <- exp(rnorm(100, log(3),  sqrt(0.5)))
VC <- exp(rnorm(100, log(30), sqrt(0.5))) 
pars <- signif(data.frame(CL=CL,VC=VC),6)
pars$ID <- 1:nrow(pars)

out <- mrgsim(mod, idata=pars, end=8, carry.out=c("CL", "VC"))
out <- out %>% as.tbl %>% distinct(ID, .keep_all=TRUE)
out <- signif(as.data.frame(out[,c("CL", "VC", "ID")]),6)


test_that("Recover items from simulated data when passed in as idata",{
  expect_equivalent(out,pars)
})



data <- expand.grid(time=seq(0,12,1), ID=1:100, cmt=1)
data <- merge(data, pars, sort=FALSE)

out <- mrgsim(mod, data=data, carry.out=c("CL", "VC"))
out <- out %>% as.tbl %>% distinct(ID, .keep_all=TRUE)
out <- signif(as.data.frame(out)[,c("CL", "VC", "ID")], 6)

test_that("Recover items from simulated data when passed in as data", {
  expect_equivalent(out,pars)
})

events <- ev(time=c(0,24,48), amt=1000, rate=50, addl=c(0,0,10), ii=12,cmt=1)
out1 <- mrgsim(mod %>% ev(events), idata=data.frame(ID=1:20), end=200,
               carry.out=c("evid", "amt", "rate", "addl", "ii","cmt"), req="")
data1 <- as.data.frame(out1)

out2 <- mrgsim(mod, data=data1, carry.out=c("evid", "amt", "rate", "addl", "ii"), req="")
data2 <- as.data.frame(out2)

context("Events-based sim same results as data set-based sim")
test_that("CP is equal when simulating from events or data", {
  expect_identical(data1$CP, data2$CP)
})


context("Time-varying data items passed in via data set")

set.seed(11111)
data1$ROW <- sample(1:nrow(data1))

out <- mrgsim(mod, data=data1, carry.out=c("ROW"))

test_that("Time-varying data items in data are properly carried into output", {
  expect_true(all(data1$ROW == out$ROW))
})
