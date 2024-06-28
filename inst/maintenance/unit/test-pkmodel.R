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

project <- file.path(system.file(package="mrgsolve"), "models")

context("Compare PKMODEL with equivalent ODEs")

ode <- mrgsolve:::house(rtol = 1E-9, atol = 1E-9) %>% param(CL=1,VC=20,KA=1.1)

code1 <- '
$PARAM CL=1, V=20, KA=1.1
$PKMODEL cmt = "GUT CENT", depot = TRUE
'

pred1 <- mcode("test13.2", code1)

ode <- mcode("pkmodel_1", {'
$PARAM CL = 1, V = 20, KA = 1.1
$CMT GUT CENT
$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT-(CL/V)*CENT;
'}, rtol = 1E-9, atol = 1E-9)

test_that("ADVAN2 same as ODE - initial condition", {
  out1 <- ode  %>% init(CENT=1000) %>%
    Req(GUT,CENT) %>% mrgsim(end=24,delta=0.1 ,digits=5, rtol = 1E-12)
  
  out2 <- pred1 %>% init(CENT=1000) %>% 
    Req(GUT,CENT) %>% mrgsim(end = 24, delta=0.1, digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(names(out1),names(out2))
})


test_that("ADVAN2 same as ODE - GUT,bolus,addl", {
  e <- ev(amt=100,ii=48,addl=4)
  out1 <- ode  %>% ev(e) %>%
    Req(GUT,CENT) %>% mrgsim(end=264,delta=0.1,rtol = 1E-12)
  out2 <- pred1 %>% ev(e) %>%
    Req(GUT,CENT) %>% mrgsim(end=264,delta=0.1)

  expect_equal(out1$CENT,out2$CENT)
})


test_that("ADVAN2 same as ODE - GUT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out1 <- ode  %>% ev(e) %>%
    Req(CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred1 %>% ev(e) %>%
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
})

test_that("ADVAN2 same as ODE - CENT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred1 %>% ev(e) %>%
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  e2 <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out3 <- pred1 %>% ev(e2) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$CENT,out3$CENT)
})


test_that("ADVAN2 same as ODE - CENT,infus,ss,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1,ss=1)
  out1 <- ode  %>% ev(e) %>%
    mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12, 
          recsort=3)
  out2 <- pred1 %>% ev(e) %>%
   mrgsim(end=264,delta=0.1,digits=5,recsort=3)
  expect_equal(out1$CENT,out2$CENT)
})

test_that("ADVAN2 same as ODE - GUT,bolus,ss,addl", {
  e <- ev(amt=1000,ii=12,addl=16,cmt=1,ss=1)
  out1 <- ode  %>% ev(e) %>%
    Req(CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5,
                         hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred1 %>% ev(e) %>%
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
})



project <- file.path(system.file(package="mrgsolve"), "models")

ode_code <- '
$PARAM CL=1, VC=20, Q=4, KA=1.1, VP=300
$CMT GUT CENT PER
$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - CENT*(CL+Q)/VC + PER*Q/VP;
dxdt_PER = CENT*Q/VC - PER*Q/VP;
'

pred2 <- '
$PARAM CL=1, V2=20, KA=1.1, Q=4, V3=300
$PKMODEL cmt = "GUT CENT PER", depot = TRUE
'

ode <- mcode("test13_3",ode_code, rtol = 1E-9, atol = 1E-9) 
pred2 <- mcode("test13_5",pred2, rtol = 1E-9, atol = 1E-9)

test_that("ADVAN4 same as ODE - initial condition", {
  out1 <- ode  %>% init(GUT=1000) %>%
    Req(GUT,CENT) %>% mrgsim(end=24,delta=0.1 ,digits=5)
  out2 <- pred2 %>% init(GUT=1000) %>%
    Req(GUT,CENT) %>% mrgsim(end=24,delta=0.1,digits=5)


  expect_equal(out1$CENT,out2$CENT)
  expect_equal(names(out1),names(out2))
})

test_that("ADVAN4 same as ODE - GUT,bolus,addl", {
  e <- ev(amt=100,ii=48,addl=4)
  out1 <- ode  %>% ev(e) %>%
    Req(GUT,CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5)
  out2 <- pred2 %>% ev(e) %>%
    Req(GUT,CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})

test_that("ADVAN4 same as ODE - GUT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4)
  out1 <- ode  %>% ev(e) %>%
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred2 %>% ev(e) %>%
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})

test_that("ADVAN4 same as ODE - CENT,infus,addl", {
  
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)

  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out3 <- pred2 %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out3$CENT)
  expect_equal(out1$PER,out3$PER)
})


test_that("ADVAN4 same as ODE - CENT,infus,ss,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=2,ss=1)
  out1 <- ode  %>% ev(e) %>%
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred2 %>% ev(e) %>%
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})

test_that("ADVAN4 same as ODE - GUT,bolus,ss,addl", {
  ode <- update(ode, rtol = 1e-8, atol = 1e-15)
  pred2 <- update(pred2, rtol = 1e-8, atol = 1e-15)
  e <- ev(amt=100,ii=12,addl=16,cmt=1,ss=1)
  out1<- ode  %>% ev(e) %>%
    mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1)
  out2 <- pred2 %>% ev(e) %>%
     mrgsim(end=264,delta=0.1,digits=5)
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})

test_that("Incorrect number of compartments causes error", {
  code <- '$PKMODEL cmt="GUT CENT OTHER PERIPH", depot = TRUE'
  expect_error(mod <- mcode("a2error3",code))
})

test_that("Error when KA==K10 in 1 compartment model", {
  mod <- param(pred1, KA = 1.5, CL = 1.5, V = 1)
  expect_error(mrgsim(mod), "k10 is too close to ka")
  mod <- param(mod, V = 1 + 5e-14)
  expect_is(mrgsim(mod), "mrgsims")
  mod <- param(mod, V = 1 + 1e-14)
  expect_error(mrgsim(mod), "k10 is too close to ka")
})
