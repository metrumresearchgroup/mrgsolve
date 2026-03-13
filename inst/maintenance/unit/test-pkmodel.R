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

## --------------------------------------------------------------------------
## ADVAN 11/12: Three-compartment model
## --------------------------------------------------------------------------

ode3_code <- '
$PARAM CL=1, VC=20, Q=4, KA=1.1, VP=300, Q2=0.8, VP2=50
$CMT GUT CENT PER1 PER2
$ODE
double k10 = CL/VC;
double k12 = Q/VC;
double k21 = Q/VP;
double k13 = Q2/VC;
double k31 = Q2/VP2;

dxdt_GUT  = -KA*GUT;
dxdt_CENT = KA*GUT - (k10+k12+k13)*CENT + k21*PER1 + k31*PER2;
dxdt_PER1 = k12*CENT - k21*PER1;
dxdt_PER2 = k13*CENT - k31*PER2;
'

pred3_code <- '
$PARAM CL=1, V2=20, KA=1.1, Q=4, V3=300, Q2=0.8, V4=50
$PKMODEL cmt = "GUT CENT PER1 PER2", depot = TRUE
'

ode3 <- mcode("ode3cmt", ode3_code, rtol = 1E-9, atol = 1E-9)
pred3 <- mcode("pk3cmt", pred3_code, rtol = 1E-9, atol = 1E-9)

test_that("ADVAN12 same as ODE - initial condition", {
  out1 <- ode3  %>% init(CENT=1000) %>%
    Req(GUT,CENT,PER1,PER2) %>% mrgsim(end=24,delta=0.1,digits=5)
  out2 <- pred3 %>% init(CENT=1000) %>%
    Req(GUT,CENT,PER1,PER2) %>% mrgsim(end=24,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
  expect_equal(names(out1),names(out2))
})

test_that("ADVAN12 same as ODE - GUT,bolus,addl", {
  e <- ev(amt=100,ii=48,addl=4)
  out1 <- ode3  %>% ev(e) %>%
    Req(GUT,CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)
  out2 <- pred3 %>% ev(e) %>%
    Req(GUT,CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

test_that("ADVAN12 same as ODE - GUT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4)
  out1 <- ode3  %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred3 %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

test_that("ADVAN12 same as ODE - CENT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out1 <- ode3  %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)

  e2 <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out2 <- pred3 %>% ev(e2) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

test_that("ADVAN12 same as ODE - CENT,infus,ss,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=2,ss=1)
  out1 <- ode3  %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred3 %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

test_that("ADVAN12 same as ODE - GUT,bolus,ss,addl", {
  ode3a <- update(ode3, rtol = 1e-8, atol = 1e-15, ss_rtol = 1e-4)
  pred3a <- update(pred3, rtol = 1e-8, atol = 1e-15, ss_rtol = 1e-4)
  e <- ev(amt=100,ii=12,addl=16,cmt=1,ss=1)
  out1 <- ode3a  %>% ev(e) %>%
    mrgsim(end=264,delta=0.1,digits=5,hmax=0.1)
  out2 <- pred3a %>% ev(e) %>%
    mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

## ADVAN11 (no depot, 3 compartments)
ode3nd_code <- '
$PARAM CL=1, VC=20, Q=4, VP=300, Q2=0.8, VP2=50
$CMT CENT PER1 PER2
$ODE
double k10 = CL/VC;
double k12 = Q/VC;
double k21 = Q/VP;
double k13 = Q2/VC;
double k31 = Q2/VP2;

dxdt_CENT = -(k10+k12+k13)*CENT + k21*PER1 + k31*PER2;
dxdt_PER1 = k12*CENT - k21*PER1;
dxdt_PER2 = k13*CENT - k31*PER2;
'

pred3nd_code <- '
$PARAM CL=1, V1=20, Q=4, V2=300, Q2=0.8, V3=50
$PKMODEL cmt = "CENT PER1 PER2", depot = FALSE
'

ode3nd <- mcode("ode3cmt_nd", ode3nd_code, rtol = 1E-9, atol = 1E-9)
pred3nd <- mcode("pk3cmt_nd", pred3nd_code, rtol = 1E-9, atol = 1E-9)

test_that("ADVAN11 same as ODE - initial condition", {
  out1 <- ode3nd  %>% init(CENT=1000) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=24,delta=0.1,digits=5)
  out2 <- pred3nd %>% init(CENT=1000) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=24,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

test_that("ADVAN11 same as ODE - CENT,bolus,addl", {
  e <- ev(amt=100,ii=48,addl=4,cmt=1)
  out1 <- ode3nd  %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)
  out2 <- pred3nd %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

test_that("ADVAN11 same as ODE - CENT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out1 <- ode3nd  %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred3nd %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
})

test_that("ADVAN11 same as ODE - CENT,infus,ss,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1,ss=1)
  out1 <- ode3nd  %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred3nd %>% ev(e) %>%
    Req(CENT,PER1,PER2) %>% mrgsim(end=264,delta=0.1,digits=5)

  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER1,out2$PER1)
  expect_equal(out1$PER2,out2$PER2)
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
