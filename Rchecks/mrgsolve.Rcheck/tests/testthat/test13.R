library(mrgsolve)
library(testthat)
require(dplyr)

Sys.setenv(R_TESTS="")

options("mrgsolve_mread_quiet"=TRUE)
project <- file.path(system.file(package="mrgsolve"), "models")
tmp <- tempdir()

context("Compare ADVAN 2 with equivalent ODEs.")

ode <- mrgsolve:::house() %>% param(Q = 0,CL=1,VC=20,KA=1.1)

code <- '
$PARAM CL=1, VC=20, KA=1.1
$CMT GUT CENT
$ADVAN2
$MAIN
pred_CL = CL;
pred_V = VC;
pred_KA = KA;
'

pred <- mread("test13.1", tempdir(), code,preclean=TRUE)

code2 <- '
$PARAM CL=1, V=20, KA=1.1
$CMT CENT
$SUB advan=1, trans=2
'
pred2 <- mread("test13.2", tempdir(), code2)



test_that("ADVAN2 same as ODE - initial condition", {
  out1 <- ode  %>% init(GUT=1000) %>% 
    Req(GUT,CENT) %>% mrgsim(end=24,delta=0.1 ,digits=5)
  out2 <- pred %>% init(GUT=1000) %>% 
    Req(GUT,CENT) %>% mrgsim(end=24,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(names(out1),names(out2))
})


test_that("ADVAN2 same as ODE - GUT,bolus,addl", {
  e <- ev(amt=100,ii=48,addl=4)
  out1 <- ode  %>% ev(e) %>% 
    Req(GUT,CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5)
  out2 <- pred %>% ev(e) %>% 
    Req(GUT,CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
})



test_that("ADVAN2 same as ODE - GUT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=2)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
})


test_that("ADVAN2 same as ODE - CENT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=2)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  e2 <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out3 <- pred2 %>% ev(e2) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$CENT,out3$CENT)
})


test_that("ADVAN2 same as ODE - CENT,infus,ss,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=2,ss=1)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  expect_equal(out1$CENT,out2$CENT)
})



test_that("ADVAN2 same as ODE - GUT,bolus,ss,addl", {
  e <- ev(amt=1000,ii=12,addl=16,cmt=1,ss=1)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1 ,digits=5,
                         hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
})



project <- file.path(system.file(package="mrgsolve"), "models")

context("Compare ADVAN 4 with equivalent ODEs.")

ode_code <- '
$PARAM CL=1, VC=20, Q=4, KA=1.1, VP=300
$CMT GUT CENT PER
$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - CENT*(CL+Q)/VC + PER*Q/VP;
dxdt_PER = CENT*Q/VC - PER*Q/VP;
'


pred_code <- '
$PARAM CL=1, VC=20, KA=1.1, Q=4, VP=300
$CMT GUT CENT PER
$ADVAN4
$MAIN
pred_CL = CL;
pred_V2 = VC;
pred_KA = KA;
pred_Q = Q;
pred_V3 = VP;
'
pred2 <- '
$PARAM CL=1, V2=20, KA=1.1, Q=4, V3=300
$CMT CENT PER
$SUB advan=3, trans=4
'

comp_forget()

ode <- mread(code=ode_code, model="test13.3", project=tmp) 
pred <- mread(code=pred_code, model="test13.4", project=tmp)
pred2 <- mread(code=pred2, model="test13.5", project=tmp)



test_that("ADVAN4 same as ODE - initial condition", {
  out1 <- ode  %>% init(GUT=1000) %>% 
    Req(GUT,CENT) %>% mrgsim(end=24,delta=0.1 ,digits=5)
  out2 <- pred %>% init(GUT=1000) %>% 
    Req(GUT,CENT) %>% mrgsim(end=24,delta=0.1,digits=5)
  
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(names(out1),names(out2))
})


test_that("ADVAN4 same as ODE - GUT,bolus,addl", {
  e <- ev(amt=100,ii=48,addl=4)
  out1 <- ode  %>% ev(e) %>% 
    Req(GUT,CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5)
  out2 <- pred %>% ev(e) %>% 
    Req(GUT,CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})



test_that("ADVAN4 same as ODE - GUT,infus,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})

test_that("ADVAN4 same as ODE - CENT,infus,addl", {
  
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=2)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=1)
  out3 <- pred2 %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
  
  expect_equal(out1$CENT,out3$CENT)
  expect_equal(out1$PER,out3$PER)
})


test_that("ADVAN4 same as ODE - CENT,infus,ss,addl", {
  e <- ev(amt=1000,rate=50,ii=48,addl=4,cmt=2,ss=1)
  out1 <- ode  %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5,hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})



test_that("ADVAN4 same as ODE - GUT,bolus,ss,addl", {
  e <- ev(amt=100,ii=12,addl=16,cmt=1,ss=1)
  out1<- ode  %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1 ,digits=5,
                             hmax=0.1,atol=1E-12,rtol=1E-12)
  out2 <- pred %>% ev(e) %>% 
    Req(CENT,PER) %>% mrgsim(end=264,delta=0.1,digits=5)
  
  
  expect_equal(out1$CENT,out2$CENT)
  expect_equal(out1$PER,out2$PER)
})

context("Check $CMT/$INIT specification with ADVAN2/4")
test_that("No compartments generates error", {
  expect_error(mod <- mread(code="$ADVAN2", model="a2error1",tmp))
  expect_error(mod <- mread(code="$ADVAN4", model="a2error2",tmp))
})

test_that("Incorrect number of compartments causes error", {
  expect_error(mod <- mread(code="$ADVAN2\n$CMT A", model="a2error3",tmp))
  expect_error(mod <- mread(code="$ADVAN4\n$CMT A B C D", model="a2error4",tmp))
})

test_that("Correct number of compartments causes no error", {
  expect_is(mread(code="$ADVAN2\n$CMT A B", model="a2error5",tmp),"mrgmod")
  expect_is(mread(code="$ADVAN4\n$CMT A B C", model="a2error6",tmp),"mrgmod")
})




