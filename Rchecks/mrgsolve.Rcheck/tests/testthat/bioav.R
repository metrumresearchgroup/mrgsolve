
library(mrgsolve)
library(testthat)
library(MASS)


Sys.setenv(R_TESTS="")



code <- '
$PARAM CL=1, FORM=1,F1 = 0.1

$MAIN

_F(1) = 1;
if(FORM==2) _F(1) = F1;

$CMT CENT
$ODE dxdt_CENT = -(CL/10)*CENT;
$CAPTURE CL FORM  F1
'
comp_forget()
rc <- 2

mod <- mread(code=code, "bioav1",tempdir()) %>% carry.out(evid)



context("Dedicated bioavailability tests")

data <- expand.ev(ID=1:2,amt=100,FORM=c(1,2))
out <- mod %>% data_set(data) %>%
  mrgsim(recsort=rc) %>% dplyr::as.tbl() %>%
  filter(evid==1)

test_that("Bioav test with doses at time=0", {
  expect_equal(c(100,100,10,10),out$CENT)
  expect_equal(rep(0,4),out$time)
  expect_equal(c(1,1,2,2),out$FORM)
})


data <- expand.ev(ID=1:2,amt=100,FORM=c(1,2), time=10)
out <- mod %>% data_set(data) %>%
  mrgsim(recsort=rc) %>% dplyr::as.tbl() %>%
  filter(evid==1)

test_that("Bioav test with doses starting at time !=0", {
  expect_equal(c(100,100,10,10),out$CENT)
  expect_equal(rep(10,4),out$time)
  expect_equal(c(1,1,2,2),out$FORM)
})


data <- expand.ev(ID=1, amt=100, FORM=c(1,2,1,1,2),time=0,evid=1) %>% 
  mutate(time=ID,ID=1,CL=0)
out <- mod %>% data_set(data) %>%
  mrgsim(recsort=rc) %>% dplyr::as.tbl() %>%
  filter(evid !=0)

test_that("Bioav, one ID multiple doses and FORM", {
  expect_equal(c(100,110,210,310,320),out$CENT)
  expect_equal(c(1,2,3,4,5), out$time)
  expect_equal(c(1,2,1,1,2),out$FORM)
})



data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=0,evid=1) %>% 
  mutate(ID=1,CL=0)
out <- mod %>% data_set(data) %>%
  mrgsim(recsort=rc,end=9) %>% dplyr::as.tbl() %>%
  filter(evid==0)

test_that("Bioav time=0 and addl", {
  expect_equal(seq(100,1000,100),out$CENT)
  expect_equal(seq(0,9), out$time)
})


data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=0,evid=1,FORM=2) %>% 
  mutate(ID=1,CL=0)
out <- mod %>% data_set(data) %>%
  mrgsim(recsort=rc,end=9) %>% dplyr::as.tbl() %>%
  filter(evid==0)


test_that("Bioav first dose time > 0 and addl", {
  expect_equal(seq(10,100,10),out$CENT)
  expect_equal(seq(0,9), out$time)
})


data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=0,evid=1,FORM=c(1,2)) %>% 
  mutate(CL=0)
out <- mod %>% data_set(data) %>%
  mrgsim(recsort=rc,end=9) %>% dplyr::as.tbl() %>%
  filter(evid==0)

test_that("Bioav test with doses at time=0", {
  expect_equal(c(seq(100,1000,100),seq(10,100,10)),out$CENT)
  expect_equal(rep(seq(0,9),2) ,out$time)
})

data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=2,evid=1,FORM=c(1,2)) %>% 
  mutate(CL=0)
out <- mod %>% data_set(data) %>%
  mrgsim(recsort=rc,end=9) %>% dplyr::as.tbl() %>%
  filter(evid==0)

test_that("Bioav test with doses at time=0", {
  expect_equal(c(c(0,0,seq(100,800,100)),c(0,0,seq(10,80,10))),out$CENT)
  expect_equal(rep(seq(0,9),2) ,out$time)
  expect_true(all(out$CL==0))
})














