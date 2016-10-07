
library(mrgsolve)
library(testthat)



Sys.setenv(R_TESTS="")

code <- '
$PARAM CL=1, FORM=1,F1 = 0.1

$MAIN

F_CENT = 1;
if(FORM==2) F_CENT = F1;

$CMT CENT
$ODE dxdt_CENT = -(CL/10)*CENT;
$CAPTURE CL FORM  F1
'

mod <- mcode("bioav1",code) %>% carry.out(evid)


context("Dedicated bioavailability tests")

test_that("Bioav test with doses at time=0", {
  data <- expand.ev(ID=1:2,amt=100,FORM=c(1,2))
  out <- mod %>% data_set(data) %>%
    mrgsim() %>% dplyr::as.tbl() %>%
    filter(evid==1)
  expect_equal(c(100,100,10,10),out$CENT)
  expect_equal(rep(0,4),out$time)
  expect_equal(c(1,1,2,2),out$FORM)
})


# 
# test_that("Bioav test with doses starting at time !=0", {
#   data <- expand.ev(ID=1:2,amt=100,FORM=c(1,2), time=10)
#   out <- mod %>% data_set(data) %>%
#     mrgsim() %>% dplyr::as.tbl() %>%
#     filter(evid==1)
#   expect_equal(c(100,100,10,10),out$CENT)
#   expect_equal(rep(10,4),out$time)
#   expect_equal(c(1,1,2,2),out$FORM)
# })
# 
# 
# 
# test_that("Bioav, one ID multiple doses and FORM", {
#   data <- expand.ev(ID=1, amt=100, FORM=c(1,2,1,1,2),time=0,evid=1) %>% 
#     mutate(time=ID,ID=1,CL=0)
#   out <- mod %>% data_set(data) %>%
#     mrgsim() %>% dplyr::as.tbl() %>%
#     filter(evid !=0)
#   
#   expect_equal(c(100,110,210,310,320),out$CENT)
#   expect_equal(c(1,2,3,4,5), out$time)
#   expect_equal(c(1,2,1,1,2),out$FORM)
# })
# 
# 
# 
# 
# test_that("Bioav time=0 and addl", {
#   data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=0,evid=1) %>% 
#     mutate(ID=1,CL=0)
#   out <- mod %>% data_set(data) %>%
#     mrgsim(end=9, recsort=2) %>% dplyr::as.tbl() %>%
#     filter(evid==0)
#   expect_equal(seq(0,900,100),out$CENT)
#   expect_equal(seq(0,9), out$time)
# })
# 
# 
# 
# test_that("Bioav first dose time > 0 and addl", {
#   
#   data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=0,evid=1,FORM=2) %>% 
#     mutate(ID=1,CL=0)
#   out <- mod %>% data_set(data) %>%
#     mrgsim(end=9,recsort=2) %>% dplyr::as.tbl() %>%
#     filter(evid==0)
#   
#   expect_equal(seq(0,90,10),out$CENT)
#   expect_equal(seq(0,9), out$time)
# })
# 
# 
# 
# 
# test_that("Bioav test with doses at time=0", {
#   data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=0,evid=1,FORM=c(1,2)) %>% 
#     mutate(CL=0)
#  
#    out <- mod %>% data_set(data) %>%
#     mrgsim(end=9,recsort=2) %>% dplyr::as.tbl() %>%
#     filter(evid==0)
#   
#   expect_equal(c(seq(0,900,100),seq(0,90,10)),out$CENT)
#   expect_equal(rep(seq(0,9),2) ,out$time)
#   
# })
# 
# 
# test_that("Bioav test with doses at time != 0", {
#   data <- expand.ev(ID=1, amt=100, ii=1,addl=9,time=2,evid=1,FORM=c(1,2)) %>% 
#     mutate(CL=0)
#   out <- mod %>% data_set(data) %>%
#     mrgsim(recsort=2,end=9) %>% dplyr::as.tbl() %>%
#     filter(evid==0)
#   expect_equal(c(c(0,0,seq(0,700,100)),c(0,0,seq(0,70,10))),out$CENT)
#   expect_equal(rep(seq(0,9),2) ,out$time)
#   expect_true(all(out$CL==0))
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
