library(mrgsolve)
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")

code1 <- '
$SET request=""
$CMT CM
$PARAM A = 0, B = 0, C = 0
$CAPTURE A B C
' 

mod1 <- mcode("code1", code1, warn=FALSE)

context("test-update")


test_that("Update parameter - via param", {
  expect_equal(param(param(mod1,B = 2))$B,2)
  expect_equal(as.list(param(param(mod1,A=11,C=22))),list(A=11,B=0,C=22))

  out <- mod1 %>% mrgsim(end=8)
  expect_true(all(c(out$A==0,out$B==0,out$C==0)))
  
  out <- mod1 %>% param(A=3, B=2, C=1) %>% mrgsim
  expect_true(all(c(out$A==3,out$B==2,out$C==1)))
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





