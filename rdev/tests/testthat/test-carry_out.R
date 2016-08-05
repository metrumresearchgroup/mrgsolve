
library(testthat)
library(mrgsolve)
require(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

rm(list=ls())

mod <- mrgsolve:::house()

data(exTheoph)

exTheoph <- 
  mutate(exTheoph, ROW=1:nrow(exTheoph)) %>%
  filter(ID <=10)

mod <- mrgsolve:::house()

data <- 
  expand.idata(ID=1:5,amt=c(200,300)) %>%
  mutate(time=0,cmt=1,evid=1, addl=13000, ii=1, DOSE=amt,ROW=ID)

idata <- expand.idata(ID=1:10) %>% mutate(FOO=ID)

out <- 
  mod %>% 
  data_set(exTheoph, ID <=2) %>% 
  carry_out(WT,evid,ROW) %>% 
  obsonly() %>% 
  mrgsim(obsfirst=FALSE)


context("Testing carry_out")

out <- mod %>% 
  data_set(exTheoph) %>% 
  carry_out(WT,ROW,FOO)%>% 
  idata_set(idata) %>%
  mrgsim(end=3,delta=1)

test_that("carry_out from complete data set",{
  expect_equal(out$WT,exTheoph$WT)
  expect_equal(out$ROW,exTheoph$ROW)
})



test_that("carry_out from idata set",{
  x <- out %>% as.data.frame %>% distinct(ID, .keep_all=TRUE)
  y <- out %>% as.data.frame %>% distinct(ID,FOO, .keep_all=TRUE)
  
  expect_equal(x$FOO,idata$FOO)
  expect_identical(x,y)
})




test_that("carry.out from condensed data set", {
  
  out <- mod %>%
    data_set(data) %>%
    carry_out(WT,ROW,FOO,evid) %>%
    idata_set(idata) %>%
    mrgsim(end=3,delta=1)
  
  out2 <- mod %>%
    data_set(data) %>%
    carry_out(WT,ROW,FOO,evid) %>%
    idata_set(idata) %>%
    mrgsim(end=3,delta=1, obsonly=TRUE)
  
  x <- out %>% as.data.frame %>% distinct(ID, .keep_all=TRUE)
  y <- out %>% as.data.frame %>% distinct(ID,ROW,FOO, .keep_all=TRUE)
  x2 <- out2 %>% as.data.frame %>% distinct(ID, .keep_all=TRUE)
  y2 <- out2 %>% as.data.frame %>% distinct(ID,ROW,FOO, .keep_all=TRUE)
  expect_identical(x,y)
  expect_identical(x2,y2)
})

