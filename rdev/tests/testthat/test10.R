
#library(metrumrg)
library(testthat)
#library(MASS)
library(mrgsolve)
require(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

mrgsolve:::comp_forget()
rm(list=ls())

mod <- mrgsolve:::house()

data(exTheoph)
exTheoph <- mutate(exTheoph, ROW=1:nrow(exTheoph)) %>%
  filter(ID <=10)

mod <- mrgsolve:::house()

data <- expand.idata(ID=1:5,amt=c(200,300)) %>%
  mutate(time=0,cmt=1,evid=1, addl=13000, ii=1, DOSE=amt,ROW=ID)

idata <- expand.idata(ID=1:10) %>% mutate(FOO=ID)

out <- mod %>% data_set(exTheoph, ID <=2) %>% carry.out(WT,evid,ROW) %>% 
  obsonly() %>% mrgsim(obsfirst=FALSE)


context("Testing carry.out")

out <- mod %>% 
  data_set(exTheoph) %>% 
  carry.out(WT,ROW,FOO)%>% 
  idata_set(idata) %>%
  mrgsim(end=3,delta=1)

test_that("carry.out from complete data set",{
  expect_equal(out$WT,exTheoph$WT)
  expect_equal(out$ROW,exTheoph$ROW)
})


x <- out %>% as.data.frame %>% distinct(ID, .keep_all=TRUE)
y <- out %>% as.data.frame %>% distinct(ID,FOO, .keep_all=TRUE)

test_that("carry.out from idata set",{
  expect_equal(x$FOO,idata$FOO)
  expect_identical(x,y)
})


out <- mod %>%
  data_set(data) %>%
  carry.out(WT,ROW,FOO,evid) %>%
  idata_set(idata) %>%
  mrgsim(end=3,delta=1)

out2 <- mod %>%
  data_set(data) %>%
  carry.out(WT,ROW,FOO,evid) %>%
  idata_set(idata) %>%
  mrgsim(end=3,delta=1, obsonly=TRUE)


x <- out %>% as.data.frame %>% distinct(ID, .keep_all=TRUE)
y <- out %>% as.data.frame %>% distinct(ID,ROW,FOO, .keep_all=TRUE)

x2 <- out2 %>% as.data.frame %>% distinct(ID, .keep_all=TRUE)
y2 <- out2 %>% as.data.frame %>% distinct(ID,ROW,FOO, .keep_all=TRUE)

test_that("carry.out from condensed data set", {
  expect_identical(x,y)
  expect_identical(x2,y2)
})

