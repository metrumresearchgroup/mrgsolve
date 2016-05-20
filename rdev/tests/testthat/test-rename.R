
#library(metrumrg)
library(testthat)
#library(MASS)
library(mrgsolve)
library(magrittr)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)


mod <- mrgsolve:::house() %>% Req(CENT,RESP) 

data(exTheoph)
df <- exTheoph %>% mutate(KYLE=7)
mod %<>% data_set(df) 



context("Rename via carry.out #30")
test_that("tran item is renamed", {
  out <- mod %>% carry.out(EVID=evid) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,EVID), names(out))))
  out <- mod %>% carry.out(EVID=evid,addl) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,EVID,addl), names(out))))
  out <- mod %>% carry.out(X=addl) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,X), names(out))))
  out <- mod %>% carry.out(EVID=evid,addl) %>% mrgsim
  expect_false(all(is.element(s(evid), names(out))))
})


test_that("Item carried from data set is renamed", {
  out <- mod %>% carry.out(Dose,WEIGHT = WT) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,WEIGHT), names(out))))
  expect_true(all(is.element(s(Dose,WEIGHT), names(out))))
})


test_that("Item carried from data set is renamed", {
  out <- mod %>% carry.out(FOO=BAR) %>% mrgsim
  expect_equal(s(ID,time,CENT,RESP), names(out))
})










