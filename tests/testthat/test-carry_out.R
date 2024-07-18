# Copyright (C) 2013 - 2020  Metrum Research Group
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

context("test-carry_out_recover")

mod <- mrgsolve::house()

data(exTheoph)

exTheoph <- 
  mutate(exTheoph, ROW=1:nrow(exTheoph)) %>%
  filter(ID <=10)

mod <- mrgsolve::house()

data <- 
  expand.idata(ID=1:5,amt=c(200,300)) %>%
  mutate(time=0,cmt=1,evid=1, addl=13000, ii=1, DOSE=amt,ROW=ID)

idata <- expand.idata(ID=1:10) %>% mutate(FOO=ID)

out <- 
  mod %>% 
  data_set(exTheoph, ID <=2) %>% 
  carry_out(WT,evid,ROW) %>%
  mrgsim(obsonly=TRUE)


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

test_that("carry_out from condensed data set", {
  
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

test_that("recover input data-set items", {
  dose <- seq(ev(amt=100,time=5,a=33,b="aa"),ev(amt=200,a=44,b="bb",time=10))
  out <- mrgsim(mod,dose,recover="A=a,b", carry_out = "a")
  expect_is(out$A,"numeric")
  expect_is(out$b,"character")
  expect_true(all(out$a==out$A))
  expect_silent(mrgsim(mod,dose,recover="A=a"))
  expect_error(mrgsim(mod,recover="a",carry_out="a"))
})

test_that("recover input data-set items that are parameters", {
  dose <- seq(ev(amt=100,time=5,a=33,b="aa"),ev(amt=200,a=44,b="bb",time=10))
  dose <- dplyr::mutate(dose, CL = factor(a), KA = "BB")
  expect_error(
    mrgsim(mod, dose), 
    regexp="data set column: KA", 
    fixed = TRUE
  )
  expect_error(
    mrgsim(mod, dose, recover = "CL"), 
    regexp="data set column: CL", 
    fixed = TRUE
  )
})
 
test_that("recover input idata-set items", {
  idata <- expand.idata(a = c(33,44), b = c("aa", "bb")) 
  out <- mrgsim_ei(mod,ev(amt = 100),idata,recover ="a,b")
  expect_is(out$a,"numeric")
  expect_is(out$b,"character")
  expect_error(mrgsim_e(mod,idata,recover="b",carry_out="b"))
})

test_that("recover input idata-set items that are parameters", {
  idata <- expand.idata(a = c(33,44), b = c("aa", "bb")) 
  idata <- dplyr::mutate(idata, CL = factor(a), KA = "BB")
  expect_error(
    mrgsim(mod,ev(amt=100),idata), 
    regexp = "data set column: KA", 
    fixed = TRUE
  )
  expect_error(
    mrgsim(mod,ev(amt=100),idata,recover = "CL"), 
    regexp="data set column: CL", 
    fixed = TRUE
  )
})

test_that("error to request matrix and recover character data", {
  data <- expand.ev(amt = 100, group = "A")
  expect_is(mrgsim(mod, data, recover = "group"), "mrgsims")
  expect_error(
    mrgsim(mod, data, recover = "group", output = "matrix"), 
    msg = "can't return matrix because non-numeric data was found"
  )
})
