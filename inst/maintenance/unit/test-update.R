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

context("test-update-unit")

code1 <- '
$SET request=""
$CMT CM
$PARAM A = 0, B = 0, C = 0
$CAPTURE A B C
' 

mod1 <- mcode("code1eekd", code1, warn=FALSE)

test_that("Update parameter - via param", {
  expect_equal(param(param(mod1,B = 2))$B,2)
  expect_equal(as.list(param(param(mod1,A=11,C=22))),list(A=11,B=0,C=22))

  out <- mod1 %>% mrgsim(end=8)
  expect_true(all(c(out$A==0,out$B==0,out$C==0)))
  
  out <- mod1 %>% param(A=3, B=2, C=1) %>% mrgsim
  expect_true(all(c(out$A==3,out$B==2,out$C==1)))
  
  expect_error(mod1 %>% param(FAKE = 5))
  expect_error(mod1 %>% param(list(FAKE=3),strict=TRUE))
  #expect_warning(mod1 %>% param(list(FAKE=3)))
  expect_is(mod1 %>% param(list(FAKE=3,B=2)), "mrgmod")
})



test_that("Update parameter - via idata", {
  idata <- expand.idata(ID=1, A=c(4,5,6),B=c(7,8,9),C=c(11,12,13,14))  
  out <- mod1 %>% idata_set(idata) %>% mrgsim %>% as_tibble %>% distinct(ID,A,B,C)
  expect_equal(unlist(out),unlist(idata))
  expect_identical(param(param(mod1,B = 2))$B,2)
})


test_that("Update parameter - via data, not-time-varying", {
  data <- expand.ev(ID=1, A=c(4,5,6),B=c(7,8,9),C=c(11,12,13,14),amt=2)
  out <- mod1 %>% data_set(data) %>% carry_out(amt,evid,cmt,time) %>%
    mrgsim() %>% as_tibble %>% filter(evid==1) %>% mutate(CM=NULL)
  expect_equal(unlist(out),unlist(data[,names(out)]))
  
})

test_that("Update parameter - via data, time-varying", {
  ## data with time-varying covariate
  data <- 
    bind_rows(
      tibble(ID=1, time=seq(0,10,1), A = 2*time, B = 1.1*time),
      tibble(ID=2, time=seq(0,15,1), A = 22*time, B = 11.1*time)
    ) %>% mutate(evid=ifelse(time==1,1,0),cmt=1)
    
  out <- mod1 %>% data_set(data)  %>% mrgsim()
  
  expect_true(all(c(data$A==out$A,data$B==out$B)))
  
})


