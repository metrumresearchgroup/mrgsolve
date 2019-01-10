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


context("test-inventory-too")

test_that("inventory conditions", {
    
  mod <- mrgsolve:::house()
  
  data <- expand.ev(amt=100,CL=1, SEX=0)
  idata <- data.frame(ID=1, CL=1, SEX=0)
  
  expect_error(inventory(mod,data,everything()))
  expect_warning(inventory(mod,data))
  expect_error(inventory(mod,data,c("CL", "VC")))
  expect_message(inventory(mod,data,c("CL", "SEX")))
  expect_message(inventory(mod,data,CL,SEX))
  expect_error(mod %>% data_set(data,need=c("CL", "VC")))
  expect_error(mod %>% idata_set(data,need=c("CL", "WTCL")))
  expect_is(mod %>% data_set(data,need="CL") %>% mrgsim(end=1),"mrgsims")
})
