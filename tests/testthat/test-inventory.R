# Copyright (C) 2013 - 2022  Metrum Research Group
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

context("test-inventory")

mod <- mcode(
  "test", 
  "$PARAM KA = 1, CL = 1, V = 1, OCC1 = 1, OCC2 = 1, F = 1",
  compile = FALSE
)
all_obj <- data.frame(KA = 1, CL = 1, V = 1, OCC1 = 1, OCC2 = 1, F = 1)
missing_obj <- data.frame(KA = 1, CL = 1, V = 1, OCC1 = 1, F = 1)

test_that("test-inventory check candidate parameters [SLV-TEST-0014]", {
  # no requirements gives back model 
  expect_message(result <- inventory(mod, all_obj))
  expect_s4_class(result, "mrgmod")
  
  # everything says it found all and returns original model 
  expect_message(
    inventory(mod, all_obj, dplyr::everything()), 
    "Found all required parameters"
  )
  expect_s4_class(
    inventory(mod, all_obj, dplyr::everything()), 
    "mrgmod"
  )
})

test_that("test-inventory error when missing required names [SLV-TEST-0015]", {
  expect_error(inventory(mod, missing_obj, dplyr::everything()))
  expect_error(inventory(mod, missing_obj, dplyr::contains("OCC")))
  expect_error(inventory(mod, missing_obj, V:F))
  expect_error(inventory(mod, missing_obj, OCC2))
})

test_that("inventory conditions [SLV-TEST-0016]", {
  mod <- house()
  data <- expand.ev(amt=100,CL=1, SEX=0)
  idata <- data.frame(ID=1, CL=1, SEX=0)
  expect_error(inventory(mod,data,everything()))
  expect_warning(inventory(mod,data))
  expect_error(inventory(mod,data,c("CL", "VC")))
  expect_message(inventory(mod,data,c("CL", "SEX")))
  expect_message(inventory(mod,data,CL,SEX))
  expect_error(data_set(mod, data,need=c("CL", "VC")))
  expect_error(idata_set(mod,data,need=c("CL", "WTCL")))
  expect_is(data_set(mod,data, need="CL") %>% mrgsim(end=1),"mrgsims")
})
