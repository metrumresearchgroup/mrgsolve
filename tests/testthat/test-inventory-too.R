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


context("test-inventory-too")

test_that("inventory conditions", {
    
  mod <- mrgsolve::house()
  
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

test_that("check_data_names", {
  code <- c("$PARAM  A=1", "$PARAM @input \n B=2", "$PARAM @covariates \n C=3", 
            "$PARAM @tag foo \n D = 4")
  mod <- mcode("cdn-1", code[1], compile = FALSE)
  data <- data.frame(A = 1)
  expect_warning(check_data_names(data, mod), "Did not find any")
  expect_error(check_data_names(data, mod, strict = TRUE), "Did not find any")
  
  mod <- mcode("cdn-2", paste0(code[1:2], collapse = "\n"), compile = FALSE)
  data <- data.frame(A = 1)
  expect_warning(check_data_names(data, mod, "B (input)"))
  expect_silent(check_data_names(data, mod, silent = TRUE))
  expect_warning(check_data_names(data, mod, check_input = FALSE), "Did not find")
  expect_error(check_data_names(data, mod, strict = TRUE), "Could not find")
  
  mod <- mcode("cdn-3", paste0(code[1:3], collapse = "\n"), compile = FALSE)
  data <- data.frame(A = 1, B = 2, C = 3)
  expect_message(check_data_names(data, mod), "Found all")
  expect_silent(check_data_names(data, mod, silent = TRUE))
  
  data$C <- NULL
  expect_warning(check_data_names(data, mod), "C \\(covariates\\)")
  expect_message(
    check_data_names(data, mod, check_covariates = FALSE), 
    "Found all expected"
  )
  
  mod <- mcode("cdn-4", paste0(code[4], collapse = "\n"), compile = FALSE)
  data <- data.frame(D = 4)
  expect_warning(check_data_names(data, mod), "Did not find")
  expect_message(check_data_names(data, mod, tags = "foo"), "Found all")

  mod <- mcode("cdn-5", paste0(code[c(3,4)], collapse = "\n"), compile = FALSE)
  data <- data.frame(C=3)
  expect_message(check_data_names(data, mod), "Found all expected")
  expect_warning(check_data_names(data, mod, tags = "foo"), "Could not find")
})
