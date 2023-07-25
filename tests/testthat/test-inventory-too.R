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
  code <- c("$PARAM  A=1", 
            "$PARAM @input \n B=2", 
            "$PARAM @covariates \n C=3", 
            "$PARAM @tag foo \n D = 4")
  
  # There must be something to check against
  mod <- mcode("cdn-1", code[1], compile = FALSE)
  data <- data.frame(A = 1)
  expect_warning(check_data_names(data, mod), 
                 "Did not find any inputs, covariates, or user")
  expect_error(check_data_names(data, mod, mode = "error"), 
               "Did not find any inputs, covariates, or user")
  
  # missing parameters in data
  mod <- mcode("cdn-2", paste0(code[1:2], collapse = "\n"), compile = FALSE)
  data <- data.frame(A = 1)
  expect_warning(check_data_names(data, mod), "Could not find the following")
  expect_warning(check_data_names(data, mod, silent = TRUE), 
                 "Could not find the following")
  expect_warning(check_data_names(data, mod, check_input = FALSE), 
                 "Did not find any inputs, covariates, or user")
  expect_error(check_data_names(data, mod, mode = "error"), 
               "Could not find the following")
  expect_message(check_data_names(data, mod, mode = "inform"), 
                 "Could not find the following")
  
  mod <- mcode("cdn-3", paste0(code[1:3], collapse = "\n"), compile = FALSE)
  data <- data.frame(A = 1, B = 2, C = 3)
  expect_message(check_data_names(data, mod), "Found all")
  expect_silent(check_data_names(data, mod, silent = TRUE))
  
  data$C <- NULL
  expect_warning(check_data_names(data, mod), "C (covariates)", fixed = TRUE)
  expect_message(
    check_data_names(data, mod, check_covariates = FALSE), 
    "Found all expected"
  )
  
  mod <- mcode("cdn-4", paste0(code[4], collapse = "\n"), compile = FALSE)
  data <- data.frame(D = 4)
  expect_warning(check_data_names(data, mod), 
                 "Did not find any inputs, covariates, or user")
  expect_message(check_data_names(data, mod, tags = "foo"), "Found all")
  
  mod <- mcode("cdn-5", paste0(code[c(3,4)], collapse = "\n"), compile = FALSE)
  data <- data.frame(C=3)
  expect_message(check_data_names(data, mod), "Found all expected")
  expect_warning(check_data_names(data, mod, tags = "foo"), 
                 "D (foo)", fixed = TRUE)
  expect_error(check_data_names(data, mod, tags = "kyle"), 
               "Unrecognized user tag")
  
  mod <- house()
  data <- data.frame(WGT = 70, SEX = 10)
  expect_warning(check_data_names(data, mod), "WT (covariates)", fixed = TRUE)
  expect_warning(
    check_data_names(data, mod, check_covariates = FALSE), 
    "Did not find any inputs, covariates, or user"
  )
  
  mod <- modlib("1005", compile = FALSE)
  tags <- param_tags(mod)
  expect_true(all(tags$tag=="input"))
})

test_that("param_tags returns tags", {
  ans <- param_tags(house())
  expect_is(ans, "data.frame")
  expect_equal(nrow(ans), 2)
  expect_identical(names(ans), c("name", "tag"))
  expect_identical(ans$tag[1], "covariates")
  
  code <- "$PARAM @tag foo\n A = 1\n$PARAM @input\nB = 2"
  mod <- mcode("cdn-6", code, compile = FALSE)
  ans <- param_tags(mod)
  expect_equal(ans$name, c("A", "B"))
  expect_equal(ans$tag, c("foo", "input"))
  
  mod <- mread("pk1", project = modlib(), compile = FALSE)
  ans <- param_tags(mod)
  expect_is(ans, "data.frame")
  expect_equal(nrow(ans), 0)
  expect_identical(names(ans), c("name", "tag"))
})
