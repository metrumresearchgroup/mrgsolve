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

mod <- mrgsolve:::house()  

test_that("params are constructed", {
  x <- param(A = 1, B = 2)  
  expect_is(x, "parameter_list")
  x <- param(list(A = 1, B = 2))
  expect_is(x, "parameter_list")
  x <- param(c(A = 1, B = 2))
  expect_is(x, "parameter_list")
  expect_error(param(A = c(1,2)))
  expect_error(param(A = "B"))
  capture.output(expect_error(param(A.2 = 3)))
})

test_that("params are accessed", {
  expect_identical(mod$CL, unname(mod@param@data$CL))
  expect_identical(mod[["KA"]], unname(mod@param@data$KA))
})

test_that("params are updated", {
  mod <- mrgsolve:::house()
  mod2 <- param(mod, CL = 55, VC = 111)
  expect_equal(mod2$CL,55)
  expect_equal(mod2$VC,111)
  
  mod2 <- update(mod, param = list(CL=99, KA = 9))
  expect_equal(mod2$CL,99)
  expect_equal(mod2$KA,9)
  
  data <- data.frame(CL = 44, D1 = 999)
  mod2 <- param(mod, data)
  expect_equal(mod2$CL,44)
  expect_equal(mod2$D1,999)
  expect_equal(mod$VC,mod2$VC)
  
  expect_error(param(mod, KYLE = 2))
  expect_error(param(mod, KA = "A"))
})


