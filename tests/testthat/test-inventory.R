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


context("test-inventory")

# test_that("inven_report displays warns and missing items", {
#   obj <- data.frame(KA = 1, CL = 1, V = 1, OCC = 1, F = 1)
#   inven_report <- mrgsolve:::inven_report
#   expect_equal(inven_report(obj, c("KA", "CL")), vector(mode = "character"))
#   expect_equal(inven_report(obj, c("KA", "OCC2")), c("OCC2"))
#   expect_warning(inven_report(obj, c("KA", "OCC2")), "The object is missing these parameters:\\n - OCC2")
# })


mod <- mcode("test", "$PARAM KA = 1, CL = 1, V = 1, OCC1 = 1, OCC2 = 1, F = 1",compile = FALSE)
all_obj <- data.frame(KA = 1, CL = 1, V = 1, OCC1 = 1, OCC2 = 1, F = 1)
missing_obj <- data.frame(KA = 1, CL = 1, V = 1, OCC1 = 1, F = 1)

test_that("inventory works", {
  # no requirements gives back model 
  expect_s4_class(inventory(mod, all_obj), "mrgmod")
  
  # everything says it found all and returns original model 
  expect_message(inventory(mod, all_obj, dplyr::everything()), "Found all required parameters")
  expect_s4_class(inventory(mod, all_obj, dplyr::everything()), "mrgmod")
  
})

test_that("inventory errors when missing required params", {

  expect_error(inventory(mod, missing_obj, dplyr::everything()))
  expect_error(inventory(mod, missing_obj, dplyr::contains("OCC")))
  expect_error(inventory(mod, missing_obj, V:F))
  expect_error(inventory(mod, missing_obj, OCC2))

})
