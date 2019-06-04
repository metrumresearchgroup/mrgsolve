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

context("test-mread")

test_that("ETA(n) in $ODE is error", {
  code <- '$OMEGA 1\n$ODE double a = ETA(1);'  
  expect_error(mcode("test-mread-eta", code, compile = FALSE))
})

test_that("Warning with no $CMT or $INIT", {
  code <- '$OMEGA 1\n$ODE double a = 2;'  
  expect_warning(mcode("test-mread-cmt", code,quiet=FALSE,compile=FALSE))
})

