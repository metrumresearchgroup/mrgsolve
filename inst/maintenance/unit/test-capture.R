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

context("test-request")

code <- '
$PARAM CL=1, V2=20,Q=30,V3=200,KA=1
$MAIN  capture b = 2;
$OMEGA 1
$CAPTURE CL VP = V2, ETA(1) ka = KA
'

mod <- mcode("test-capture", code)

test_that("Renamed captured items are properly named", {
  out <- mrgsim(mod, carry_out = "evid")
  expect_identical(
    names(out), 
    c("ID", "time", "evid", "CL", "VP", "ETA_1", "ka", "b")
  )
})

