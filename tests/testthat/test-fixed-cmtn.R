# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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


code <- '

$CMTN DEPOT

$PARAM CL=1, VC=20

$INIT CENT=0 , DEPOT=0

$FIXED A=1.1, B=2.2

$PARAM KM = 2, VMAX=100

$FIXED  
C = 3.3, D=4.4, E = 5.5

$MAIN double Z = A+B+C+D+E;

$TABLE double cmtn = N_DEPOT;
$CAPTURE cmtn
'


mod <- try(suppressWarnings(mcode("FOO",code, audit=FALSE)))

out <- mrgsim(mod)

context("CMTN block gives compartment numbers")

test_that("Model compiles with FIXED and CMTN",{
  expect_is(mod, "mrgmod")
  expect_true(all(out$cmtn==2))
})

context("Fixed parameters")

test_that("FIXED items are excluded from param", {
    expect_identical(names(param(mod)),c("CL", "VC", "KM", "VMAX"))
    expect_identical(names(mod@fixed),c("A", "B", "C", "D", "E"))
})

test_that("FIXED items can be recovered", {
    expect_identical(names(as.list(allparam(mod))),c("CL", "VC", "KM", "VMAX","A","B","C","D","E"))
})

