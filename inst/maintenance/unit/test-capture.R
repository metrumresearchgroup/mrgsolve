# Copyright (C) 2013 - 2019  Metrum Research Group
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

context("test-capture")

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

test_that("error if cmt in capture issue-555", {
  code <- "$CMT A\n$CAPTURE A"
  expect_error(mcode("cmt-in-capture",code,compile=FALSE), 
               "compartment should not be in")
  code <- "$CMT A\n$CAPTURE B=A"
  expect_is(mcode("rename-cmt-in-capture",code,compile=FALSE),"mrgmod")
})

code <- '
$PARAM CL=1, V2=20,Q=30,V3=200,KA=1
$GLOBAL
double z = 5;
$MAIN  double b = 2;
$OMEGA 1
$OMEGA @labels OGA2
2
$CAPTURE CL VP = V2
'

test_that("capture via mread [SLV-TEST-0008]", {
  mod <- mcode("capture-mread", code, capture = "Q,a=b,OGA2,z") 
  out <- outvars(mod)
  expect_equal(out$capture, c("CL", "VP", "Q", "a", "OGA2", "z"))
  expect_error(
    mread("pk1", modlib(), capture = "mrgsolve"),
    msg = "all requested `capture` variables must exist in the model"
  )
  mod <- mcode("capture-mread", code, capture="(everything)", compile = FALSE)
  res <- c("CL","VP", "Q", "V3", "KA", "OGA2", "ETA_1", "ETA_2", "z", "b")
  expect_equal(outvars(mod)$capture, res)
})

test_that("dynamic capture under nm-vars [SLV-TEST-0009]", {
  code <- '
  $plugin nm-vars
  $cmt @number 1
  $main F1 = 1.23;
  '
  mod <- mcode(
    "dynamic-capture-f1", 
    code, 
    compile = FALSE, 
    capture = "F1"
  )
  expect_is(mod, "mrgmod")
  expect_equal(mod@capture, c(F1 = "F1"))
})

test_that("capture pp directive via mread [SLV-TEST-0010]", {
  mod <- modlib("irm3", capture = "STIM", compile = FALSE)  
  expect_equal(outvars(mod)$capture, c("CP", "STIM"))
})
