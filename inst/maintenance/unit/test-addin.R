# Copyright (C) 2013 - 2026  Metrum Research Group, LLC
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

test_that("Addin: convert nonmem adds semicolon", {
  code <- "$MAIN CL = 1"
  out <- mrgsolve:::apply_convert_nm(code, code)
  expect_length(out, 1)
  expect_equal(out, "$MAIN CL = 1;")

  code <- "CL = 1"
  out <- mrgsolve:::apply_convert_nm(code, code)
  expect_equal(out, "CL = 1;")

  code <- "$PARAM CL = 1"
  out <- mrgsolve:::apply_convert_nm(code, code)
  expect_identical(code, out)
})

test_that("Addin: convert pow", {
  code <- "$MAIN CL =  a ** b"
  out <- mrgsolve:::apply_convert_pow(code, code)
  expect_length(out, 1)
  expect_equal(out, "$MAIN CL = pow(a, b)")

  code <- "CL = a ** b"
  out <- mrgsolve:::apply_convert_pow(code, code)
  expect_equal(out, "CL = pow(a, b)")

  code <- "$PARAM CL = 1"
  out <- mrgsolve:::apply_convert_pow(code, code)
  expect_identical(code, out)
})

test_that("Addin: expected spacing after convert pow", {
  code <- "$MAIN CL=1 *   THETA(5) ** SEX * EXP(ETA(2))"
  out <- mrgsolve:::apply_convert_pow(code, code)
  expect_length(out, 1)
  expect_identical(out, "$MAIN CL = 1*pow(THETA(5), SEX)*EXP(ETA(2))")
})

test_that("Addin: convert nonmem converts IF/ELSE", {
  code <- "$MAIN  IF(A.EQ.2) THEN\nB = 3\nEND IF"
  out <- mrgsolve:::apply_convert_nm(code, code)
  expect_length(out, 1)
  expect_match(out, "if(A == 2) {", fixed = TRUE)
  expect_match(out, "B = 3;\n}", fixed = TRUE)
  
  code <- "$ODE\nIF(WT.EQ.70) THEN\nZ = 5\nELSE IF(WT.EQ.80) THEN\n Z = 50\nEND IF"
  out <- mrgsolve:::apply_convert_nm(code, code)
  expect_length(out, 1)
  expect_match(out, "if(WT == 70) {", fixed = TRUE)
  expect_match(out, "Z = 5;", fixed = TRUE)
  expect_match(out, "else if(WT == 80) {", fixed = TRUE)
  
})
