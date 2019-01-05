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

context("test-rcpp_globals")

test_that("rcpp_globals", {
  pream <- c("global CharacterVector x = asdfasdf;", "Rcpp::IntegerVector z = 123;")
  spec <- list(PREAMBLE = pream)
  x <- mrgsolve:::global_rcpp(spec)
  expect_equal(x$PREAMBLE[1], "x = asdfasdf;")
  expect_equal(x$PREAMBLE[2], "Rcpp::IntegerVector z = 123;")
  expect_equal(length(x$GLOBAL), 1);
  expect_true(grepl("Rcpp::CharacterVector x;", x$GLOBAL, fixed = TRUE))
  expect_false(grepl("Rcpp::IntegerVector x;", x$GLOBAL, fixed = TRUE))
})
