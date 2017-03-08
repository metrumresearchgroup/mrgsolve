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

context("test-annot")

test_that("Testing modMATRIX", {
    expect_equal(dim(modMATRIX("  0 0 0   0")), c(4,4))
    expect_equal(dim(modMATRIX("  0\n 0\n 0   0")), c(4,4))
    expect_equal(dim(modMATRIX("  0 0 0   ", block=TRUE)), c(2,2))
    expect_error(modMATRIX("  0 0 0  0 ", block=TRUE))
    expect_equal(modMATRIX("0 0 0", use=FALSE), matrix(0,nrow=3,ncol=3))
})





