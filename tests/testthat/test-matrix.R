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

context("test-matrix")

test_that("Testing modMATRIX", {
    expect_equal(dim(modMATRIX("  0 0 0   0")), c(4,4))
    expect_equal(dim(modMATRIX("  0\n 0\n 0   0")), c(4,4))
    expect_equal(dim(modMATRIX("  0 0 0   ", block=TRUE)), c(2,2))
    expect_error(modMATRIX("  0 0 0  0 ", block=TRUE))
    expect_equal(modMATRIX("0 0 0", use=FALSE), matrix(0,nrow=3,ncol=3))
})

test_that("SUPERMATRIX", {
    ml <- list(matrix(1, 2, 2), matrix(3, 4, 4))
    dimnames(ml[[1]]) <- list(c("a", "b"), c("A", "B"))
    ans <- mrgsolve:::SUPERMATRIX(ml)
    expect_is(ans, "matrix")
    expect_equal(dim(ans), c(6, 6))
    ml$a <- "a"
    expect_error(mrgsolve:::SUPERMATRIX(ml), msg = "is not TRUE")
    expect_error(mrgsolve:::SUPERMATRIX(ml[[1]]), msg = "is not TRUE")
    ml$a <- NULL
    ans <- mrgsolve:::SUPERMATRIX(ml[2])
    expect_identical(unname(ml[[2]]),unname(ans))
    ans <- mrgsolve:::SUPERMATRIX(ml[1], keep_names = TRUE)
    expect_identical(ans, ml[[1]])
    ans1 <- mrgsolve:::SUPERMATRIX(list())
    expect_identical(ans1, matrix(0, nrow = 0, ncol = 0))
    ans2 <- mrgsolve:::SUPERMATRIX(omat()@data)
    expect_identical(ans1, ans2)
})
