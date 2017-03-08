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

project <- file.path(system.file(package="mrgsolve"), "models")

context("Testing matlist operations")

code <- '
$PARAM CL=1
$INIT CENT=0
$ODE dxdt_CENT = 0;

$OMEGA 0 0 0 
$SIGMA 0 0 0
'

tmp <- tempdir()

mod <- mcode("test4",code)


mod <- mod %>% omat(diag(c(1.2, 2.3,3.4))) %>% smat(diag(c(0.1, 0.2, 0.3)))


test_that("Indexing OMEGA matrix elements", {
  expect_equivalent(as.matrix(omat(mod))[2,2],2.3)
  expect_equivalent(as.matrix(omat(mod))[2,1],0)
  expect_equivalent(as.matrix(omat(mod))[3,3],3.4)
})

test_that("Indexing SIGMA matrix elements", {
  expect_equivalent(as.matrix(smat(mod))[2,2],0.2)
  expect_equivalent(as.matrix(smat(mod))[2,1],0)
  expect_equivalent(as.matrix(smat(mod))[3,3],0.3)
})

o1 <- omat(diag(c(1.1, 2.2, 3.3)), diag(c(4.4, 5.5, 6.6)), matrix(seq(91,99),nrow=3, byrow=TRUE))
mat <- as.matrix(o1)
test_that("Indexing OMEGA matrix elements with multiple matrices", {
    expect_equivalent(mat[3,3],3.3)
    expect_equivalent(mat[7,2],0)
    expect_equivalent(mat[7,7],91)
    expect_equivalent(mat[9,8],98)
})







