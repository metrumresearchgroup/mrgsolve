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

context("Testing that plots can be generated from output objects")

mod <- mrgsolve:::house() %>% init(CENT=1000)
out <- mrgsim(mod)


# test_that("Plot from mrgsims without a formula", {
#   expect_is(plot(out), "trellis")
# })
# 
# test_that("Plot from mrgsims with a formula", {
#   expect_is(plot(out, CP~time), "trellis")
# })
# 
# 
# out <- knobs(mod, CL=c(1,2,3), VC=c(10,20))
# 
# test_that("Plot from batch_mrgsims without a formula", {
#   expect_is(plot(out), "trellis")
#   
# })
# 
# test_that("Plot from batch_mrgsims with a formula", {
#   expect_is(plot(out, CP~time), "trellis")
# })
