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



# 
# project <- file.path(system.file(package="mrgsolve"), "models")
# 
# mod <- mread("firstmodel", project)
# 
# context("Testing error generation ")
# 
# test_that("Error when parameters and compartments have same name", {
#   expect_error(mrgmod("firstmodel", project, param=list(CENT=2)))
#   expect_error(mrgmod("firstmodel", project, init=list(CL=2)))
# })
# 
# test_that("Error when parameters and compartments have same name introduced via update", {
#   expect_error(update(mod, param=list(CENT=2), open=TRUE))
#   expect_error(update(mod, init=list(CL=2), open=TRUE))
# })
# 
# test_that("Error when a parameter name is listed in compartments", {
#   expect_error(update(mod, init=list(CL=2), open=TRUE))
#   expect_error(mrgmod("firstmodel", project, init=list(CL=2)))
# })
# 
# test_that("Error  when a compartment names is listed in parameters", {
#   expect_error(update(mod, param=list(CENT=2), open=TRUE))
#   expect_error(mrgmod("firstmodel", project, param=list(CENT=2)))
# })
# 
# 
# test_that("Error when the number of parameters changes without recompile",{
#   bad <- update(mod, param=list(FAKEPARAMETER=22), open=TRUE)
#   expect_error(mrgsim(bad))
# })
# 
