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

context("Test opts")

test_that("Options where they don't belong", {
  code <- '
  $PARAM CL = 1
  $ODE >> annotated=TRUE
  dxdt_CENT = 0;
  $CMT CENT
  $TABLE >> zip=55455
  double a = 2;
  $SET y = TRUE
  $CMTN CENT
  '
  mod <- mcode("test-opts-1", code)
  expect_is(mod, "mrgmod")
  
})


test_that("Scrape and call", {
  code <- '
  >> d = 2
  CL=1, V=2, KA=3
  '
  e <- mrgsolve:::parse_env(1)
  
  code <- trimws(unlist(strsplit(code, "\n")))
  
  code <- structure(code[code!=""], pos=1)
  
  x <- mrgsolve:::scrape_and_call(code,e,narrow=TRUE,
                                  mrgsolve:::PARAM)
  
  expect_identical(e$param[[1]], list(CL=1, V=2, KA=3))

})
# 
# code <- '
#   >> d = 2, block=TRUE
#    1 
#    2 3 
#    4 5 6
#   '
# e <- mrgsolve:::parse_env(1)
# 
# code <- trimws(unlist(strsplit(code, "\n")))
# 
# code <- structure(code[code!=""], pos=1)
# 
# mrgsolve:::scrape_and_call(code,e,
#                 pass="specMATRIX",
#                 def=list(oclass="omegalist",type="omega"),
#                 split=FALSE,all=TRUE,narrow=FALSE)



