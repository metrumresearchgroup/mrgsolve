# Copyright (C) 2013 - 2020  Metrum Research Group
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

context("test-solver")

test_that("ode variables are initialized issue-613", {
  code <- '
[cmt] CENT

[ ode ] 
double number = 2;
double CP = CENT/10;
dxdt_CENT  = -0.1*CENT;

[ capture ] CP number
'
  mod <- mcode("issue-613", code)
  dose <- ev(amt = 100) %>% ev_rep(ID = 1:4)
  out1 <- mrgsim(mod,dose)
  out2 <- mrgsim(mod,dose)
  expect_identical(out1,out2)
  expect_true(all(out1$number==2))
})


