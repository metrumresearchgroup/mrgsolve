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


code <- '
$PARAM CL=1, FORM=1,F1 = 0.1

$MAIN

F_CENT = 1;
if(FORM==2) F_CENT = F1;

$CMT CENT
$ODE dxdt_CENT = -(CL/10)*CENT;
$CAPTURE CL FORM  F1
'

mod <- mcode("bioav1",code) %>% carry_out(evid)


context("test-bioav")

test_that("Bioav test with doses at time=0", {
  data <- expand.ev(ID=1:2,amt=100,FORM=c(1,2))
  out <- mod %>% data_set(data) %>%
    mrgsim() %>% filter(evid==1)
  expect_equal(c(100,100,10,10),out$CENT)
  expect_equal(rep(0,4),out$time)
  expect_equal(c(1,1,2,2),out$FORM)
})

