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

context("test-mevent")

code <- '
$PARAM CL = 1, V = 20, KA1 = 0.1, KA2 = 2
$PKMODEL cmt = "GUT CENT", depot = TRUE
$PREAMBLE capture KA = KA1;
$MAIN
if(EVID==1) {
  self.mevent(TIME + 2, 33);
  KA = KA1;
}
if(self.evid==33) {  
  KA = KA2;
}

'
test_that("mevent - time-varying KA", {
  mod <- mcode("mevent", code)
  expect_is(mod, "mrgmod")
  out <- mod %>% ev(amt = 100) %>% mrgsim()
  expect_is(out, "mrgsims")
  expect_equal(out$KA[1],0.1)
  expect_equal(out$KA[nrow(out)], 2)
})


code <- '
$SET end = 72
$PARAM mode = 0
$MAIN

if(mode==1) {
  self.mevent(2.1,33);
}
if(mode==2) {
  double mt = self.mtime(2.2);
}
'

test_that("Unprotected mevent or mtime code runs", {
  mod <- mcode("unprotected-mevent-mtime", code)  
  
  out0 <- mrgsim(mod)
  expect_is(out0, "mrgsims")
  
  out1 <- mrgsim(mod, param = list(mode = 1))
  expect_is(out1, "mrgsims")

  out2 <- mrgsim(mod, param = list(mode = 2))
  expect_is(out2, "mrgsims")
})
