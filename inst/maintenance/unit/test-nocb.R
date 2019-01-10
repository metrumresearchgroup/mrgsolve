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

context("test-nocb")

code <- '
$PARAM ke = 0
$ODE
dxdt_foo = -ke*foo;

$CAPTURE ke
$CMT foo
'

e1 <- ev(amt = 0, evid = 2, ke = 0E-12)
e2 <- ev(amt = 100, ke = 0E-12)
e3 <- ev(amt = 0, evid = 1, ke = 1, time = 20)
e4 <- as.ev(data.frame(amt = 0, evid = 2, ke = 1, time = c(21,22,23), cmt = 1))
e <- as.data.frame(c(e1,e2, e3, e4)) %>% mutate(ID = 1)

mod <- mcode("mypk",code)

test_that("simulation with nocb", {
  out <- mod %>% mrgsim(data = e, nocb = TRUE, end = -1)
  expect_true(out$foo[4] < 1)
  expect_true(out$foo[3] == 100)
})

test_that("simulation with locf", {
  out <- mod %>% mrgsim(data = e, nocb = FALSE, end = -1)
  expect_true(out$foo[4] == 100)
  expect_true(out$foo[5] < 50)
})

