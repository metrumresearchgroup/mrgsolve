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

context("test-ev")

mod <- mrgsolve:::house()

test_that("dosing without ss", {
  e <- ev(amt = 100, ii = 24, addl = 3)
  d <- realize_addl(e)
  mod <- obsonly(mod) %>% update(end = 96, atol = 1E-20)
  out1 <- mrgsim(mod, events = e, recsort = 2)
  out2 <- mrgsim(mod, events = d)
  expect_true(identical(out1, out2))
})

test_that("dosing with ss", {
  e <- ev(amt = 100, ii = 24, addl = 3, ss = 1)
  d <- realize_addl(e)
  mod <- obsonly(mod) %>% update(end = 96, atol = 1E-20)
  out1 <- mrgsim(mod, events = e, recsort = 2)
  out2 <- mrgsim(mod, events = d)
  expect_true(identical(out1, out2))
})

