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

context("test-mrgsims")

test_that("mrgsims class", {
  mod <- mrgsolve::house()   
  out <- mrgsim(mod)
  expect_true(is.mrgsims(out))
  expect_is(as.list(out), "mrgsims_list")
})

test_that("plot from character", {
  out <- mrgsolve::house() %>% mrgsim()
  pl1 <- out %>%  plot(" CENT DV, CP")
  pl2 <- out %>% plot(CENT+DV+CP~time)
  expect_equal(pl1,pl2)
})

