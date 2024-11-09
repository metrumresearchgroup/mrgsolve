# Copyright (C) 2013 - 2024  Metrum Research Group
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

library(mrgsolve)
library(testthat)
library(dplyr)

code <- '
$MAIN
capture nrow = self.nrow;
capture rown = self.rown;
capture idn = self.idn;
capture nid = self.nid;
capture tlast = self.timelast;
'

mod <- mcode("test-self", code)

test_that("test timelast", {
  mod <- update(mod, end = 45)
  out <- mrgsim(mod)
  expect_true(all(out$tlast==45))

  data1 <- data.frame(ID = 1, time = seq(11), cmt = 0, evid = 0, amt = 0)
  data2 <- data.frame(ID = 2, time = seq(3), cmt = 0, evid = 0, amt = 0)
  data <- rbind(data1, data2)
  out <- mrgsim_df(mod, data)
  d <- unique(out[, c("ID", "tlast")])
  expect_equal(d$ID, c(1,2))
  expect_equal(d$tlast, c(11, 3))
})

test_that("test nrow and rown", {
  mod <- update(mod, end = 137)
  out <- mrgsim(mod)
  expect_true(all(out$nrow==138))
  expect_identical(out$rown, seq(nrow(out))-1)
})

test_that("test idn and nid", {
  id <- data.frame(ID = 1:33)
  out <- mrgsim_i(mod,id)
  out <- mutate(out, diff = ID - idn)
  expect_true(all(out$diff==1))
  expect_true(all(out$nid==33))
  expect_true(all(out$nrow==825))
})
