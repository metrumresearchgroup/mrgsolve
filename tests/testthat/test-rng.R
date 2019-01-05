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

context("test-rng")

mod <- mrgsolve:::house(omega=diag(c(1,1,1,1)))

ident <- function(x,y,...) {
  identical(x,y) 
}

test_that("Different seeds give different results without call to set.seed()", {
  out1 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
  out2 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
  expect_false(ident(out1,out2))
})

test_that("Different seeds give different results with different calls to set.seed()", {
  set.seed(112)
  out1 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
  set.seed(333)
  out2 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
  expect_false(ident(out1,out2))
})

test_that("Same seeds give same results with call to set.seed()", {
  set.seed(112)
  out1 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
  set.seed(112)
  out2 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
  expect_true(ident(out1,out1))
})



