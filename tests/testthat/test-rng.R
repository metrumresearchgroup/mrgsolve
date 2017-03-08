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

context("R RNG respected via set.seed()")
mod <- mrgsolve:::house(omega=diag(c(1,1,1,1)))

out1 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
out2 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
set.seed(333)
out3 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
set.seed(333)
out4 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))

out5 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20),seed=555)
out6 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20),seed=555)




ident <- function(x,y,...) {
  x@date <- y@date <- date()
  identical(x,y) 
}

test_that("Runs with different seeds give different results without call to set.seed()", {
  expect_false(ident(out1,out2))
})

test_that("Runs with different seeds give different results with different calls to set.seed()", {
  expect_false(ident(out3,out5))
})
test_that("Runs with same seeds give same results with call to set.seed()", {
  expect_true(ident(out3,out4))
})
test_that("Runs with same seeds give same results when seed passed to mrgsim()", {
  expect_true(ident(out5,out6))
})


