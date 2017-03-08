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


project <- file.path(system.file(package="mrgsolve"), "models")

context("Test knobs")
mod <- mrgsolve:::house(atol=1E-20,rtol=1E-12,digits=8)

out <- knobs(mod %>% init(GUT=0), CL=c(1,2,3), foo=c(2,3,4),fooo=1, amt=c(100,200), cmt=1)
dfout <- as.data.frame(out)

test_that("knobs() returns object of class batch_mrgsims", {
  expect_is(out, "batch_mrgsims")
})

test_that("Moving knobs are correctly identified", {
  expect_identical(mrgsolve:::moving(out), c("CL", "amt"))
})


test_that("CL knob is correctly captured in output as CL", {
  expect_true(is.element("CL", names(out)))
  expect_identical(unique(dfout$CL),c(1,2,3))
})

test_that("A false knob does not appear in simulated output", {
  expect_false(is.element("foo", names(out)))
})
