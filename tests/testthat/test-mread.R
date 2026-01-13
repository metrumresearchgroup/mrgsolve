# Copyright (C) 2013 - 2019  Metrum Research Group
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

context("test-mread")

test_that("ETA(n) in $ODE is error", {
  code <- '$CMT A\n$OMEGA 1\n$ODE double a = ETA(1);'  
  expect_error(
    mcode(
      "test-mread-eta", 
      code, 
      quiet = TRUE, 
      compile = FALSE
    ), 
    regexp = "ETA(n) is not allowed in ODE", 
    fixed = TRUE
  )
})

test_that("Warning with no $CMT or $INIT", {
  code <- '$OMEGA 1\n$ODE double a = 2;'  
  expect_warning(
    mcode(
      "test-mread-cmt", 
      code, 
      compile = FALSE
    ), 
    regexp = "Could not find a $INIT or $CMT block", 
    fixed = TRUE
  )
})

test_that("read in rmd file", {
  mod <- mread("popex.Rmd", modlib(), compile = FALSE) 
  expect_is(mod, "mrgmod")
})

test_that("ERROR is alias for TABLE", {
  code <- "$ERROR double x=2;"
  expect_is(mcode("error-is-table", code), "mrgmod")
})

test_that("mread does not expand partial alias matches", {
  expect_warning(
    mcode(
      "test-mread-no-partial-alias",
      "$CMT A B\n$DESFOO\ndxdt_A  = 0;\ndxdt_B=0;",
    ),
    regexp = "invalid blocks found: DESFOO",
    fixed = TRUE
  )
})

test_that("mcode writes with newlines", {
  code <- '[CMT] A\n[ODE]\ndxdt_A = -0.1*A;'
  
  # Test with no split; always worked
  mod1 <- mcode("test-mcode-as-is", code, compile = FALSE)
  expect_is(mod1, "mrgmod")
  expect_length(capture.output(see(mod1)), 5)
  
  # Test after splitting; fails with previous behavior
  vcode <- unlist(strsplit(code, "\n"))
  mod2 <- mcode("test-mcode-with-newlines", vcode, compile = FALSE)
  expect_is(mod2, "mrgmod")
  expect_length(capture.output(see(mod2)), 5)
})
