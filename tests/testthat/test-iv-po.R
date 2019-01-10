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


oral <- function(dose,par,time) {
  par <- as.list(par)
  par$K <- par$CL/par$VC
  with(par, {((KA*dose)/(VC*(KA-K)))*(exp(-K*time) - exp(-KA*time))})
}
iv <- function(dose,par,time) {
  par <- as.list(par)
  par$K <- par$CL/par$VC
  with(par, (dose/VC)*exp(-K*time))
  
}

context("test-iv-po")
mod <- mrgsolve:::house() %>% 
  update(atol = 1E-20, rtol = 1E-12, digits = 8)

out <- mrgsim(mod)
comparepo <- signif(oral(as.list(init(mod))$GUT, param(mod), stime(mod)), digits=8)

modiv <- mod %>% init(GUT=0, CENT=1000)
outiv <- mrgsim(modiv)
compareiv <- signif(iv(as.list(init(modiv))$CENT, param(modiv), stime(modiv)), digits=8)


test_that("Simulation output is of class mrgsims", {
  expect_is(out, "mrgsims")
})

test_that("The simulation model can be recovered from output", {
  expect_identical(mod, mrgsolve:::mod(out))
  expect_is(mrgsolve:::mod(out), "mrgmod")
})

test_that("CP from oral model is identical to closed form result", {
  expect_true(all(comparepo ==out$CP))
})

test_that("CP from iv model is identical to closed form result", {
  expect_true(all(compareiv == outiv$CP))
})

test_that("Error on dosing into non-existant compartment", {
  expect_error(mod %>% ev(amt = 100, cmt = 1000) %>% mrgsim())
})

