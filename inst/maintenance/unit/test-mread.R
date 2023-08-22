# Copyright (C) 2013 - 2022  Metrum Research Group
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

code <- '
$PARAM CL=1, VC=20
VMAX=2, KM=3

$INIT GUT=100, CENT=5

$SET
end=111, delta=1.1, hmax=10, maxsteps=22334

$GLOBAL
#define CP (CENT/VC)
double KE = 0;

$MAIN
KE = CL/VC;
double KA = 1.2;

$ODE
double set_in_ode = 1;

dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - KE*CENT;

$TABLE
double FLAG = 2;
double ETA1 = ETA(1);
double EPS1 = EPS(1);

$CAPTURE CP FLAG ETA1 EPS1 set_in_ode

$OMEGA @name A
1
2
3

$OMEGA  @name B @corr
0.1
0.5 0.2

$SIGMA
0.55

$SIGMA @block
0.1 0.002 0.3

'

mod <- mcode("test2",code, warn=TRUE)

test_that("Parameters are parsed properly with mread", {
  expect_equal(param(mod)$CL,1)
  expect_equal(param(mod)$VC,20)
  expect_equal(mrgsolve:::Pars(mod), c("CL", "VC", "VMAX", "KM"))
})

test_that("Compartments are parsed properly with mread", {
  expect_equal(init(mod)$GUT,100)
  expect_equal(init(mod)$CENT,5)
  expect_equal(mrgsolve:::cmt(mod), c("GUT", "CENT"))
})

test_that("Settings are parsed properly with mread", {
  expect_equal(mod@end,111)
  expect_equal(mod@delta,1.1)
  expect_equal(mod@hmax,10)
  expect_equal(mod@maxsteps,22334)  
})

test_that("mread output had class mrgmod", {
  expect_is(mod, "mrgmod")
})

test_that("Tabled items are in simulated data", {
  out <- mrgsim(mod)
  expect_true("CP" %in% names(out))
  expect_true(all(out$FLAG==2))
})

test_that("Omega matrices are properly parsed", {
  mat <- as.matrix(omat(mod))
  expect_equivalent(mat[2,2],2)
  expect_equivalent(mat[4,4],0.1)
  expect_equivalent(mat[5,5],0.2)
  expect_equivalent(signif(mat[5,4],4),signif(0.07071068,4))
})

test_that("Sigma matrices are properly parsed", {
  mat <- as.matrix(smat(mod))
  expect_equivalent(mat[1,1],0.55)
  expect_equivalent(mat[3,3],0.3)
  expect_equivalent(mat[3,2],0.002)
})

test_that("EPS values have proper variance", {
  set.seed(8282)
  out <- mrgsim(mod,end=100000, delta=1, init = list(GUT = 0, CENT = 0))
  expect_equal(round(var(out$EPS1),2),0.55)
})

test_that("New EPS draws for records at the same time", {
  data <- data.frame(ID = 1, TIME = c(0, 1, 1, 2), EVID = 0, CMT = 1)  
  out <- mrgsim(mod, data)
  expect_length(unique(out$EPS1), nrow(data))
})

test_that("Error when code is passed as project", {
  expect_error(suppressWarnings(mread("hey",code)))
})

test_that("Model name with spaces is error", {
  expect_error(mcode("ab cd", ""))
})

test_that("Error with duplicate blocks", {
  expect_error(mcode("a", "$MAIN \n $MAIN",compile = FALSE))
  expect_error(mcode("a", "$SET \n $SET",compile = FALSE))
})

test_that("Recover data when compile fails", {
  code <- '[main] double a = 2\n[param] b = 5\n'
  expect_warning(
    mod <- mcode("fail", code, recover = TRUE), 
    regexp = "returning object for debugging purposes only"
  )
  expect_is(mod, "list")
  expect_named(mod)
  expect_true("mod" %in% names(mod))
  expect_true("build" %in% names(mod))
  expect_true("out" %in% names(mod))
  expect_true("spec" %in% names(mod))
  recov <- mrgsolve:::build_format_recover(mod)
  expect_is(recov, "character")
  recov_list <- yaml::yaml.load(recov)
  expect_is(recov_list, "list")
})
