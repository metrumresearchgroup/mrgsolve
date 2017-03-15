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

context("Testing new model specification file")

code <- '
$PARAM CL=1, VC=20, KA=1.2
VMAX=2, KM=3

$INIT GUT=100, CENT=5

$SET
end=111, delta=1.1, hmax=10, maxsteps=22334

$GLOBAL
#define CP (CENT/VC)
double KE = 0;

$MAIN
KE = CL/VC;

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - KE*CENT;

$TABLE
double FLAG = 2;
double ETA1 = ETA(1);
double EPS1 = EPS(1);

$CAPTURE CP FLAG ETA1 EPS1

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

mod <- suppressWarnings(mcode("test2",code, warn=FALSE))

test_that("Parameters are parsed properly with mread", {
  expect_equal(param(mod)$CL,1)
  expect_equal(param(mod)$VC,20)
  expect_equal(param(mod)$KA,1.2)
  expect_equal(mrgsolve:::pars(mod), c("CL", "VC", "KA", "VMAX", "KM"))
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


set.seed(8282)

test_that("EPS values have proper variance", {
  out <- mrgsim(mod,end=100000, delta=1)
  expect_equal(round(var(out$EPS1),2),0.55)
})


code <- '
$PARAM CL=1, VC=20
$INIT  CENT=0

$GLOBAL 
double kyle=5;


$MAIN
double ke=CL/20;
double ke2 = CL/VC;

double a=5;
a = a/1;

bool TRUTH = true;

bool LIE = false;
LIE = false;

$ODE
double set_in_ode =1;
ke2=556.2;

$CAPTURE ke ke2 TRUTH set_in_ode
'

test_that("User-declared C++ variables are available globally", {
  obj <- try(suppressWarnings(mcode("test-mread-3",code, warn=FALSE)))
  expect_is(obj, "mrgmod")
})


test_that("Error when code is passed as project", {
  expect_error(suppressWarnings(mread("hey",code)))
})


test_that("Model name with spaces is error", {
    expect_error(mcode("ab cd", ""))
})

test_that("Error with duplicate blocks", {
  expect_error(mcode("a", "$ODE \n $ODE"))
  expect_error(mcode("a", "$MAIN \n $MAIN"))
  expect_error(mcode("a", "$SET \n $SET"))
})

