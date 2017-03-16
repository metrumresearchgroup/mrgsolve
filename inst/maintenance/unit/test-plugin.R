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


code <- '
$PLUGIN Rcpp

$MAIN
double a = R::rnorm(10,1);
double b = R::rbeta(1,1);
double c = R::rexp(0.5);
double d = R::rpois(0.6);
double e = R::rbinom(1,0.23);
double f = R::runif(25,70);
double g = R::rweibull(1,1.5);

$CAPTURE a b c d e f g

'

context("PLUGIN: Rcpp")

test_that("Rcpp all distributions", {
  mod <- mcode("test_plugin-1", code, warn=FALSE)
  out <- mod %>% mrgsim(end=1000)
  expect_true(all(out$f > 25 & out$f < 70))
  expect_true(all(out$b > 0 & out$b < 1))
  expect_true((mean(out$a) > 8) & (mean(out$a) < 11))

})



code <- '
$OMEGA 1 1 3

$MAIN


if(NEWIND <= 1) {

  double a = ETA(1);
  double b = ETA(2);

  int i = 0;

  while(fabs(b) > 1 && i < 100) {
    simeta();
    b = ETA(2);
    i++;
  }
}

double c = ETA(3);

$CAPTURE a b c
'


context("PLUGIN: simeta")

test_that("resimulate ETAs", {
    mod <- mcode("test_plugin-2", code)
    out <- mod %>% mrgsim(end=-1, nid=1000)
    expect_true(var(out$a) > var(out$b))
    expect_true(var(out$c) > var(out$a))
    expect_true(min(out$b) >= -1 & max(out$b) <= 1)
})


