# # Copyright (C) 2013 - 2019  Metrum Research Group
# #
# # This file is part of mrgsolve.
# #
# # mrgsolve is free software: you can redistribute it and/or modify it
# # under the terms of the GNU General Public License as published by
# # the Free Software Foundation, either version 2 of the License, or
# # (at your option) any later version.
# #
# # mrgsolve is distributed in the hope that it will be useful, but
# # WITHOUT ANY WARRANTY; without even the implied warranty of
# # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# # GNU General Public License for more details.
# #
# # You should have received a copy of the GNU General Public License
# # along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.
# 
library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-request")

code <- '
$PARAM CL=1, V2=20,Q=30,V3=200,KA=1
$CMT GUT CENT
$INIT PERIPH=2
$PKMODEL ncmt=2, depot=TRUE

$MAIN
double a = 1;
double b = 2;
double z = 3;

$CAPTURE b  z
'

mod <- mcode("req1", code)

#
test_that("Req gets the right variables", {
  x1 <- names(mod %>% mrgsim)
  x2 <- names(mod %>% Req(PERIPH,GUT) %>% mrgsim)
  x3 <- names(mod %>% Req(PERIPH,b) %>% mrgsim)
  x4 <- names(mod %>% Req(b,z) %>% mrgsim)
  expect_identical(x1,s_(ID,time,GUT,CENT,PERIPH,b,z))
  expect_identical(x2,s_(ID,time,GUT,PERIPH))
  expect_identical(x3,s_(ID,time,PERIPH,b))
  expect_identical(x4,s_(ID,time,b,z))
})

test_that("Req with rename", {
  x2 <- names(mod %>% Req(R1 = PERIPH,R2 = GUT) %>% mrgsim)
  expect_identical(x2,s_(ID,time,R2, R1))
  expect_error(mod %>% Req(R1 = PERIPH, R1 = GUT) %>% mrgsim)
})

test_that("request with rename", {
  mod2 <- update(mod, req = "R1 = PERIPH, R2 = GUT")
  x2 <- names(mrgsim(mod2))
  expect_identical(x2,s_(ID,time,R2, R1,b,z))
  expect_error(update(mod, req="R1 = PERIPH, R1 = GUT"))
})

test_that("Req gets the right variables, with request", {
  mod <- update(mod, request="CENT")
  x1 <- names(mod %>% mrgsim)
  x2 <- names(mod %>% Req(PERIPH,GUT) %>% mrgsim)
  x3 <- names(mod %>% Req(PERIPH,b) %>% mrgsim)
  x4 <- names(mod %>% Req(z,b) %>% mrgsim)
  expect_identical(x1,s_(ID,time,CENT,b,z))
  expect_identical(x2,s_(ID,time,GUT,PERIPH))
  expect_identical(x3,s_(ID,time,PERIPH,b))
  expect_identical(x4,s_(ID,time,b,z))
})

context("Testing various request settings")

code <- '
$PARAM CL=1
$INIT GUT=100, CENT=5, PERIPH=3


$ODE
dxdt_GUT = 0;
dxdt_CENT = 0;
dxdt_PERIPH = 0;

$TABLE
double FLAG = 2;
double ETA1 = 1.1;
double CP = 1;
double EPS1 = 1.2;

$CAPTURE CP FLAG ETA1 EPS1
'

mod <- suppressWarnings(mcode("test3tga", code))

test_that("Testing request setting", {
  out <- mrgsim(mod, request="PERIPH,CENT", end = 1)
  out2 <- mrgsim(update(mod, request="CENT,PERIPH,GUT"),end=1)
  
  expect_equal(names(out),c("ID", "time","CENT","PERIPH","CP", "FLAG","ETA1", "EPS1"))
  expect_equal(names(out2),c("ID", "time", "GUT","CENT","PERIPH","CP","FLAG","ETA1", "EPS1"))
})

code <- '
$PARAM CL=1
$INIT GUT=100, CENT=5, PERIPH=3
$SET
req="CENT"

$ODE
dxdt_GUT = 0;
dxdt_CENT = 0;
dxdt_PERIPH = 0;

$TABLE
double CP = 1;
double FLAG = 2;
double ETA1 = ETA(1);
double EPS1 = EPS(1);
'

test_that("request is made in SET block", {
  mod <- suppressWarnings(mcode("test3bqea",code))
  cols <- names(mrgsim(mod))
  expect_identical(mod@request, "CENT")
  expect_identical(update(mod, req=c("PERIPH", "GUT"))@request, c("PERIPH", "GUT"))
  expect_identical(update(mod, req="PERIPH,GUT")@request, "PERIPH,GUT")
  expect_identical(intersect(cols,mrgsolve:::cmt(mod)), "CENT")
})

test_that("request is (all) by default", {
  mod <- mcode("test3c",'$CMT CENT\n$PARAM CL=1', compile=FALSE)
  expect_identical(mod@request, "(all)")
})

test_that("Typedef capture", {
  code <- '
  $MAIN
  capture a = 1;
  double aa = 21;
  capture b = 2;

  $CMT CM_T

  $ODE
  int c = 3;
  double cc = 33;
  dxdt_CM_T = 0;

  $TABLE
  double dd = 44;
  capture d = 4;
  capture capture_y = 1234;
  double capture_n = 999;
  '
  
  mod <- mcode("test3dbbae", code)
  
  out <- mod %>% mrgsim(end=3)
  
  expect_true(all(out$a == 1))
  expect_true(all(out$b == 2))
  expect_true(all(out$d == 4))
  expect_false("capture_n" %in% names(out))
  
  code <- '
  $CMT CM_T
  $ODE
  capture c = 3;
  double cc = 33;
  dxdt_CM_T = 0;
  '
  expect_silent(mod <- mcode("test3ewerw", code))
  
})


