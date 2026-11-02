
library(testthat)
library(mrgsolve)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-paramset")

code <- '
$PLUGIN paramset

$GLOBAL
paramset p;

$PARAM
WT  = 70,  FLAG = 5
AGE = 35,  FOO = 0
BAR = 100

$MAIN
if(NEWIND <= 1) p.reset();

if(TIME==2) p.update(_THETA_);

if(TIME==4) p.FLAG = 12;

if(TIME == 8) p.reset();

$ERROR
capture flag = p.FLAG;
'

test_that("paramset plugin: reset, update, and manual assignment", {
  mod <- mcode("test-paramset", code, preclean = TRUE)

  # Overriding the model's live default for FLAG has no effect on what
  # `p.reset()` resets FLAG to; `paramset` bakes in the defaults from
  # the $PARAM block at compile time.
  mod <- param(mod, FLAG = 0)

  data <- data.frame(amt = 0, evid = 0, FLAG = 1, time = seq(0,10), cmt = 0, ID = 1)

  # FLAG is time-varying in the data.
  data$FLAG <- ifelse(data$time > 5, 20, 10)

  out <- mrgsim_df(mod, data)

  expect_equal(out$time, seq(0, 10))
  expect_equal(
    out$flag,
    c(5, 5, 10, 10, 12, 12, 12, 12, 5, 5, 5)
  )
})

test_that("paramset plugin: param passed to mread is reflected at init", {
  # Unlike `param(mod, FLAG = 0)` after the fact, a `param`
  # override passed into mread or mcode is applied before the model is built,
  # so it is baked into paramset's defaults and shows up every time
  # `p.reset()` runs.
  mod <- mcode(
    "test-paramset-mread-param", code,
    preclean = TRUE,
    param = list(FLAG = 100)
  )

  data <- data.frame(amt = 0, evid = 0, time = seq(0,10), cmt = 0, ID = 1)

  out <- mrgsim_df(mod, data)

  expect_equal(out$time, seq(0, 10))
  expect_equal(
    out$flag,
    c(100, 100, 100, 100, 12, 12, 12, 12, 100, 100, 100)
  )
})

test_that("paramset plugin: p.reset() on NEWIND <= 1 resets for each new ID", {
  # `p.reset()` prevents carryover from previous ID.
  mod <- mcode("test-paramset-multi-id", code, preclean = TRUE)

  data <- expand.ev(amt = 0, evid = 0, cmt = 0, ID = c(1,2))
  data <- expand_observations(data, 0:5, unique = TRUE)

  out <- mrgsim_df(mod, data)

  expect_equal(out$time, rep(0:5, times = 2))

  out5 <- subset(out, time==5)
  expect_equal(out5$flag, c(12,12))

  out0 <- subset(out, time==0)
  expect_equal(out0$flag, c(5,5))
})

ops_header <- '
#ifndef MRGSOLVE_PARAMSET_OPS_H
#define MRGSOLVE_PARAMSET_OPS_H

void use_paramset(paramset& p, double time, const dvec& theta) {
  if(time == 0) p.reset();
  if(time == 2) p.update(theta);
  if(time == 4) p.FLAG = 12;
  if(time == 8) p.reset();
}

#endif
'

hfile <- file.path(tempdir(), "paramset-ops.h")
writeLines(ops_header, con = hfile)

code_include <- '
$PLUGIN paramset

$INCLUDE paramset-ops.h

$GLOBAL
paramset p;

$PARAM
WT  = 70,  FLAG = 5
AGE = 35,  FOO = 0
BAR = 100

$MAIN
use_paramset(p, TIME, _THETA_);

$ERROR
capture flag = p.FLAG;
'

test_that("paramset plugin: operate on paramset via a function in an included header", {
  mod <- mcode("test-paramset-include", code_include, preclean = TRUE)

  mod <- param(mod, FLAG = 0)

  data <- data.frame(amt = 0, evid = 0, FLAG = 1, time = seq(0,10), cmt = 0, ID = 1)
  data$FLAG <- ifelse(data$time > 5, 20, 10)

  out <- mrgsim_df(mod, data)

  expect_equal(out$time, seq(0, 10))
  expect_equal(
    out$flag,
    c(5, 5, 10, 10, 12, 12, 12, 12, 5, 5, 5)
  )
})
