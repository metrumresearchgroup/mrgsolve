
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
  mod <- mcode("test-paramset", code, preclean = TRUE, delta = 1)

  # Overriding the model's live default for FLAG has no effect on what
  # `p.reset()` resets FLAG to -- `paramset` bakes in the defaults from
  # the $PARAM block at compile time.
  mod <- param(mod, FLAG = 0)

  data <- data.frame(amt = 0, evid = 0, FLAG = 1, time = seq(0,10), cmt = 0, ID = 1)

  # FLAG is also time-varying in the data, independent of both the
  # compiled-in default (5) and the param() override (0) above.
  data$FLAG <- ifelse(data$time > 5, 20, 10)

  out <- mrgsim_df(mod, data)

  expect_equal(
    out$flag,
    c(5, 5, 10, 10, 12, 12, 12, 12, 5, 5, 5)
  )
})
