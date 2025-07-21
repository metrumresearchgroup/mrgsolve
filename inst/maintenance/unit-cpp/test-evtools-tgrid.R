library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

local_edition(3)

code <- '
$plugin evtools
$param evi = 0, xstart = 1, xend = 365*24*60, xdelta = 1
$param mode = 0, checktime = 12

$main 
if(NEWIND <= 1) int count = 0;

if(mode==0 && NEWIND <= 1) { 
  evt::ev obs = evt::tgrid(xstart, xend, xdelta);
  obs.evid = evi;
  self.push(obs);
}

if(mode==1 && TIME==checktime && EVID==0) { 
  evt::ev obs = evt::tgrid(xstart, xend, xdelta);
  obs.evid = evi;
  self.push(obs);
}

$table
if(EVID==evi) ++count;

$capture count
'

mod <- mcode("test-evtools-tgrid", code)

test_that("Insert an internal-only time grid with NEWIND", {
  
  n <- length(seq(mod$xstart, mod$xend, mod$xdelta))
  mod <- update(mod, end = -1, add = c(0, mod$xend + 3))
  
  # Count all EVID=0 records
  mod <- param(mod, evi = 0, mode = 0)
  out <- mrgsim(mod, recsort = 4)
  expect_equal(nrow(out), 2)
  expect_equal(out$count[2], n + length(stime(mod)))
  
  mod <- param(mod, evi = 999, mode = 0)
  out2 <- mrgsim(mod, recsort = 4)
  expect_equal(nrow(out2), 2)
  expect_equal(out2$count[2], n)
  
  mod <- param(mod, evi = 2, mode = 0)
  out3 <- mrgsim(mod)
  expect_equal(nrow(out3), 2)
  expect_equal(out3$count[2], n)
})

test_that("Insert internal-only time grid with odd delta", {
  mod <- param(mod, xdelta = 1.1)
  n <- length(seq(mod$xstart, mod$xend, mod$xdelta))
  mod <- update(mod, end = -1, add = c(0, mod$xend + 3))
  
  mod <- param(mod, evi = 999, mode = 0)
  out <- mrgsim(mod, recsort = 4)
  expect_equal(nrow(out), 2)
  expect_equal(out$count[2], n)
})

test_that("Insert internal-only time grid starting at other time", {
  mod <- param(mod, evi = 999, mode = 0, xstart = 12)
  n <- length(seq(mod$xstart, mod$xend, mod$xdelta))
  mod <- update(mod, end = -1, add = c(0, mod$xend + 13))
  out <- mrgsim(mod)
  expect_equal(out$count[2], n)
  
  mod <- param(mod, checktime = 12)
  mod <- param(mod, evi = 999, mode = 1, xstart = 12, xend = 15)
  n <- length(seq(mod$xstart, mod$xend, mod$xdelta))
  mod <- update(mod, end = -1, add = c(0, mod$checktime, mod$xend + 100))
  out <- mrgsim(mod)
  expect_equal(out$count[3], n)
  
  # Error to start in the past; we try to initiate this at `checktime`
  mod <- param(mod, evi = 999, mode = 1, xstart = 0)
  expect_error(mrgsim(mod), "cannot start in the past")
})
