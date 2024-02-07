library(testthat)
library(mrgsolve)
library(dplyr)

local_edition(3)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

code <- '
$PARAM 
CL = 1/10, V = 20, KA = 1, Rate = 0, Cmt = 1
dose = 100, interval = 4*7, last = 10*112
FLAG  = 0

$PKMODEL cmt = "DEPOT,CENT", depot = TRUE

$PLUGIN evtools

$GLOBAL
evt::regimen reg;

$MAIN 
if(NEWIND <=1) {
  reg.init(self);
  reg.amt(dose);
  reg.ii(interval);
  reg.rate(Rate);
  reg.until(last);
  reg.cmt(Cmt);
  if(FLAG) reg.flagnext();
}

$ERROR
reg.execute();
capture CP = CENT/V;
capture cdose = reg.amt();
capture cinterval = reg.ii();
capture crate = reg.rate();
capture cuntil = reg.until();
capture ccmt = reg.cmt();
'

mod1 <- mcode("test-evtools-regimen", code, quiet = TRUE)
mod2 <- modlib("pk1", quiet = TRUE)
mod2 <- param(mod2, as.list(param(mod1)))

test_that("Identical results with pk1", {
  data <- ev(amt = mod1$dose, ii = mod1$interval, until = mod1$last)
  out1 <- mrgsim(mod1, end = 10*112)
  out2 <- mrgsim(mod2, data = data, end = 10*112, obsonly = TRUE)
  expect_identical(out1$CP, out2$CP)  
  expect_true(all(out1$cinterval==mod1$interval))
  expect_true(all(out1$cdose==mod1$dose))
  expect_true(all(out1$cuntil==mod1$last))
  expect_true(all(out1$crate==mod1$Rate))
  
  # Change up specifics, including infusion
  data <- ev(amt = 950, ii = 6, until = 72, rate = 300, cmt = 2)
  mod1 <- param(mod1, dose = 950, interval = 6, last = 72, Rate = 300, Cmt = 2)
  mod1 <- param(mod1, CL = 5)
  mod2 <- param(mod2, CL = 5)
  out1 <- mrgsim(mod1, end = 144)
  out2 <- mrgsim(mod2, end = 144, data = data, obsonly = TRUE)
  expect_identical(out1$CP, out2$CP)
  expect_true(all(out1$DEPOT==0))
  
  expect_true(all(out1$cinterval==mod1$interval))
  expect_true(all(out1$cdose==mod1$dose))
  expect_true(all(out1$cuntil==mod1$last))
  expect_true(all(out1$crate==mod1$Rate))
})

test_that("flag_next stops the simulation", {
  mod1 <- update(mod1, end = 10*112)
  out0 <- mrgsim(mod1, delta = 1)
  set.seed(1023)
  pick <- c(0, sort(sample(seq(10*112), size = 100)))
  out2 <- mrgsim(mod1, end = -1, add = pick, param = list(FLAG=1))
  out1 <- filter(out0, time %in% pick)
  out2 <- filter(out2, time %in% pick)
  expect_equal(out1$CP, out2$CP, tolerance = 1e-10)
})
