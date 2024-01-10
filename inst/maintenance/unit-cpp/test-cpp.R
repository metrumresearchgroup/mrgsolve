
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-cpp")

code <- '
[ plugin ] nm-vars, mrgx, Rcpp

[ cmt ] @number 1

[ des ]  
DADT(1) = -0.1 * A(1);
'

test_that("build a model with mrgx and nm-vars", {
  expect_is(
    mcode("test-cpp-mrgx-nm-vars", code, quiet = TRUE), 
    "mrgmod"
  )
})

code <- '
$INCLUDE include-1.h
$CMT @number 4
$MAIN capture cmt = include1::getcmt(self);
'
incl <- '
namespace include1 {int getcmt(databox& self) {return self.cmt; }}
'

cfile <- file.path(tempdir(), "get-cmt.cpp")
hfile <- file.path(tempdir(), "include-1.h")
writeLines(code, con = cfile)
writeLines(incl, con = hfile)

test_that("included file understands databox", {
  mod <- mread(cfile)  
  data <- expand.ev(amt = 100, cmt = c(1,2,3,4))
  out <- mrgsim(mod, data, end = -1)
  expect_equal(out$cmt, c(1,2,3,4))
})

code <- '
$PK
if(NEWIND <= 1) capture FLAG = 0;
$CMT A
$TABLE 
if(TIME==2) self.mevent(2.001,33);
if(EVID==33) ++FLAG;
'

test_that("ev history is reset", {
  mod <- mcode("test-cpp-re-event", code, end = -1, add = c(1,2,3))
  out <- mrgsim_df(mod, ev(amt = 0, ID = 1:2, evid = 2)) 
  expect_equal(sum(out$FLAG), 2)  
  out3 <- subset(out, time==3)
  expect_length(out3$FLAG, 2)
  expect_true(all(out3$FLAG==1))
})

code <- '
$PARAM CHECK = 1
$CMT A
$TABLE 
if(TIME==1) {
  mrg::evdata ev(1.1, 1); 
  ev.amt = 100; 
  ev.check_unique = CHECK;
  self.mevector.push_back(ev);
  self.mevector.push_back(ev);
  self.mevector.push_back(ev);
}
'

test_that("control multiple doses", {
  mod <- mcode("test-cpp-multi-dose", code, end = 2)
  out <- mrgsim_df(mod) 
  expect_equal(max(out$A), 100)
  
  out <- mrgsim_df(mod, param = list(CHECK = 0))
  expect_equal(max(out$A), 300)
})

# Test events from within the model
code <- '
$PARAM r = 0, l = 0, d = 5, n = 0, t = 0, dose = 100
$MAIN
ALAG_A = l;
D_A = d;
if(TIME==0 && EVID==0) {
  mrg::evdata ev(t, 1); 
  ev.amt = dose; 
  ev.now = n==1;
  ev.rate = r;
  self.mevector.push_back(ev);
}
if(TIME >= 3) {
  F_A = 0.5;
}
$CMT A
$DES dxdt_A = -0.1*A;
'

mod <- mcode("test-ev-in-model", code, delta = 0.1, end = 12)

b <- mrgsim(mod)

test_that("mev bolus", {
  expect_equal(b$A[1],0)
  expect_true(b$A[2] > 99 && b$A[2] <= 100)
})

test_that("mev bolus now", {
  a <- mrgsim(mod, param = list(n = 1))  
  expect_equal(a$A[1],100)
  expect_true(a$A[2] > 99 && a$A[2] <= 100)
  expect_identical(a$A[-1], b$A[-1])
})

test_that("mev bolus later", {
  a <- mrgsim(mod, param = list(t = 2))  
  a <- filter_sims(a, time >= 2)
  expect_equal(a$A, b$A[seq(nrow(a))])
})

test_that("mev infusion", {
  a <- mrgsim(mod, param = list(r = 20))  
  a <- filter_sims(a, A == max(A))
  expect_equal(a$time, mod$dose/20)
})

test_that("mev infusion now", {
  a <- mrgsim(mod, param = list(r = 20, n = 1))
  c <- mrgsim(mod, param = list(r = 20))
  expect_equivalent(a, c)
})

test_that("mev infusion later", {
  a <- mrgsim(mod, param = list(r = 20, n = 1, t = 2))
  a <- filter_sims(a, A==max(A))
  expect_identical(a$time, 5)
})

test_that("mev lag times are respected", {
  a <- mrgsim(mod, param = list(l = 2, n = 1, d = 4, r = -2))
  a <- filter_sims(a, A==max(A))
  expect_identical(a$time, param(a)$l + param(a)$d)
})

test_that("mev lag times with F are respected", {
  a <- mrgsim(mod, param = list(r = 50, t = 4, l = 2))
  a <- filter_sims(a, A==max(A))
  expect_identical(a$time, param(a)$t + param(a)$l + 0.5*mod$dose/param(a)$r)
})

code <- '
$CMT A
$MAIN
if(TIME==1) {                // Give dose at TIME==1
  mrgsolve::evdata ev(0, 1); // Set time to 0
  ev.now = true;             // Also set now to true
  ev.amt = 100;
  self.mevector.push_back(ev);
}
'

test_that("now doses are given even when time is past #1151", {
  mod <- mcode("gh-1151", code)
  out <- mrgsim(mod)
  expect_false(all(out$A==0))
  expect_equal(out$A[1], 0)
  expect_equal(out$A[2], 100)
})
