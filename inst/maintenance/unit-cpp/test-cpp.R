
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


# Test events from within the model
code <- '
$PARAM r = 0, l = 0, d = 5, n = 0, t = 0, dose = 100
$MAIN
ALAG_A = l;
D_A = d;
if(TIME==0) {
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
