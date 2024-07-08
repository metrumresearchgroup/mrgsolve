
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

local_edition(3)

code <- '
$SET end = 12, rtol = 1e-4, atol = 1e-4, delta = 0.25
$PLUGIN evtools
$PARAM 
mode = 0, f1 = 1, dose = 100, irate = 50, newtime = 2
reptime = 5
$CMT A B C
$PK
F_A = f1;
$DES
dxdt_A = -1 * A;
dxdt_B =  1 * A - 0.1 * B;
dxdt_C = 0;
$TABLE
bool givedose = TIME==0;
if(mode==1 && givedose) {
  evt::bolus(self, dose, 1);
}
if(mode==2 && givedose) {
  evt::infuse(self, dose, 1, irate);
}
if(mode==3 && givedose) {
  evt::ev ev = evt::bolus(dose, 1); 
  evt::retime(ev, 2);
  evt::push(self, ev);
}
if(mode==4 && givedose) {
  evt::ev ev = evt::infuse(dose, 1, irate); 
  evt::retime(ev, newtime);
  evt::push(self, ev);
}
if(mode==5 && givedose) {
  evt::bolus(self, dose, 1); 
  evt::bolus(self, dose, 1);
  evt::bolus(self, dose, 1);
}
if(mode==6 && givedose) {
  evt::ev ev = evt::bolus(dose, 1);
  ev.time = 100;
  evt::now(ev);
  evt::push(self, ev);
}
if(mode==7 && TIME==reptime) {
  evt::replace(self, C/2.0, 3); 
}
if(mode==8 && givedose) {
  evt::ev rep = evt::replace(C/10.0, 3);
  evt::retime(rep, reptime);
  self.push(rep);
}
'

mod <- mcode("test-evtools-model-1", code)

mrgsim(mod, param = list(mode = 1, f1 = 1)) 

test_that("evtools - bolus now", {
  
  out <- mrgsim(mod, param = list(mode = 1))
  expect_equal(out$A[1], mod$dose)
  expect_equal(out$B[1], 0)
  
  out <- mrgsim(mod, param = list(mode = 1, f1 = 0.5))
  expect_equal(out$A[1], mod$dose * param(out)$f1)
  expect_equal(out$B[1], 0)
})

test_that("evtools - infusion now", {
  out <- mrgsim(mod, param = list(mode = 2))
  tmax <- out$time[which.max(out$A)]
  expect_equal(tmax, mod$dose/mod$irate)
  expect_equal(out$B[1], 0)
  
  out <- mrgsim(mod, param = list(mode = 2, f1 = 0.5))
  tmax <- out$time[which.max(out$A)]
  env <- as.list(param(out)) 
  expect_equal(tmax, param(out)$f1 * mod$dose / mod$irate)
})

test_that("evtools - bolus with retime", {
  out <- mrgsim(mod, param = list(mode = 3), recsort = 3)
  cmax <- filter_sims(out, time==2)
  expect_equal(nrow(cmax), 1L)
  expect_equal(cmax$A, mod$dose)
})

test_that("evtools - infusion with retime", {
  out <- mrgsim(mod, param = list(mode = 4), recsort = 3)
  tmax <- out$time[which.max(out$A)]
  expect_equal(tmax, mod$newtime + mod$dose/mod$irate)
  expect_equal(out$B[1], 0)
})

test_that("evtools - multiple bolus doses given now", {
  out <- mrgsim(mod, param = list(mode = 5))
  expect_equal(out$A[1], 300)
  tmax <- out$time[which.max(out$A)]
  expect_equal(tmax, 0)
})

test_that("evtools - give timed dose now", {
  a <- mrgsim(mod, param = list(mode = 6))
  b <- mrgsim(mod, param = list(mode = 1))
  expect_identical(as.data.frame(a), as.data.frame(b))
})

test_that("evtools - replace", {
  mod <- init(mod, C = 100)
  mod <- update(mod, delta = 0.1)
  a <- mrgsim(mod, param = list(mode = 7))
  before <- filter(a, time < 5)
  expect_true(all(before$C==100))
  after <- filter(a, time >= 5)
  expect_true(all(after$C==50))
  
  # When the replacement is timed into the future, we see the 
  # replacement right at the indicated time
  b <- mrgsim(mod, param = list(mode = 8, reptime = 8))
  before <- filter(b, time <= 8) # note: <= 8
  expect_true(all(before$C==100))
  after <- filter(b, time > 8) # note: > 8
  expect_true(all(after$C==10))
  
  # We can control this with `recsort`
  c <- mrgsim(mod, param = list(mode = 8, reptime = 8), recsort = 3)
  before <- filter(c, time < 8) # note: < 8
  expect_true(all(before$C==100))
  after <- filter(c, time >= 8) # note: >= 8
  expect_true(all(after$C==10))
})
