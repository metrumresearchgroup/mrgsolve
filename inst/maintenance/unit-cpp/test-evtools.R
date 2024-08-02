
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
reptime = 5, A0 = 0, B0 = 0, C0 = 0, dosetime = 0
$CMT A B C
$PK
F_A = f1;
A_0 = A0;
B_0 = B0;

$DES
dxdt_A = -1 * A;
dxdt_B =  1 * A - 0.1 * B;
dxdt_C = 0;
$TABLE
bool givedose = TIME==dosetime;
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
if(mode==9 && givedose) {
  evt::reset(self);
}
if(mode==10 && givedose) {
  evt::ev res = evt::reset();
  res.time = 15;
  self.push(res);
}
if(mode==11 && givedose) {
  evt::reset(self, 100, 3);
}
if(mode==12 && givedose) {
  evt::reset(self, 100, 1, 20);
}
if(mode==13 && givedose) {
  evt::ev res = evt::reset(100, 3);
  self.push(res);
}
if(mode==14 && givedose) {
  evt::ev res = evt::reset(100, 1, 20);
  self.push(res);
}
'

mod <- mcode("test-evtools-model-1", code)

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

test_that("evtools - reset", {
  mod <- param(mod, dosetime = 5, B0 = 50, mode = 9)
  out <- mrgsim(mod)
  expect_true(all(out$A==0))
  expect_true(all(out$C==0))
  x <- filter(out, time %in% c(0,5))
  expect_true(all(x$B==50))
  x <- filter(out, time==4)
  expect_equal(x$B, 50*exp(-0.1*4), tolerance = 1e-2)
  mod <- param(mod, mode = 10)
  out2 <- mrgsim(mod)
  expect_equal(out@data, out2@data)
})

test_that("evtools - reset with bolus", {
  mod <- param(mod, dosetime = 5, B0 = 50, mode = 11)
  out <- mrgsim(mod)
  x <- filter(out, time==5)
  expect_equal(x$A, 0)
  expect_equal(x$B, 50)
  expect_equal(x$C, 100)
  x <- filter(out, time==4.75)
  expect_equal(x$C, 0)
  x <- filter(out, time > 5)
  expect_true(all(x$C==100))
})


test_that("evtools - reset with infusion", {
  mod <- param(mod, dosetime = 5, B0 = 50, mode = 12)
  out <- mrgsim(mod, rtol = 1e-12)
  x <- filter(out, time %in% c(0,5))
  expect_true(all(x$B==50))
  expect_true(all(x$A==0))
  x <- filter(out, time == 4.75)
  expect_equal(x$A, 0)
  expect_equal(x$B, 50*exp(-0.1*4.75), tolerance = 1e-2)
  # Maximum A is at the end of the infusion
  x <- filter(out, A==max(A))
  expect_equal(x$time, 10)
  
  # Identical results
  out1 <- mrgsim(mod, param = list(mode = 11))
  out2 <- mrgsim(mod, param = list(mode = 12))
  out3 <- mrgsim(mod, param = list(mode = 13))
  out4 <- mrgsim(mod, param = list(mode = 14))
  
  expect_identical(out1@data, out3@data)
  expect_identical(out2@data, out4@data)
})
