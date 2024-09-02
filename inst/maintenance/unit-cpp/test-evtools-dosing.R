library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

local_edition(3)

code <- '
$PLUGIN evtools

$PARAM Mdose = 0

$PARAM 
Lag   =  0
Ii    =  24
Ss    =  0
Addl  =  0
Rate  =  0
Dur   =  0
Amt   =  100
Cmt2  = -1
Amt2  = -1
Rate2 = -1
Time2 = -1

$CMT B D

$MAIN 
ALAG_B = Lag;
D_B = Dur; 

$ODE
dxdt_D =  -0.3 * D;
dxdt_B =   0.3 * D - 0.1*B;

$ERROR

if(TIME==0 && Mdose > 0) {
  evt::ev dose = evt::infuse(Amt, 1, Rate); 
  evt::ss(dose, Ss); 
  evt::ii(dose, Ii); 
  evt::addl(dose, Addl); 
  if(Rate2 > 0) evt::rate(dose, Rate2);
  if(Cmt2 > 0) evt::cmt(dose, Cmt2);
  if(Amt2 > 0) evt::amt(dose, Amt2);
  if(Time2 > 0) evt::retime(dose, Time2);
  self.push(dose);
}
'

mod1 <- mcode("reset", code, delta = 0.1, end = 96)
mod2 <- update(mod1, param = list(Mdose = 1))

test_that("Bolus - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 24, addl = 3)
  p <- list(Amt = 100, Ii = 24, Addl = 3)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <-   mutate(d, time = 3)
  p$Time2 <- 3
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Bolus, lag - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 24, addl = 3, Lag = 5)
  p <- list(Amt = 100, Ii = 24, Addl = 3, Lag = 5)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <-   mutate(d, time = 2.5)
  p$Time2 <- 2.5
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)

  expect_identical(out3$B, out4$B)
})

test_that("Bolus, ss - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 24, addl = 3, ss = 1)
  p <- list(Amt = 100, Ii = 24, Addl = 3, Ss = 1)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <-   mutate(d, time = 3.5)
  p$Time2 <- 3.5
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Bolus, ss, lag - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 24, addl = 3, ss = 1, Lag = 3)
  p <- list(Amt = 100, Ii = 24, Addl = 3, Ss = 1, Lag = 3)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <- mutate(d, time = 3.1)
  p$Time2 <- 3.1
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Infusion - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 12, addl = 3, Dur = 7, rate = -2)
  p <- list(Amt = 100, Ii = 12, Addl = 3, Dur = 7, Rate = -2)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <- mutate(d, time = 2)
  p$Time2 <- 2
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Infusion, lag - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 8, addl = 3, Lag = 5, Dur = 5, rate = -2)
  p <- list(Amt = 100, Ii = 8, Addl = 3, Lag = 5, Dur = 5, Rate = -2)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <- mutate(d, time = 4)
  p$Time2 <- 4
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Infusion, ss - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 24, addl = 2, ss = 1, Dur = 8, rate = -2)
  p <- list(Amt = 100, Ii = 24, Addl = 2, Ss = 1, Dur = 8, Rate = -2)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <- mutate(d, time = 1.5)
  p$Time2 <- 1.5
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Infusion, ss, lag - ss and addl via evt", {
  d <-   ev(amt = 100, ii = 16, addl = 3, ss = 1, Lag = 3, Dur = 9, rate = -2)
  p <- list(Amt = 100, Ii = 16, Addl = 3, Ss = 1, Lag = 3, Dur = 9, Rate = -2)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # same dose, but "later"
  d <- mutate(d, time = 2.1)
  p$Time2 <- 2.1
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Alternate infusion - dosing via evt", {
  # data event passes the rate directly rather than modeled duration
  d <-   ev(amt = 100, ii = 24, addl = 1, tinf = 4)
  p <- list(Amt = 100, Ii = 24, Addl = 1, Dur = 4, Rate = -2)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  # Both use direct rate rather than modeled duration
  d <- ev(amt = 100, ii = 24, addl = 1, rate = 20)
  p <- list(Amt = 100, Ii = 24, Addl = 1, Rate = 20)
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Single dose - dosing via evt", {
  d <-   ev(amt = 100, ii = 24, tinf = 3)
  p <- list(Amt = 100, Ii = 24, Addl = 0, Dur = 3, Rate = -2)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  
  d <- mutate(d, time = 3.3)
  p$Time2 <- 3.3
  out3 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out4 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out3$B, out4$B)
})

test_that("Switch compartment - dosing via evt", {
  d <-   ev(amt = 100, ii = 24, addl = 2, cmt = 2)
  p <- list(Amt = 100, Ii = 24, Addl = 2, Cmt2 = 2)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
  expect_identical(out1$D, out2$D)
})

test_that("Switch dose amount - dosing via evt", {
  d <-   ev(amt = 200, ii = 24, addl = 2)
  p <- list(Amt = 100, Ii = 24, Addl = 2, Amt2 = 200)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
})

test_that("Switch infusion rate - dosing via evt", {
  d <-   ev(amt = 100, ii = 24, addl = 2, tinf = 5)
  p <- list(Amt = 100, Ii = 24, Addl = 2, Rate2 = 100/5, Rate = -2, Dur = 10)
  out1 <- mrgsim(mod1, d, recsort = 3, obsonly = TRUE)
  out2 <- mrgsim(mod2, param = p, obsonly = TRUE, recsort = 3)
  
  expect_identical(out1$B, out2$B)
})
