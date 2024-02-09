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



code <- ' // gh-1169
$PARAM CL = 1, V = 10, KA = 1

$PKMODEL cmt = "GUT,CENT", depot = TRUE

$PLUGIN evtools

$GLOBAL
evt::regimen reg;

$PK
if(NEWIND <= 1) {
  reg.init(self);
  reg.amt(100);
  reg.ii(24);
  reg.cmt(2);
}

$ERROR
if(TIME==168 && EVID==0) {
  reg.ii(12);
  reg.amt(reg.amt()/2);
}

if(TIME==300 && EVID==0) {
  reg.ii(6);
  reg.amt(reg.amt()/2);
}

if(TIME==400 && EVID==0) {
  reg.ii(24);
  reg.amt(reg.amt()*4);
}

reg.execute();
'
mod <- mcode("fix-gh-1169", code)

test_that("regimen::ii() gives expected result gh-1169", {
  
  out <- mrgsim(mod, end = 840, output = "df")
  out <- mutate(out, DAY = 1+floor(time/24)) 
  
  summ <-
    out %>% 
    group_by(DAY) %>% 
    summarise(Cmax = max(CENT), Cmin = min(CENT)) %>% 
    ungroup()
  
  # ggplot(as.data.frame(out), aes(time,CENT)) + geom_line() + geom_vline(xintercept = c(168, 300, 400))
  
  #' The exact values for the cutoffs aren't obvious; but looking for 
  #' steady-state Cmax that starts high, goes down in the next two periods and 
  #' then comes back up to the original; Cmin will will start low, increase x2 
  #' and then return to original values; changing dose interval and dose as 
  #' suggested in the code will fit this pattern
  summ1 <- filter(summ, DAY >  2 & DAY <=  7)
  summ2 <- filter(summ, DAY >  8 & DAY <= 13) 
  summ3 <- filter(summ, DAY > 14 & DAY <  17)
  summ4 <- filter(summ, DAY > 19 & DAY <  35)
  
  expect_true(all(summ1$Cmax > 109 & summ1$Cmax < 110))
  expect_true(all(summ2$Cmax >  71 & summ2$Cmax <  72))
  expect_true(all(summ3$Cmax >  54 & summ3$Cmax <  56))
  expect_true(all(summ4$Cmax > 109 & summ4$Cmax < 110))
  
  expect_true(all(summ1$Cmin > 11 & summ1$Cmin < 12))
  expect_true(all(summ2$Cmin > 23 & summ2$Cmin < 24))
  expect_true(all(summ3$Cmin > 33 & summ3$Cmin < 34))
  expect_true(all(summ4$Cmin > 11 & summ4$Cmin < 12))
})
