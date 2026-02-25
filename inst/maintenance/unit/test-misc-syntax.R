
library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

theta_sigma_n <- '
[ param ] THETA1 = 1.2, SN = 1
[ SIGMA ] 1.1 2.2 3.3 4.4
[ main ] 
capture CL = THETA(1);
[ error ] 
capture SIGMA2 = SIGMA(SN);

'
mod_th_sg_n <- mcode("theta_sigma_n", theta_sigma_n)

test_that("THETA(n) is allowed", {
  out <- mrgsim(mod_th_sg_n, end = 1)
  expect_equal(out$CL[1], 1.2)
  expect_error(
    mcode("thetan2", "[param] THETA = 2"), 
    regexp="Reserved words in model names: THETA"
  )
})

test_that("Access SIGMA(n) [SLV-TEST-0090]", {
  out <- mrgsim(mod_th_sg_n, end = -1, param = list(SN = 2))
  expect_equal(out$SIGMA2, 2.2)
  out <- mrgsim(mod_th_sg_n, end = -1, param = list(SN = 4))
  expect_equal(out$SIGMA2, 4.4)
})

mod_th_sg_n <- NULL

test_that("autodec + nm-vars functional test", {
  eps <- 1e-4
  mod <- modlib("nm-like", delta = 0.1, preclean = TRUE, capture = "KA,INH")  
  dose <- ev(amt = 100, D2I = 2, cmt = 2, rate = -2)
  out <- mrgsim(mod, dose)
  tmax <- out$time[which.max(out$A2)]
  expect_true(abs(tmax-2) < eps)
  expect_true(all(out$KA==mod$THETA3))
  expect_true("INH" %in% names(out))
  dose <- ev(amt = 100, cmt = 1, F1I = 0.81)
  out <- mrgsim(mod, dose)
  expect_true(abs(out$A1[2]-0.81*dose$amt[1]) < eps)
})

test_that("autodec + nm-vars using nm reserved", {
  code <- '
  $PLUGIN autodec nm-vars
  $MAIN 
  A = 1; 
  B = 2;
  F1 = 5;
  '
  expect_error(
    mcode("auto-nm-reserved", code, compile = FALSE), 
    regexp="reserved: A", fixed = TRUE
  )
})

test_that("grep code autodec and nm-vars", {
  code <- '
$PLUGIN autodec nm-vars
$PARAM CL = 1, BC = 2
$CMT FOO BAR YAK
$MAIN 
F1 = 0.25;
X = 33;
double Y = 44;
ALAG2 = 5;
D3 = 10;
F1 = 1.23;
$ODE @!audit
DADT(1) = 1; 
DADT(2) = 2;
$TABLE
Z = 5;
'
  
  x <- mcode("parse1", code, compile = FALSE)
  where <- x@soloc
  f <- list.files(where, full.names = TRUE)
  code1 <- readLines(f[1])
  
  f1 <- grepl("double& F1", code1, fixed = TRUE)
  al <- grepl("double& ALAG", code1, fixed = TRUE)
  d3 <- grepl("double& D3", code1, fixed = TRUE)
  x <- grepl("double X", code1, fixed = TRUE)
  y <- grepl("double Y", code1, fixed = TRUE)
  z <- grepl("double Z", code1, fixed = TRUE)
  da <- grepl("DADT", code1, fixed = TRUE)
  cl <- grepl("double& CL", code1, fixed = TRUE)
  
  expect_equal(sum(f1), 2)
  expect_equal(sum(al), 2)
  expect_equal(sum(d3), 2)
  expect_equal(sum(x), 1)
  expect_equal(sum(y), 1)
  expect_equal(sum(z), 1)
  expect_equal(sum(da), 5)
  expect_equal(sum(cl), 3)
})

test_that("grep code autodec", {
  code <- '
$PLUGIN autodec
$PARAM CL = 1, BC = 2
$CMT FOO BAR YAK
$MAIN 
F1 = 0.25;
X = 33;
double Y = 44;
ALAG2 = 5;
D3 = 10;
$ODE @!audit
DADT(1) = 1; 
DADT(2) = 2;
$TABLE
Z = 5;
'
  
  x <- mcode("parse2", code, compile = FALSE)
  where <- x@soloc
  f <- list.files(where, full.names = TRUE)
  code1 <- readLines(f[1])
  f1 <- grepl("double F1", code1, fixed = TRUE)
  al <- grepl("double ALAG", code1, fixed = TRUE)
  d3 <- grepl("double D3", code1, fixed = TRUE)
  x <- grepl("double X", code1, fixed = TRUE)
  y <- grepl("double Y", code1, fixed = TRUE)
  z <- grepl("double Z", code1, fixed = TRUE)
  da <- grepl("\\bDADT", code1, fixed = TRUE)
  cl <- grepl("double& CL", code1, fixed = TRUE)
  
  expect_equal(sum(f1), 1)
  expect_equal(sum(al), 1)
  expect_equal(sum(d3), 1)
  expect_equal(sum(x), 1)
  expect_equal(sum(y), 1)
  expect_equal(sum(z), 1)
  expect_equal(sum(da), 0)
  expect_gt(sum(cl), 1)
})

test_that("grep code nm-vars", {
  code <- '
$PLUGIN  nm-vars
$PARAM CL = 1, BC = 2
$CMT FOO BAR YAK
$MAIN 
F1 = 0.25;
double X = 33;
double Y = 44;
ALAG2 = 5;
D3 = 10;
F1 = 1.23;
$ODE @!audit
DADT(1) = 1; 
DADT(2) = 2;
$TABLE
double Z = 5;
'
  
  x <- mcode("parse3", code, compile = FALSE)
  where <- x@soloc
  f <- list.files(where, full.names = TRUE)
  code1 <- readLines(f[1])
  
  f1 <- grepl("double& F1", code1, fixed = TRUE)
  al <- grepl("double& ALAG", code1, fixed = TRUE)
  d3 <- grepl("double& D3", code1, fixed = TRUE)
  x <- grepl("double X", code1, fixed = TRUE)
  y <- grepl("double Y", code1, fixed = TRUE)
  z <- grepl("double Z", code1, fixed = TRUE)
  da <- grepl("DADT", code1, fixed = TRUE)
  cl <- grepl("double& CL", code1, fixed = TRUE)
  foo <- grepl("FOO", code1, fixed = TRUE)
  
  expect_equal(sum(f1), 2)
  expect_equal(sum(al), 2)
  expect_equal(sum(d3), 2)
  expect_equal(sum(x), 1)
  expect_equal(sum(y), 1)
  expect_equal(sum(z), 1)
  expect_equal(sum(da), 5)
  expect_equal(sum(cl), 3)
  expect_equal(sum(foo), 7)
})

test_that("Recognize end of infusion", {
  
  code <- '
$PARAM CL = 1, V = 20
$PKMODEL cmt = "A"
$PREAMBLE int count = 0;
$MAIN if(END_OF_INFUSION) ++count;
$CAPTURE count
'
  
  mod <- mcode("test-end-of-infusion", code)
  
  out1 <- mrgsim(mod, ev(amt = 100, tinf = 5), obsonly = TRUE)
  
  expect_true(all(out1$count[1:5]==0))
  expect_true(all(out1$count[6:24]==1))
  
  out2 <- mrgsim(mod, ev(amt = 100), obsonly = TRUE)
  expect_true(all(out2$count==0))
  
  out3 <- mrgsim(mod, ev(amt = 100, tinf=100), obsonly = TRUE)
  expect_true(all(out3$count==0))
})

