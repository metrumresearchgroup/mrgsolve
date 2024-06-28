library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

nmv <- function(x) {
  paste0("[ plugin ] nm-vars \n ", x) 
}

test_that("find nm vars - frda only", {
  code <- '
  [ MAIN ] 
  F2 = 2;
  D1 = 1;
  [ TABLE ] 
  ALAG3 = 3;
  '
  spec <- modelparse(code, split = TRUE)
  ans <- mrgsolve:::find_nm_vars(spec)
  expect_true(ans$found_any)
  expect_false(ans$has_ode)
  expect_length(ans$cmtn, 3)
  expect_equal(nrow(ans$match), 3)
  expect_equivalent(ans$match, ans$frda)
  prefixes <- c("ALAG", "D", "F")
  prefixes <- prefixes[order(prefixes)]
  expect_equal(ans$frda$prefix,  prefixes)
  expect_equal(ans$frda$cmt, c(3, 1 , 2))
  expect_equal(nrow(ans$ddt), 0)
})

test_that("find nm vars - A/A_0/DADT only", {
  code <- '
  [ MAIN ] 
  A_0(2) = 1;
  [ ODE] 
  DADT(1) = -A(1);
  DADT(2) = A(1) + A(2);
  DADT(3) = A(3);
  DADT(4) = 0;
  '
  spec <- modelparse(code, split = TRUE)
  ans <- mrgsolve:::find_nm_vars(spec)
  expect_true(ans$found_any)
  expect_true(ans$has_ode)
  expect_length(ans$cmtn, 4)
  expect_equal(nrow(ans$match), 8)
  prefixes <- c("A", "A_0", "DADT")
  prefixes <- prefixes[order(prefixes)]
  expect_equal(unique(ans$match$prefix), prefixes)
  expect_true(all(ans$ddt$prefix == 'DADT'))
  expect_equal(ans$dcmtn, seq(4))
  expect_equal(ans$ddt$cmt, ans$dcmtn)
  expect_equal(nrow(ans$frda), 0)
  expect_is(ans$cmtn, "numeric")
  expect_is(ans$dcmtn, "numeric")
})

test_that("find nm vars - both", {
  code <- '
  [ PREAMBLE ] 
  F1 = 1;
  [ MAIN ] 
  A_0(2) = 1;
  R2 = 2;
  ALAG1 = 1;
  [ ODE] 
  DADT(1) = -A(1);
  DADT(2) = A(1) + A(2);
  [ TABLE ] 
  D1 = 1;
  CP = A(2) / FOO;
  GUT  = A(1);
  '
  spec <- modelparse(code, split = TRUE)
  ans <- mrgsolve:::find_nm_vars(spec)
  expect_true(ans$found_any)
  expect_true(ans$has_ode)
  expect_length(ans$cmtn, 2)
  expect_equal(nrow(ans$match), 9)
  prefixes <- c("A", "A_0", "DADT", "ALAG", "D", "F", "R")
  frda <- prefixes %in% c("F", "R", "D", "ALAG")
  prefixes <- prefixes[order(frda,prefixes)]
  expect_equal(unique(ans$match$prefix), prefixes)
  expect_true(all(ans$ddt$prefix == 'DADT'))
  expect_equal(ans$dcmtn, seq(2))
  expect_equal(ans$ddt$cmt, ans$dcmtn)
  expect_equal(nrow(ans$frda), 4)
  expect_is(ans$cmtn, "numeric")
  expect_is(ans$dcmtn, "numeric")
})

test_that("FRDA in param is error", {
  expect_error(
    mcode("frda1", nmv("[ param ] CL = 1, F1 = 5")), 
    regexp = "reserved: F1"
  )
  expect_error(
    mcode("frda2", nmv("[ param ] CL = 1, R2 = 2")), 
    regexp = "reserved: R2"
  )
  expect_error(
    mcode("frda3", nmv("[ param ] CL = 1, D5 = 2")), 
    regexp = "reserved: D5"
  )
  expect_error(
    mcode("frda4", nmv("[ param ] CL = 1, ALAG1 = 2")), 
    regexp = "reserved: ALAG1"
  )
})

test_that("FRDA in cmt is error", {
  expect_error(
    mcode("frda5", nmv("[ cmt ] GUT CENT F1")), 
    msg = "reserved: F1"
  )
  expect_error(
    mcode("frda5", nmv("[ cmt ] GUT R1 CENT")), 
    regexp = "reserved: R1"
  )
  expect_error(
    mcode("frda7", nmv("[ cmt ] D5 GUT CENT")), 
    regexp = "reserved: D5"
  )
  expect_error(
    mcode("frda7", nmv("[ cmt  ] B C  ALAG1 D")), 
    regexp = "reserved: ALAG1"
  )
})

test_that("NM Reserved in param is error", {
  expect_error(
    mcode("nmres1", nmv("[ param ] A = 1, CL = 2, WT = 70")), 
    regexp = "reserved: A"
  )
  expect_error(
    mcode("nmres2", nmv("[ param ] CL = 1, V = 2, A_0 = 70")), 
    regexp = "reserved: A_0"
  )
  expect_error(
    mcode("nmres3", nmv("[ param ] CL = 1, DADT = 2, WT = 70")), 
    regexp = "reserved: DADT"
  ) 
  expect_error(
    mcode("nmres4", nmv("[ param ] CL = 1, V = 2, T = 70")), 
    regexp = "reserved: T"
  )
})

test_that("NM Reserved in init is error", {
  expect_error(
    mcode("nmres5", nmv("[ cmt ] GUT CENT A")), 
    msg = "reserved: A"
  )
  expect_error(
    mcode("nmres6", nmv("[ cmt ] GUT A_0 CENT")), 
    regexp = "reserved: A_0"
  )
  expect_error(
    mcode("nmres7", nmv("[ cmt ] X GUT DADT")), 
    regexp = "reserved: DADT"
  )
  expect_error(
    mcode("nmres8", nmv("[ cmt  ] B C  T D")), 
    regexp = "reserved: T"
  ) 
})

test_that("Compartment number bounds checking", {
  expect_error(
    mcode("frda8", nmv("[ cmt ] B C D \n [ main ] F5 = 2;")), 
    regexp = "out of range: F5"
  )
  expect_error(
    mcode("frda8", nmv("[ cmt ] B C D \n [ main ] ALAG50 = 2; \n F2 = 1;")), 
    regexp = "out of range: ALAG50"
  )
  expect_error(
    mcode("frda9", nmv("[ cmt ] B C D \n [ table ] double Foo = A(10);")), 
    regexp = "out of range: A\\(10\\)"
  )
  expect_error(
    mcode("frda10", nmv("[ cmt ] B  \n [ ode ] @!audit \n DADT(5) = 0;")), 
    regexp = "out of range: DADT\\(5\\)"
  )
})

test_that("nm-vars functional test", {
  mod1 <- house(param = list(F1 = 0.8))
  code2 <- '
  [ plugin ] nm-vars
  [ param ] CL = 1, VC = 20, KA = 1.2, F1I = 0.8
  [ cmt ] @number 3
  [ main ] 
  F1 = F1I;
  A_0(3) = 50;
  capture a = EXP(CL);
  capture b = LOG(VC);
  capture c = SQRT(KA);
  capture d = DEXP(CL); 
  capture e = LOG10(VC);
  capture f = COS(M_PI); 
  capture g = SIN(M_PI/2);
  
  [ ode ] 
  DADT(1) = -KA * A(1); 
  DADT(2) =  KA * A(1) - (CL/VC) * A(2);
  DADT(3) =  0;
  [ table ] 
  capture CP = A(2) / VC;
  '
  mod2 <- mcode("nmv-functional", code2, preclean = TRUE)
  dose <- ev(amt = 100)
  out1 <- mrgsim(mod1, dose, end = 48, delta = 1)
  out2 <- mrgsim(mod2, dose, end = 48, delta = 1)
  tol <- 0.01
  expect_equal(out1$CP, out2$CP, tolerance = tol)
  expect_equal(out1$GUT, out2$A1, tolerance = tol)
  expect_equal(out1$CENT, out2$A2, tolerance = tol)
  expect_equal(out1$RESP[1], out2$A3[1], tolerance = tol)
  expect_equal(out2$a[1], exp(mod2$CL), tolerance = 1e-3)
  expect_equal(out2$b[1], log(mod2$VC), tolerance = 1e-3)
  expect_equal(out2$c[1], sqrt(mod2$KA), tolerance = 1e-3)
  expect_equal(out2$d[1], out2$a[1], tolerance = 1e-5)
  expect_equal(out2$e[1], log10(mod2$VC), tolerance = 1e-3)
  expect_equal(out2$f[1], cos(pi), tolerance = 1e-5)
  expect_equal(out2$g[1], sin(pi/2), tolerance = 1e-5)
})

test_that("nm-vars no frda items", {
  code <- '
  $plugin nm-vars
  $cmt A1
  $main A_0(1) = 1;
'
  mod <- mcode("u229", code, compile = FALSE)
  a <- readLines(file.path(soloc(mod), "u229-mread-header.h"))
  expect_false(any(grepl("#define  __[]", a, fixed = TRUE)))
})
