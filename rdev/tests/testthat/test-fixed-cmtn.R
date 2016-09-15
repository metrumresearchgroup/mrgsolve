
library(testthat)
library(mrgsolve)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

rm(list=ls())


code <- '

$CMTN DEPOT

$PARAM CL=1, VC=20

$INIT CENT=0 , DEPOT=0

$FIXED A=1.1, B=2.2

$PARAM KM = 2, VMAX=100

$FIXED  
C = 3.3, D=4.4, E = 5.5

$MAIN double Z = A+B+C+D+E;

$TABLE table(cmtn) = N_DEPOT;
'


mod <- try(suppressWarnings(mcode("FOO",code, audit=FALSE)))

out <- mrgsim(mod)

context("CMTN block gives compartment numbers")

test_that("Model compiles with FIXED and CMTN",{
  expect_is(mod, "mrgmod")
  expect_true(all(out$cmtn==2))
})

context("Fixed parameters")

test_that("FIXED items are excluded from param", {
    expect_identical(names(param(mod)),c("CL", "VC", "KM", "VMAX"))
    expect_identical(names(mod@fixed),c("A", "B", "C", "D", "E"))
})

test_that("FIXED items can be recovered", {
    expect_identical(names(as.list(allparam(mod))),c("CL", "VC", "KM", "VMAX","A","B","C","D","E"))
})

