library(mrgsolve)
library(testthat)
#library(MASS)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)
project <- file.path(system.file(package="mrgsolve"), "models")

context("Testing matlist operations")

code <- '
$PARAM CL=1
$INIT CENT=0
$ODE dxdt_CENT = 0;

$OMEGA 0 0 0 
$SIGMA 0 0 0
'

tmp <- tempdir()

mod <- mread(code=code, project=tmp, model="test4")


mod <- mod %>% omat(diag(c(1.2, 2.3,3.4))) %>% smat(diag(c(0.1, 0.2, 0.3)))


test_that("Indexing OMEGA matrix elements", {
  expect_equivalent(as.matrix(omat(mod))[2,2],2.3)
  expect_equivalent(as.matrix(omat(mod))[2,1],0)
  expect_equivalent(as.matrix(omat(mod))[3,3],3.4)
})

test_that("Indexing SIGMA matrix elements", {
  expect_equivalent(as.matrix(smat(mod))[2,2],0.2)
  expect_equivalent(as.matrix(smat(mod))[2,1],0)
  expect_equivalent(as.matrix(smat(mod))[3,3],0.3)
})

o1 <- omat(diag(c(1.1, 2.2, 3.3)), diag(c(4.4, 5.5, 6.6)), matrix(seq(91,99),nrow=3, byrow=TRUE))
mat <- as.matrix(o1)
test_that("Indexing OMEGA matrix elements with multiple matrices", {
    expect_equivalent(mat[3,3],3.3)
    expect_equivalent(mat[7,2],0)
    expect_equivalent(mat[7,7],91)
    expect_equivalent(mat[9,8],98)
})







