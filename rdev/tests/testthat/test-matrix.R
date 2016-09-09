library(mrgsolve)
library(testthat)
library(dplyr)


Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-annot")



test_that("Testing modMATRIX", {
    expect_equal(dim(modMATRIX("  0 0 0   0")), c(4,4))
    expect_equal(dim(modMATRIX("  0\n 0\n 0   0")), c(4,4))
    expect_equal(dim(modMATRIX("  0 0 0   ", block=TRUE)), c(2,2))
    expect_error(modMATRIX("  0 0 0  0 ", block=TRUE))
    expect_equal(modMATRIX("0 0 0", use=FALSE), matrix(0,nrow=3,ncol=3))
})





