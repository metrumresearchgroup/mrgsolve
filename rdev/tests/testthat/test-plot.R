library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

project <- file.path(system.file(package="mrgsolve"), "models")

context("Testing that plots can be generated from output objects")

mod <- mrgsolve:::house() %>% init(CENT=1000)
out <- mrgsim(mod)


# test_that("Plot from mrgsims without a formula", {
#   expect_is(plot(out), "trellis")
# })
# 
# test_that("Plot from mrgsims with a formula", {
#   expect_is(plot(out, CP~time), "trellis")
# })
# 
# 
# out <- knobs(mod, CL=c(1,2,3), VC=c(10,20))
# 
# test_that("Plot from batch_mrgsims without a formula", {
#   expect_is(plot(out), "trellis")
#   
# })
# 
# test_that("Plot from batch_mrgsims with a formula", {
#   expect_is(plot(out, CP~time), "trellis")
# })
