
library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)


test_that("test audit ode block dadt", {
  code1 <- "$CMT A B C  \n $ODE \n dxdt_A  = 0;"
  expect_warning(
    mcode("audit1", code1, compile = FALSE, quiet = FALSE), 
    regexp = "--| missing: dxdt_B\n--| missing: dxdt_C"
  )
  
  code2 <- "$CMT A B C  \n $ODE @!audit \n dxdt_A  = 0;"
  expect_silent(mcode("audit2", code2, compile = FALSE))
  
  code2b <- "$CMT A B  \n $ODE \n dxdt_A=0; \n dxdt_B=0;"
  expect_silent(mcode("audit2b", code2b, compile = FALSE))
  
  code3 <- "$PLUGIN nm-vars \n $CMT A1 B C  \n $ODE @audit \n DADT(1)  = 0;"
  expect_warning(
    mcode("audit3", code3, compile = FALSE), 
    regexp = "missing: DADT(2)\n--| missing: DADT(3)", 
    fixed = TRUE
  )
  
  code4 <- "$PLUGIN nm-vars \n $CMT @number 3  \n $ODE @!audit \n DADT(1)  = 0;"
  expect_silent(mcode("audit4", code4, compile = FALSE))
  
  code4b <- "$PLUGIN nm-vars \n $CMT @number 2  \n $ODE @audit \n DADT(1)=0; \n DADT(2) =1;"
  expect_silent(mcode("audit4", code4, compile = FALSE))
  
  code5 <- "$CMT @number 3  \n $ODE @!audit \n dxdt_A1  = 0;"
  expect_silent(mcode("audit5", code5, compile = FALSE))
  
  code6 <- "$CMT @number 3"
  expect_silent(mod <- mcode("audit6", code6, compile = FALSE))
  expect_is(mod, "mrgmod")
})
