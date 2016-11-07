Sys.setenv("R_TESTS" = "")
library(testthat)
library(mrgsolve)

if(.Platform$OS.type != "windows") {
  test_check("mrgsolve",reporter="summary")
}

