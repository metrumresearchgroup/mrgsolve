library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)


context("test-cmtn")

code <- '
$CMT A B C D Z
$CMTN A Z
$CAPTURE N_Z N_A
'

test_that("Get compartment numbers by $CMTN", {
  mod <- mcode("test-cmtn-1", code)
  expect_is(mod, "mrgmod")
  out <- mrgsim(mod)
  expect_all_equal(out$N_Z, 5)
  expect_all_equal(out$N_A, 1)
})

code <- '
$CMT A B C D Z
$PLUGIN N_CMT
$CAPTURE N_A N_B N_C N_D N_Z
'

test_that("Get compartment numbers by N_CMT plugin", {
  mod <- mcode("test-cmtn-2", code)
  mod <- update(mod, outvars = paste0("N_", names(init(mod))))
  expect_is(mod, "mrgmod")
  out <- mrgsim(mod)
  check <- as.data.frame(out)[1,,drop = TRUE]
  check <- unlist(check[-c(1,2)], use.names = FALSE)
  expect_equal(check, seq(5))
})


code <- '
$CMT A B C D Z
$MAIN 
F_B = 1;
ALAG_D = 0.25;
$CAPTURE N_B N_D
'

test_that("Get compartment numbers by FRDA", {
  mod <- mcode("test-cmtn-3", code)
  expect_is(mod, "mrgmod")
  out <- mrgsim(mod)
  expect_all_equal(out$N_B, 2)
  expect_all_equal(out$N_D, 4)
})

