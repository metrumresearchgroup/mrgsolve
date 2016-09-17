library(mrgsolve)
library(testthat)

Sys.setenv(R_TESTS="")

context("test-modspec")

options(mrgsolve_mread_quiet=TRUE)

mtemp <- function(...) mcode(model="tmp",..., compile=FALSE)

test_that("Parse matrix", {
  
  code <- "$OMEGA \n 1 2 \n 3"
  mod <- mtemp(code)
  expect_equal(dim(omat(mod))[[1]],c(3,3))
  
  code <- "$OMEGA \n block=TRUE \n 1 0.002 \n 3"
  mod <- mtemp(code)
  expect_equal(dim(omat(mod))[[1]],c(2,2))  
  
})

test_that("Parse capture", {
  
  code <- "$CAPTURE\n yes=TRUE \n z a"
  mod <- mtemp(code)
  expect_equal(mod@capture, c("z", "a"))
  
  code <- "$CAPTURE\n yes=TRUE \n z a \n\n\n d\n e, f"
  mod <- mtemp(code)
  expect_equal(mod@capture, c("z", "a", "d", "e", "f"))
  
  code <- "$CAPTURE >> yes=TRUE \n z a \n\n\n d\n e, f"
  mod <- mtemp(code)
  expect_equal(mod@capture, c("z", "a", "d", "e", "f"))
  
})

test_that("Parse theta", {
  code <- "$THETA\n  0.1 0.2 \n 0.3"
  mod <- mtemp(code)
  expect_equal(param(mod), param(THETA1=0.1, THETA2=0.2, THETA3=0.3))
  
  code <- "$THETA\n name='theta' \n  0.1 0.2 \n 0.3"
  mod <- mtemp(code)
  expect_equal(param(mod), param(theta1=0.1, theta2=0.2, theta3=0.3))
  
  code <- "$THETA >> name='theta' \n  0.1 0.2 \n 0.3"
  mod <- mtemp(code)
  expect_equal(param(mod), param(theta1=0.1, theta2=0.2, theta3=0.3))
})

test_that("Using table macro generates error", {
  code <- "$TABLE\n table(CP) = 1; \n double x=3; \n table(Y) = 1;"
  expect_error(mod <- mtemp(code))

})


