library(mrgsolve)
library(testthat)


Sys.setenv(R_TESTS="")

code <- '
$PARAM CL=1, V2=20,Q=30,V3=200,KA=1
$CMT GUT CENT PERIPH
$PKMODEL ncmt=2, depot=TRUE

$MAIN
double a = 1;
double b = 2;

$CAPTURE a b
'

mod <- mcode("req1", code)


context("test-request")


test_that("Req gets the right variables", {
  x1 <- names(mod %>% mrgsim)
  x2 <- names(mod %>% Req(PERIPH,GUT) %>% mrgsim)
  x3 <- names(mod %>% Req(PERIPH,b) %>% mrgsim)
  x4 <- names(mod %>% Req(b,a) %>% mrgsim)
  expect_identical(x1,s(ID,time,GUT,CENT,PERIPH,a,b))
  expect_identical(x2,s(ID,time,PERIPH,GUT))
  expect_identical(x3,s(ID,time,PERIPH,b))
  expect_identical(x4,s(ID,time,b,a))
})



mod <- update(mod, request="CENT")

test_that("Req gets the right variables, with request", {
  x1 <- names(mod %>% mrgsim)
  x2 <- names(mod %>% Req(PERIPH,GUT) %>% mrgsim)
  x3 <- names(mod %>% Req(PERIPH,b) %>% mrgsim)
  x4 <- names(mod %>% Req(b,a) %>% mrgsim)
  expect_identical(x1,s(ID,time,CENT,a,b))
  expect_identical(x2,s(ID,time,PERIPH,GUT))
  expect_identical(x3,s(ID,time,PERIPH,b))
  expect_identical(x4,s(ID,time,b,a))
})



