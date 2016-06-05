
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

mrgsolve:::comp_forget()
rm(list=ls())



code <- '
$PLUGIN mrgx

$MAIN
double a = mrgx::rnorm(10,2);
double b = mrgx::rbeta(1,1);
double c = mrgx::rexp(0.5);
double d = mrgx::rpois(0.6);
double e = mrgx::rbinom(0.23);
double f = mrgx::runif(25,70);
double g = mrgx::rweibull(1,1.5);

$CAPTURE a b c d e f g

'

context("mrgx - random")

test_that("mrgx all distributions", {
  mod <- mcode("test_plugin-1", code, warn=FALSE)
  out <- mod %>% mrgsim(end=1000)
  expect_true(all(out$f > 25 & out$f < 70))
  expect_true(all(out$b > 0 & out$b < 1))
  expect_true(mean(out$a) > 9 & mean(out$a) < 10)
  
})



