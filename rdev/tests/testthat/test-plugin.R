
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

mrgsolve:::comp_forget()
rm(list=ls())



code <- '
$PLUGIN Rcpp

$MAIN
double a = R::rnorm(10,2);
double b = R::rbeta(1,1);
double c = R::rexp(0.5);
double d = R::rpois(0.6);
double e = R::rbinom(1,0.23);
double f = R::runif(25,70);
double g = R::rweibull(1,1.5);

$CAPTURE a b c d e f g

'

context("Rcpp - random")

test_that("Rcpp all distributions", {
  mod <- mcode("test_plugin-1", code, warn=FALSE)
  out <- mod %>% mrgsim(end=1000)
  expect_true(all(out$f > 25 & out$f < 70))
  expect_true(all(out$b > 0 & out$b < 1))
  expect_true(mean(out$a) > 9 & mean(out$a) < 10)
  
})



