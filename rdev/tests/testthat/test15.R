library(mrgsolve)
library(testthat)
require(dplyr)

Sys.setenv(R_TESTS="")

options("mrgsolve_mread_quiet"=TRUE)
project <- file.path(system.file(package="mrgsolve"), "models")

context("Infusions that end too close to observations.")

mod  <- mrgsolve:::house()

test_that("Infusion with no obs overlap", {
  mod <- mod %>% ev(amt=6, rate=5)
  expect_is(mod %>% mrgsim, "mrgsims")
})

test_that("Infusion with obs overlap", {
  mod <- mod %>% ev(amt=6, rate=5)
  expect_is(mod %>% mrgsim(delta=0.1), "mrgsims")
  #expect_error(mod %>% update(mindt=0) %>% mrgsim(delta=0.1))
})


context("Infusion with ss flag")

test_that("Infusion executes with ss flag and ii==dur", {
    out <- 
      mod %>% 
      Req(CP) %>% obsonly %>%
      ev(amt=100,rate=100,ii=1,addl=10,ss=1) %>% 
      mrgsim(end=10,digits=5) %>% filter(time>0)
  
    expect_true(all(out$CP==100))
  
})

