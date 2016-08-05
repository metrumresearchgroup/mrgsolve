
library(mrgsolve)
library(testthat)
#library(MASS)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)


mod <- mrgsolve:::house() %>% update(end=5, delta=1) %>% Req(CP)


context("Testing carry_out of tran / PK dosing items")

ex <- rep(0,7)

test_that("carry.out amt", {
    out <- mod %>% ev(amt=800, rate=2) %>% carry_out(amt) %>% mrgsim
    ex[2] <- 800
    expect_equal(out$amt, ex)
})

test_that("carry.out rate", {
  out <- mod %>% ev(amt=800, rate=2) %>% carry_out(amt,rate,ss) %>% mrgsim
  ex[2] <- 2
  expect_equal(out$rate, ex)
  ex[2] <- 0
  expect_equal(out$ss,ex)
})

test_that("carry.out rate", {
  out <- mod %>% ev(amt=800, rate=2) %>% carry_out(amt,rate,ss) %>% mrgsim
  ex[2] <- 2
  expect_equal(out$rate, ex)

})

test_that("carry.out mixed", {
  out <- mod %>% ev(amt=800, rate=2, ii=12, addl=22) %>% carry_out(addl,ii,amt,rate,ss) %>% mrgsim
  expect_equal(names(out), c("ID", "time","amt", "ss", "ii", "addl", "rate", "CP"))
  expect_equivalent(as.data.frame(out)[2,2:7], list(0,800,0,12,22,2))
})

mod <- ev(mod, amt=200, ii=14, rate=11, ss=1,addl=99)
test_that("carry_out mixed, rename", {
  out <- mod %>% carry.out(a=addl,i=ii,d=amt,r=rate,s=ss) %>% mrgsim
  expect_equal(names(out), c("ID", "time","d", "s", "i", "a", "r", "CP"))
  expect_equivalent(as.data.frame(out)[2,2:7], list(0,200,1,14,99,11))
})

test_that("carry_out mixed, some rename", {
  out <- mod %>% carry.out(a=addl,ii,d=amt,rate,s=ss) %>% mrgsim
  expect_equal(names(out), c("ID", "time","d", "s", "ii", "a", "rate", "CP"))
  expect_equivalent(as.data.frame(out)[2,2:7], list(0,200,1,14,99,11))
})


