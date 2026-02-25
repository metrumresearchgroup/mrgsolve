library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-rdefs")

mtry <- function(...) {
  suppressMessages(suppressWarnings(try(mcode(...))))  
}

code <- c("$param B = 1", "$cmt A", "$omega @labels ETACL\n 1")

test_that("Params are const everywhere", {
  codex <- c(code, "$preamble B = 2;")
  a <- mtry("param-preamble", codex, recover = TRUE)
  expect_match(a$out$stderr, "declared const here",  all = FALSE)

  codex <- c(code, "$main B = 2;")
  a <- mtry("param-main", codex, recover = TRUE)
  expect_match(a$out$stderr, "declared const here",  all = FALSE)

  codex <- c(code, "$ode B = 2;")
  a <- mtry("param-ode", codex, recover = TRUE)
  expect_match(a$out$stderr, "declared const here",  all = FALSE)

  codex <- c(code, "$table B = 2;")
  a <- mtry("param-table", codex, recover = TRUE)
  expect_match(a$out$stderr, "declared const here",  all = FALSE)

})

test_that("FRDA is const in TABLE", {
  codex <- c(code, "$main F_A = 0.9;", "$table F_A = 0.5;")
  a <- mtry("frda-table", codex, recover = TRUE)
  expect_match(a$out$stderr, "declared const here",  all = FALSE)
})

test_that("ETA is const in MAIN", {
  codex <- c(code, "$main ETACL = 1.2;")
  a <- mtry("frda-table", codex, recover = TRUE)
  expect_match(a$out$stderr, "cannot assign to variable",  all = FALSE)
})

test_that("CMT_0 is const in TABLE", {
  codex <- c(code, "$table A_0 = 100;")
  a <- mtry("cmt-0-table", codex, recover = TRUE)
  expect_match(a$out$stderr, "declared const here",  all = FALSE)
})

test_that("FRDA is mutable in MAIN", {
  codex <- c(code, "$main F_A = 0.9;")
  a <- mtry("frda-main", codex, recover = TRUE)
  expect_is(a, "mrgmod")
})

test_that("CMT_0 is mutable in MAIN", {
  codex <- c(code, "$main A_0 = 900;")
  a <- mtry("cmt-0-main", codex, recover = TRUE)
  expect_is(a, "mrgmod")
})

code <- '
$PARAM PARAMX = 1
$CMT CMTA
$PLUGIN N_CMT
$PREAMBLE a = 1
$MAIN b = 2
$ODE @!audit
c = 3
$EVENT d = 4
$TABLE e = 5
'

test_that("All blocks get defs", {
  mod <- mcode("all-blocks-defs", code, compile = FALSE)
  raw <- readLines(mod@shlib$source)
  
  # All 5
  npar <- sum(grepl("PARAMX", raw))
  expect_equal(npar, 5)
  
  # Just 4 - not preamble
  ncmt <- sum(grepl(" CMTA =", raw, fixed = TRUE))
  expect_equal(ncmt, 4)
  
  # Just 4 - not preamble
  ninit <- sum(grepl(" CMTA_0 =", raw, fixed = TRUE))
  expect_equal(ninit, 4)
  
  # Just 3 - not preamble or ode
  nf <- sum(grepl(" F_CMTA =", raw, fixed = TRUE))
  expect_equal(nf, 3)
  nr <- sum(grepl(" R_CMTA =", raw, fixed = TRUE))
  expect_equal(nr, 3)
  nd <- sum(grepl(" D_CMTA =", raw, fixed = TRUE))
  expect_equal(nd, 3)
  na <- sum(grepl(" ALAG_CMTA =", raw, fixed = TRUE))
  expect_equal(na, 3)
  
  # Just 1 in global
  nn <- sum(grepl("N_CMTA =", raw, fixed = TRUE))
  expect_equal(nn, 1)
})
