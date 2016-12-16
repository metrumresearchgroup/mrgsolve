library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("util functions")

test_that("columns are renamed", {
  expect_equal(names(mrgsolve:::rename_cols(Theoph, c("time" = "Time", "dv" = "conc"))),
               c("Subject", "Wt", "Dose", "time", "dv"))
})

test_that("columns are renamed and order preserved", {
  expect_equal(names(mrgsolve:::rename_cols(Theoph, c("dv" = "conc", "time" = "Time"))),
               c("Subject", "Wt", "Dose", "time", "dv"))
})

test_that("columns that don't exist throw an error", {
  expect_error(mrgsolve:::rename_cols(Theoph, c("dv" = "Donc")),
               "the following columns do not exist in the dataset:  Donc")
  expect_error(mrgsolve:::rename_cols(Theoph, c("dv" = "Donc", "id" = "subject")),
               "the following columns do not exist in the dataset:  Donc, subject")
})


context("Test utils")
mod <- mrgsolve:::house()
out <- mrgsim(mod)

test_that("Corecing simulated output to data.frame", {
  expect_is(as.data.frame(out), "data.frame")
})
test_that("Corecing simulated output to matrix", {
  expect_is(as.matrix(out), "matrix")
})
test_that("Corecing parameters to list", {
  expect_is(as.list(param(mod)), "list")
})
test_that("Corecing parameters to numeric", {
  expect_is(as.numeric(param(mod)), "numeric")
})
test_that("Corecing initials to list", {
  expect_is(as.list(init(mod)), "list")
})
test_that("Corecing initials to numeric", {
  expect_is(as.numeric(init(mod)), "numeric")
})
test_that("Corecing parameters to data.frame", {
  expect_is(as.data.frame(param(mod)), "data.frame")
  
})
test_that("Corecing initials to data.frame", {
  expect_is(as.data.frame(init(mod)), "data.frame")
})
test_that("stime correctly generates simulation times", {
  expect_equal(stime(mod), seq(0,mod@end, mod@delta))
  expect_equal(stime(update(mod, end=-1, add=c(1,6,9))), c(1,6,9))
  expect_error(stime(update(mod, end=-1, add=c())))
})
test_that("Negative end time gives simulations at add only", {
  expect_equal(stime(update(mod, end=-1, add=c(1,6,9))), c(1,6,9))
  expect_error(stime(update(mod, end=-1, add=c())))
})
test_that("If no simulation times can be rendered time=0 only is simulated", {
  out <- mrgsim(mod, end=-1, add=numeric(0))
  expect_equal(unique(out$time),0)
})




##' set_altname should return list with from/to
##' coercing to character returns x$from
##' renaming retains the order of the new character vector
test_that("altname", {
  x <- mrgsolve:::set_altname(c("a = Z", "c=J", "b   = B"))
  expect_identical(x$from, c("Z", "J", "B"))
  expect_identical(x$to, c("a", "c", "b"))
  res <- mrgsolve:::altname.altname(x, c(LETTERS))
  expect_identical(res[2],"b")
  expect_identical(res[26],"a")
  expect_identical(res[10],"c")
  expect_identical(mrgsolve:::as.character.altname(x),x$from)
  res2 <- mrgsolve:::altname.altname(x, rev(LETTERS))
  expect_identical(res,rev(res2))
})





