library(mrgsolve)
library(testthat)
require(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

project <- file.path(system.file(package="mrgsolve"), "models")

context("Use dplyr generics on mrgsims objects")

out  <- mrgsolve:::house() %>% ev(amt=100) %>% mrgsim(end=122)

test_that("Pipe to as.tbl", {
  expect_is( out %>% as.tbl, "tbl_df")
})


test_that("Pipe to mutate", {
  x <- out %>% mutate(test=2)
  expect_is(x, "tbl_df")
  expect_true(exists("test", x))
  expect_true(all(x$test==2))
})


test_that("Pipe to filter", {
  x <- out %>% filter(time <=10)
  expect_is(x, "tbl_df")
  expect_true(max(x$time)==10)
})


test_that("Pipe to summarise", {
  x <- out %>% summarise(max=max(time))
  expect_is(x, "tbl_df")
  expect_true(nrow(x)==1)
  expect_true(x$max==122)
})




test_that("Pipe to select", {
  x <- out %>% select(ID,RESP,time)
  expect_is(x, "tbl_df")
  expect_identical(names(x),c("ID","RESP", "time"))
})



test_that("Pipe to group_by", {
  x <- out %>% group_by(ID,RESP)
  expect_is(x, "tbl_df")
  expect_identical(as.character(groups(x)),c("ID","RESP"))
})



test_that("Pipe to slice", {
  x <- out %>% slice(c(6,11))
  expect_is(x, "data.frame")
  expect_true(all(x$time %in% c(1,2.25)))
})






