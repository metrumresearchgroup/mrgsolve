# Copyright (C) 2013 - 2026  Metrum Research Group
#
# This file is part of mrgsolve.

library(testthat)
library(mrgsolve)

context("test-complete_tran")

test_that("minimal data.frame gets expected lowercase columns", {
  data <- data.frame(x = c(1, 2))
  out <- complete_tran(data)
  expect_identical(
    names(out),
    c("x", "id", "time", "amt", "cmt", "evid", "rate", "ii", "addl", "ss")
  )
  expect_identical(out$id, c(1L, 1L))
  expect_identical(out$cmt, c(1L, 1L))
  expect_identical(out$time, c(0, 0))
})

test_that("existing values are preserved", {
  data <- data.frame(time = c(3, 7), amt = c(100, 200), cmt = c(2L, 3L))
  out <- complete_tran(data)
  expect_identical(out$time, data$time)
  expect_identical(out$amt, data$amt)
  expect_identical(out$cmt, data$cmt)
})

test_that("uppercase mode adds uppercase columns", {
  data <- data.frame(x = 1:2)
  out <- complete_tran(data, case = "upper")
  expect_true(all(c("ID", "TIME", "AMT", "CMT", "EVID", "RATE", "II", "ADDL", "SS") %in% names(out)))
  expect_identical(out$ID, c(1L, 1L))
})

test_that("id FALSE does not add ID or id", {
  out1 <- complete_tran(data.frame(time = 0), id = FALSE)
  out2 <- complete_tran(data.frame(TIME = 0), case = "upper", id = FALSE)
  expect_false(any(c("id", "ID") %in% names(out1)))
  expect_false(any(c("id", "ID") %in% names(out2)))
})

test_that("preserve infers uppercase when uppercase columns are present", {
  out <- complete_tran(data.frame(TIME = 0, AMT = 100), case = "preserve")
  expect_true(all(c("CMT", "EVID", "RATE", "II", "ADDL", "SS", "ID") %in% names(out)))
})

test_that("preserve defaults to lowercase when no clear convention exists", {
  out <- complete_tran(data.frame(wt = c(50, 60)), case = "preserve")
  expect_true(all(c("id", "time", "amt", "cmt", "evid", "rate", "ii", "addl", "ss") %in% names(out)))
})

test_that("row order is preserved", {
  data <- data.frame(rec = c(3, 1, 2), amt = c(0, 100, 0))
  out <- complete_tran(data)
  expect_identical(out$rec, c(3, 1, 2))
})

test_that("invalid input errors", {
  expect_error(complete_tran(1), "data frame")
  expect_error(complete_tran(data.frame(x = 1), id = c(TRUE, FALSE)), "TRUE or FALSE")
})

test_that("existing columns are not overwritten when both cases exist", {
  data <- data.frame(time = 1, TIME = 2, amt = 10, AMT = 20)
  out <- complete_tran(data, case = "lower")
  expect_identical(out$time, 1)
  expect_identical(out$TIME, 2)
  expect_identical(out$amt, 10)
  expect_identical(out$AMT, 20)
})

test_that("works with tibble", {
  skip_if_not_installed("tibble")
  data <- tibble::tibble(time = c(0, 24))
  out <- complete_tran(data)
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("id", "amt", "cmt", "evid", "rate", "ii", "addl", "ss") %in% names(out)))
})
