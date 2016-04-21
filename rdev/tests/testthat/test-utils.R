context("util functions")

test_that("columns are renamed", {
  expect_equal(names(rename_cols(Theoph, c("time" = "Time", "dv" = "conc"))),
                c("Subject", "Wt", "Dose", "time", "dv"))
                })

test_that("columns are renamed and order preserved", {
  expect_equal(names(rename_cols(Theoph, c("dv" = "conc", "time" = "Time"))),
                c("Subject", "Wt", "Dose", "time", "dv"))
                })

test_that("columns that don't exist throw an error", {
  expect_error(rename_cols(Theoph, c("dv" = "Donc")),
              "the following columns do not exist in the dataset:  Donc")
  expect_error(rename_cols(Theoph, c("dv" = "Donc", "id" = "subject")),
               "the following columns do not exist in the dataset:  Donc, subject")
})
