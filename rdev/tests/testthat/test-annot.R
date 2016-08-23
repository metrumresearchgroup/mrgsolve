library(mrgsolve)
library(testthat)
library(dplyr)


Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-annot")

test_that("parse line - name : value : text", {
    
    x <- "CL: 2 : Clearance (L/hr)"
    y <- mrgsolve:::parse_annot_line(x)
    expect_identical(y$unit, "L/hr")
    expect_identical(y$descr, "Clearance")
    expect_identical(y$value, "2")
    expect_identical(y$options, character(0))
    
    x <- "CL: 2 : (L/hr2) Clearance [k=1]"
    y <- mrgsolve:::parse_annot_line(x)
    expect_identical(y$unit, "L/hr2")
    expect_identical(y$descr, "Clearance")
    expect_identical(y$value, "2")
    expect_identical(y$options, "k=1")
    
    x <- "CL: 2 : Clearance"
    y <- mrgsolve:::parse_annot_line(x)
    expect_identical(y$unit, character(0))
    expect_identical(y$descr, "Clearance")
    expect_identical(y$value, "2")
    expect_identical(y$options, character(0))
    
    x <- "CL: 2 : . "
    y <- mrgsolve:::parse_annot_line(x)
    expect_identical(y$descr, ".")
    expect_identical(y$value, "2")
    
})

test_that("parse line - name  : text", {
  
  x <- "CL:  Clearance (L/hr)"
  y <- mrgsolve:::parse_annot_line(x, novalue=TRUE)
  
  expect_identical(y$unit, "L/hr")
  expect_identical(y$descr, "Clearance")
  expect_identical(y$value, "0")
  expect_identical(y$options, character(0))
  expect_error(mrgsolve:::parse_annot_line(x))
  
  x <- "CL:  (L/hr2) Clearance [k=1]"
  y <- mrgsolve:::parse_annot_line(x, novalue=TRUE)
  expect_identical(y$unit, "L/hr2")
  expect_identical(y$descr, "Clearance")
  expect_identical(y$value, "0")
  expect_identical(y$options, "k=1")
  
})


test_that("parse line - value  : text", {
  
  x <- "1: Clearance (L/hr)"
  y <- mrgsolve:::parse_annot_line(x, noname=TRUE)
  
  expect_identical(y$unit, "L/hr")
  expect_identical(y$descr, "Clearance")
  expect_identical(y$value, "1")
  expect_identical(y$options, character(0))
  expect_error(mrgsolve:::parse_annot_line(x))
  
})
