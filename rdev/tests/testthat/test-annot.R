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
    expect_identical(y$options, '.')
    
    x <- "CL: 2 : (L/hr2) Clearance [k=1]"
    y <- mrgsolve:::parse_annot_line(x)
    expect_identical(y$unit, "L/hr2")
    expect_identical(y$descr, "Clearance")
    expect_identical(y$value, "2")
    expect_identical(y$options, "k=1")
    
    x <- "CL: 2 : Clearance"
    y <- mrgsolve:::parse_annot_line(x)
    expect_identical(y$unit, '.')
    expect_identical(y$descr, "Clearance")
    expect_identical(y$value, "2")
    expect_identical(y$options, '.')
    
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
  expect_identical(y$options, '.')
  expect_error(mrgsolve:::parse_annot_line(x))
  
  x <- "CL:  (L/hr2) Clearance [  k=1  ]"
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
  expect_identical(y$options, '.')
  expect_error(mrgsolve:::parse_annot_line(x))
  
})

test_that("Full specification - $PARAM", {
  
  code <- '
    $PARAM >> annotated=TRUE
    CL: 2 : Clearance (  L/hr)
    VC: 12 : Volume (L)
  '
  mod <- mcode("test-annot-1",code,compile=FALSE)
  expect_identical(pars(mod), c("CL", "VC"))
  expect_equivalent(as.numeric(param((mod))), c(2,12))
})

test_that("Full specification - $THETA", {
  
  code <- '
  $THETA >> annotated=TRUE, name="TH"
  2 : Clearance (  L/hr)
  12 : Volume (L)
  '
  mod <- mcode("test-annot-1b",code,compile=FALSE)
  expect_identical(pars(mod), c("TH1", "TH2"))
  expect_equivalent(as.numeric(param((mod))), c(2,12))
})


test_that("Full specification - $CMT", {
  
  code <- '
    $CMT >> annotated=TRUE
    GUT : Dosing (  mg)
    CENT : Central (mg )
  '
  mod <- mcode("test-annot-2",code,compile=FALSE)
  expect_identical(cmt(mod), c("GUT", "CENT"))
  an <- mrgsolve:::details(mod) %>% (dplyr::bind_rows)
  expect_equivalent(an$unit, c("mg", "mg"))
  expect_equivalent(an$descr, c("Dosing", "Central"))
})

test_that("Full specification - $INIT", {
  
  code <- '
  $INIT >> annotated=TRUE
  GUT :  12.3 : Dosing (  mg)
  CENT : 45.6 : Central (mg )
  '
  mod <- mcode("test-annot-2b",code,compile=FALSE)
  expect_identical(cmt(mod), c("GUT", "CENT"))
  expect_equivalent(as.numeric(init(mod)), c(12.3, 45.6))
  an <- mrgsolve:::details(mod) %>% (dplyr::bind_rows)
  expect_equivalent(an$unit, c("mg", "mg"))
  expect_equivalent(an$descr, c("Dosing", "Central"))
  
})


test_that("Full specification - $FIXED", {
  
  code <- '
  $FIXED >> annotated=TRUE
  A: 1 : Letter-A (a)
  B: 2 : Letter-B (b)
  C: 3 : Letter-C (c)
  '
  mod <- mcode("test-annot-3",code,compile=FALSE)
  expect_identical(names(allparam(mod)), c("A", "B", "C"))
  expect_equivalent(as.numeric(allparam(mod)),c(1,2,3))
  an <- mrgsolve:::details(mod) %>% (dplyr::bind_rows)
  expect_equivalent(an$unit, c("a", "b", "c"))
  expect_equivalent(an$descr, c("Letter-A", "Letter-B", "Letter-C"))
})

test_that("Full specification - $VCMT", {
  
  code <- '
  $VCMT >> annotated=TRUE
  A: 123 (x)
  B: 456 (y)
  C: 789 (z)
  '
  mod <- mcode("test-annot-4",code,compile=FALSE)
  expect_identical(cmt(mod), c("A", "B", "C"))
  an <- mrgsolve:::details(mod) %>% (dplyr::bind_rows)
  expect_equivalent(an$unit, c("x", "y", "z"))
  expect_equivalent(an$descr, c("123", "456", "789"))
})
