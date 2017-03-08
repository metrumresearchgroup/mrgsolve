# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.

library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

pars <- mrgsolve:::pars
cmt <- mrgsolve:::cmt


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
    $PARAM @annotated
    CL: 2 : Clearance (  L/hr)
    VC: 12 : Volume (L)
  '
  mod <- mcode("test-annot-1",code,compile=FALSE)
  expect_identical(pars(mod), c("CL", "VC"))
  expect_equivalent(as.numeric(param((mod))), c(2,12))
})

test_that("Full specification - $THETA", {
  
  code <- '
  $THETA  @annotated @name TH
  2 : Clearance (  L/hr)
  12 : Volume (L)
  '
  mod <- mcode("test-annot-1b",code,compile=FALSE)
  expect_identical(pars(mod), c("TH1", "TH2"))
  expect_equivalent(as.numeric(param((mod))), c(2,12))
})


test_that("Full specification - $CMT", {
  
  code <- '
    $CMT @annotated
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
  $INIT @annotated
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
  $FIXED @annotated
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
  $VCMT @annotated
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

test_that("Full specification - $CAPTURE", {
  
  code <- '
  $MAIN
  double a = 2;
  double b = 3;
  double c = 4;
  
  $CAPTURE @annotated
  a: first  (x)
  b: second (y)
  c: third  (z)
  '
  mod <- mcode("test-annot-5",code,compile=FALSE)
  expect_identical(mod@capture, c("a", "b", "c"))
  an <- mrgsolve:::details(mod) %>% (dplyr::bind_rows)
  expect_equivalent(an$unit, c("x", "y", "z"))
  expect_equivalent(an$descr, c("first", "second", "third"))
  expect_true(all(an$block =="CAPTURE"))
})
