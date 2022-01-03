# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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

project <- file.path(system.file(package="mrgsolve"), "models")

context("test-matlist")

code <- '
$OMEGA 0 0 0 
$SIGMA 0 0 0
'

mod <- mcode("test4",code,compile=FALSE)


mod <- mod %>% 
  omat(diag(c(1.2, 2.3,3.4))) %>% 
  smat(diag(c(0.1, 0.2, 0.3)))

test_that("Indexing OMEGA matrix elements", {
  expect_equivalent(as.matrix(omat(mod))[2,2],2.3)
  expect_equivalent(as.matrix(omat(mod))[2,1],0)
  expect_equivalent(as.matrix(omat(mod))[3,3],3.4)
})

test_that("Indexing SIGMA matrix elements", {
  expect_equivalent(as.matrix(smat(mod))[2,2],0.2)
  expect_equivalent(as.matrix(smat(mod))[2,1],0)
  expect_equivalent(as.matrix(smat(mod))[3,3],0.3)
})

a <- diag(c(1.1, 2.2, 3.3))
b <- diag(c(4.4, 5.5, 6.6))
c <- matrix(seq(91,99),nrow = 3, byrow = TRUE)
o1 <- omat(a,b,c)

mat <- as.matrix(o1)

test_that("Indexing OMEGA matrix elements with multiple matrices", {
    expect_equivalent(mat[3,3],3.3)
    expect_equivalent(mat[7,2],0)
    expect_equivalent(mat[7,7],91)
    expect_equivalent(mat[9,8],98)
})

test_that("Update a model with no matrix", {
  mod@omega <- omat()
  expect_is(omat(mod, list()), "mrgmod")
  expect_is(omat(mod, matrix(0,0,0)), "mrgmod")  
  expect_error(omat(mod, dmat(1,2,3)), "improper signature")
})

test_that("Update a model matrix", {
  expect_is(omat(mod, dmat(55,66,77)), "mrgmod")  
  expect_error(omat(mod, dmat(1)))  
})

test_that("valid matlist", {
  x <- omat(dmat(1,2,3))
  expect_true(mrgsolve:::valid.matlist(x))
})

test_that("new_omat", {
  x <- mrgsolve:::new_omat(dmat(1,2,3))
  expect_is(x,"omegalist")
  expect_equal(dim(x), list(`...`=c(3,3)))
})

test_that("new_smat", {
  x <- mrgsolve:::new_smat(dmat(1,2,3,4))
  expect_is(x,"sigmalist")
  expect_equal(dim(x), list(`...`=c(4,4)))
})

test_that("collapse_omega", {
  code <- '
  $OMEGA @labels a b
  1 2
  $OMEGA @labels c d e
  3 4 5
  $OMEGA @labels f
  6
  '
  mod <- mcode("collapse1", code, compile = FALSE)
  expect_equal(length(omat(mod)), 3)
  mod2 <- collapse_omega(mod)
  omat2 <- omat(mod2)
  expect_equal(length(omat2), 1)
  expect_equal(labels(omat2), list(c("a", "b", "c", "d", "e", "f")))
  mod3 <- collapse_omega(mod, range = c(2,3), name = "foo")
  omat3 <- omat(mod3)
  expect_equal(length(omat3), 2)
  expect_equal(names(omat3)[2], "foo")
  lab3 <- list(c("a", "b"), c("c", "d", "e", "f"))
  expect_equal(labels(omat3),lab3)
  mod4 <- collapse_omega(mod, range = c(2,NA), name = "foo")
  expect_identical(omat(mod4), omat3)
  expect_identical(as.matrix(omat2), as.matrix(omat(mod)))
  expect_identical(as.matrix(omat3), as.matrix(omat(mod)))
  expect_identical(as.matrix(omat(mod4)), as.matrix(omat(mod)))
  expect_error(
    collapse_omega(mod, range = c("a", "b")), 
    regexp = "must be numeric type"
  )
  expect_error(
    collapse_omega(mod, range = 1), 
    regexp = "length 2"
  )
  expect_error(
    collapse_omega(mod, range = c(3,2)), 
    regexp = "`range\\[2\\]` must be >"
  )
  expect_error(
    collapse_omega(mod, range = c(-1, 2)), 
    regexp = "must be > 0"
  )
  expect_error(
    collapse_omega(mod, range = c(1 , 100)), 
    regexp = "must be <= length\\(x\\)", 
  )
  expect_error(
    collapse_matrix(list(a = 1)), 
    regexp = "must be a `matlist` object"
  )
  code <- paste0(code, "\n", "$set collapse_omega = TRUE")  
  mod <- mcode("collapse_omega", code, compile = FALSE)
  mat <- as.matrix(omat(mod))
  expect_identical(dim(mat), c(6L,6L))
})

test_that("collapse_sigma", {
  code <- '
  $SIGMA @labels a b
  1 2
  $SIGMA @labels c d e
  3 4 5
  $OMEGA @labels f
  6
  '
  mod <- mcode("collapse2", code, compile = FALSE)
  expect_equal(length(smat(mod)), 2)
  mod2 <- collapse_sigma(mod)
  smat2 <- smat(mod2)
  expect_equal(nrow(as.matrix(smat2)), 5)
  expect_equal(length(smat2), 1)
  code <- "$sigma 1 2 \n$sigma 4 \n$set collapse_sigma=TRUE"  
  mod <- mcode("collapse_sigma", code, compile = FALSE)
  mat <- as.matrix(smat(mod))
  expect_identical(dim(mat), c(3L,3L))
})

test_that("test-matlist duplicate labels", {
  expect_error(c(omat(house()), omat(house())))  
})
