# Copyright (C) 2013 - 2024  Metrum Research Group
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

context("test-mwrite")

test_that("convert list to code", {
  # This could be simpler, but haven't found an easy way to do `c = "a"`
  l <- list(a = 1, b = 1.234, c = "a", d = c("x", "y", "z"), e = FALSE)  
  x <- mrgsolve:::tocode(l)  
  expect_equal(x[1], "a = 1")
  expect_equal(x[2], "b = 1.234")
  expect_equal(x[3], 'c = "a"')
  expect_equal(x[4], 'd = c("x", "y", "z")')
  expect_equal(x[5], "e = FALSE")
})

test_that("convert model to list", {
  mod <- house(end = 26, delta = 2, outvars = "GUT, DV, CP")
  l <- mrgsolve:::mwrite_model_to_list(mod)
  expect_equal(l$format, "list")
  expect_equal(l$version, 1)
  expect_equal(l$update$end, 26)
  expect_equal(l$update$delta, 2)
  expect_equal(l$update$outvars, c("GUT", "DV", "CP"))
  expect_equal(l$set$end, 120)
  expect_equal(l$set$delta, 0.25)
  
  nomega <- nrow(omat(mod))
  expect_length(l$omega, 3)
  expect_equal(names(l$sigma), c("data", "labels", "names"))
  expect_is(l$omega$data, "list")
  expect_length(l$omega$data, 1)
  expect_length(l$omega$data[[1]], nomega)
  expect_length(l$omega$labels, 1)
  expect_length(l$omega$labels[[1]],  nomega)
  expect_is(l$omega$labels, "list")
  expect_length(l$omega$names, 1)
  expect_is(l$omega$names, "character")
  
  mod2 <- mod
  mod2@omega <- omat()
  l <- mrgsolve:::mwrite_model_to_list(mod2)
  expect_length(l$omega, 3)
  expect_equal(names(l$sigma), c("data", "labels", "names"))
  expect_is(l$omega$data, "list")
  expect_length(l$omega$data, 0)
  expect_length(l$omega$labels, 0)
  expect_is(l$omega$labels, "list")
  expect_length(l$omega$names, 0)
  expect_is(l$omega$names, "character")
})

test_that("mwrite, mread yaml", {
  skip_if_not_installed("yaml")
  temp <- tempfile()
  mod <- modlib("pk1", compile = FALSE)
  mod <- update(mod, end = 16, delta = 8)
  mwrite_yaml(mod, temp)
  expect_true(file.exists(temp))
  yaml <- readLines(temp)
  l <- yaml::yaml.load(yaml)
  expect_equal(l$format, "yaml")
  expect_equal(l$version, 1)
  expect_equal(l$param$V, 20)
  
  # Read back in 
  mod2 <- mread_yaml(temp, compile = FALSE)
  expect_identical(param(mod), param(mod2))
  expect_identical(stime(mod), stime(mod2))
  
  # Switch up the model name
  mod3 <- mread_yaml(temp, model = "foo", compile = FALSE)
  expect_equal(mod3@model, "foo_mod")
  
  # We can pass capture through
  mod4 <- mread_yaml(
    temp, compile = FALSE, capture = "KA"
  )
  expect_true("KA" %in% outvars(mod4)$capture)
})

test_that("yaml_to_cpp", {
  skip_if_not_installed("yaml")
  mod <- modlib("popex", compile = FALSE)
  temp <- tempfile()
  mwrite_yaml(mod, temp)
  yaml_to_cpp(temp, model = "bar", project = tempdir())
  file <- file.path(tempdir(), "bar.mod")
  expect_true(file.exists(file))
  mod <- mread(file, compile = FALSE)
  expect_is(mod, "mrgmod")  
  x <- readLines(file)
  expect_no_match(x, "hmax = 0", all = FALSE, fixed = TRUE)
  
  # Request updates to be written
  yaml_to_cpp(temp, model = "bar", project = tempdir(), update = TRUE)
  x <- readLines(file)
  expect_match(x, "hmax = 0", all = FALSE, fixed = TRUE)
})

test_that("imposter code", {
  skip_if_not_installed("yaml")
  mod <- modlib("pk2", compile = FALSE)
  x <- mwrite_yaml(mod, file = NULL)
  x$source <- NULL
  temp <- tempfile()
  yaml <- yaml::as.yaml(x)
  cat(yaml, file = temp)
  expect_error(
    mread_yaml(temp, compile = FALSE), 
    "was not written by `mwrite_yaml()`.", 
    fixed = TRUE
  )
})

test_that("mwrite with no file", {
  skip_if_not_installed("yaml")
  l <- mwrite_yaml(house(), file = NULL)  
  expect_is(l, "list")
  expect_equal(l$format, "list")
  expect_error(mwrite_yaml(house()), "missing, with no default")
})

test_that("captures are handled", {
  skip_if_not_installed("yaml")
  # no names
  temp1 <- tempfile()
  code <- "$PARAM CL=1,V=2,KA=3\n$CAPTURE V CL"
  mod <- mcode("cap1", code, compile = FALSE)
  l <- mwrite_yaml(mod, file = temp1)
  expect_identical(l$capture, c("V", "CL"))
  m <- mread_yaml(temp1, compile = FALSE)
  expect_equivalent(m@capture, c("V", "CL"))
  
  # one renamed
  temp2 <- tempfile()
  code <- "$PARAM CL=1,V=2,KA=3\n$CAPTURE V a = CL"
  mod <- mcode("cap2", code, compile = FALSE)
  l <- mwrite_yaml(mod, file = temp2)
  expect_identical(l$capture, c("V", "a = CL"))
  m <- mread_yaml(temp2, compile = FALSE)
  expect_equivalent(m@capture, c(V = "V", CL = "a"))
  
  # all renamed
  temp3 <- tempfile()
  code <- "$PARAM CL=1,V=2,KA=3\n$CAPTURE b = V, a = CL"
  mod <- mcode("cap3", code, compile = FALSE)
  l <- mwrite_yaml(mod, file = temp3)
  expect_equivalent(l$capture, c("b = V", "a = CL"))
  m <- mread_yaml(temp3, compile = FALSE)
  expect_equivalent(m@capture, c(V = "b", CL = "a"))
})

test_that("handle multiple unnamed matrices", {
  skip_if_not_installed("yaml")
  temp <- tempfile()
  code <- '$OMEGA 1 2 3\n$OMEGA 3 4 5 6'
  mod <- mcode("foo", code, compile = FALSE)
  a <- mwrite_yaml(mod, file = temp)
  yam <- yaml::yaml.load_file(temp)$omega
  expect_equal(names(yam$data), paste0("matrix", 1:2))
  expect_equal(names(yam$labels), paste0("matrix", 1:2))
  expect_equal(names(yam$data$matrix1), paste0("row", 1:3))
  expect_equal(names(yam$data$matrix2), paste0("row", 1:4))
  expect_equal(a$file, temp)
  mod2 <- mread_yaml(file = temp, compile = FALSE)  
  expect_identical(mod@omega, mod2@omega)
})

test_that("matrix names are retained", {
  skip_if_not_installed("yaml")
  code <- '$OMEGA 1 2 3\n@name metrum\n$SIGMA 1 2\n @name rg @labels a b'
  mod <- mcode("foo", code, compile = FALSE)
  expect_equal(names(omat(mod)), "metrum")
  expect_equal(names(smat(mod)), "rg")
  temp <- tempfile()
  x <- mwrite_yaml(mod, file = temp)
  yam <- yaml::yaml.load_file(x$file)
  expect_equal(yam$omega$names, "metrum")
  expect_equal(yam$sigma$names, "rg")
  mod2 <- mread_yaml(temp, compile = FALSE)
  expect_equal(names(omat(mod2)), "metrum")
  expect_equal(names(smat(mod2)), "rg")
})

test_that("render matrix as list of numeric rows", {
  mat <- matrix(rnorm(25), nrow = 5, ncol = 5)  
  l <- mrgsolve:::get_upper_tri(mat)
  expect_equal(names(l), paste0("row", 1:5))
  for(j in 1:5) {
    expect_equal(l[[j]], mat[1:j, j])
  }
})

test_that("code gets appropriately quoted", {
  skip_if_not_installed("yaml")
  
  code <- '$SET ss_cmt = "B", outvars = "A", delta = 5\n$CMT A B'
  
  mod <- mcode("test-quote", code, compile = FALSE)
  temp <- tempfile()
  x <- mwrite_yaml(mod, file = temp)
  expect_equal(x$set$ss_cmt, "B")
  
  mod <- mread_yaml(temp, compile = FALSE)
  expect_is(mod, "mrgmod")
  ov <- outvars(mod)
  expect_equal(ov$cmt, "A")
  expect_equal(ov$capture, character(0))
  
  # Check that A is in quotes
  cpp <- yaml_to_cpp(file = temp, model = "test-quote", project = tempdir())
  lines <- readLines(cpp)
  expect_match(lines, 'outvars = "A"', all=FALSE)
})
