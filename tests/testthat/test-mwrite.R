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
  
  expect_length(l$omega, 3)
  expect_equal(names(l$sigma), c("data", "labels", "names"))
  expect_is(l$omega$data, "list")
  expect_length(l$omega$data, 1)
  expect_length(l$omega$data[[1]], 10)
  expect_length(l$omega$labels, 1)
  expect_length(l$omega$labels[[1]], 4)
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
  mod <- modlib("popex", compile = FALSE)
  temp <- tempfile()
  mwrite_yaml(mod, temp)
  yaml_to_cpp(temp, model = "bar", project = tempdir())
  file <- file.path(tempdir(), "bar.mod")
  expect_true(file.exists(file))
  mod <- mread(file, compile = FALSE)
  expect_is(mod, "mrgmod")  
  
  yaml_to_cpp(temp, model = "bar", project = tempdir(), update = TRUE)
  x <- readLines(file)
  expect_match(x, "hmax = 0", all = FALSE, fixed = TRUE)
})


test_that("mwrite, mread json", {
  skip_if_not_installed("jsonlite")
  temp <- tempfile()
  mod <- modlib("pbpk", compile = FALSE)
  mod <- update(mod, add = c(1,2,3)/10)
  mod <- param(mod, Kpte = 1e6, F = 0.1)
  mwrite_json(mod, temp)
  expect_true(file.exists(temp))
  json <- readLines(temp)
  l <- jsonlite::fromJSON(json)
  expect_equal(l$format, "json")
  expect_equal(l$version, 1)
  expect_equal(l$param$BW, 70)
  
  # Read back in
  mod2 <- mread_json(temp, compile = FALSE)
  expect_identical(param(mod), param(mod2))
  expect_identical(stime(mod), stime(mod2))
  
  # Switch up model name
  mod3 <- mread_json(temp, model = "foo", compile = FALSE)
  expect_equal(mod3@model, "foo_mod")
  
  # We can pass capture through
  mod4 <- mread_json(
    temp, compile = FALSE, capture = "BW"
  )
  expect_true("BW" %in% outvars(mod4)$capture)
})

test_that("json_to_cpp", {
  mod <- modlib("popex", compile = FALSE)
  temp <- tempfile()
  mwrite_json(mod, temp)
  json_to_cpp(temp, model = "yak", project = tempdir())
  file <- file.path(tempdir(), "yak.mod")
  expect_true(file.exists(file))
  mod <- mread(file, compile = FALSE)
  expect_is(mod, "mrgmod")  
  
  json_to_cpp(temp, model = "bar", project = tempdir(), update = TRUE)
  x <- readLines(file)
  expect_match(x, "ixpr = 0", all = FALSE, fixed = TRUE)
})
