# Copyright (C) 2013 - 2020  Metrum Research Group
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

context("test-mrgsims")

test_that("mrgsims class", {
  mod <- mrgsolve::house()   
  out <- mrgsim(mod)
  expect_true(is.mrgsims(out))
  expect_is(as.list(out), "mrgsims_list")
})

test_that("plot from character", {
  out <- mrgsolve::house() %>% mrgsim()
  pl1 <- plot(out, " CENT DV, CP")
  pl2 <- plot(out, CENT+DV+CP~time)
  expect_is(pl1,"trellis")
  expect_is(pl2,"trellis")
})

test_that("plot - fixed y scale", {
  mod <- house()
  out <- mrgsim(mod, end = 0, outvars = "CP") 
  
  p <- plot(out)
  expect_equal(p$y.scale$relation, "free")

  p <- plot(out, fixy = TRUE)
  expect_equal(p$y.scale$relation, "same")
  
  p <- plot(out, fixy = FALSE)
  expect_equal(p$y.scale$relation, "free")
  
  p <- plot(out, scales = "free")
  expect_equal(p$y.scale$relation, "free")
  
  p <- plot(out, scales = "same")
  expect_equal(p$y.scale$relation, "same")
  
  p <- plot(out, scales = list())
  expect_equal(p$y.scale$relation, "same")
  
  p <- plot(out, scales = list(), fixy = FALSE)
  expect_equal(p$y.scale$relation, "free")
  
  sc <- list(y = list(relation = "free"))
  p <- plot(out, scales = sc)
  expect_equal(p$y.scale$relation, "free")

  sc <- list(y = list(relation = "same"))
  p <- plot(out, scales = sc)
  expect_equal(p$y.scale$relation, "same")
  
})


test_that("plot - log y scale", {
  mod <- house(init = list(CENT = 100))
  out <- mrgsim(mod, end = 1, outvars = "CP") 
  
  p <- plot(out)
  expect_false(p$y.scale$log)
  
  p <- plot(out, logy = TRUE)
  expect_true(p$y.scale$log)
  
  p <- plot(out, logy = FALSE)
  expect_false(p$y.scale$log)

})
