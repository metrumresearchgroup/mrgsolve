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

context("test-dplyr-generics")

project <- file.path(system.file(package="mrgsolve"), "models")

out  <- mrgsolve:::house() %>% ev(amt=100) %>% mrgsim(end=122)

test_that("Pipe to tibble", {
  expect_is(out %>% as.tbl, "tbl_df")
  expect_is(out %>% as_tibble, "tbl_df")
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
  x <- out %>% dplyr::select(ID,RESP,time)
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

test_that("filter_mrgsims", {
  x <- out %>% filter_sims(time > 30)
  expect_is(x, "mrgsims")
  expect_true(all(x$time > 30))
})

test_that("mutate_mrgsims", {
  chk <- out$CENT + out$RESP
  x <- out %>% mutate_sims(foo = CENT + RESP)
  expect_is(x, "mrgsims")
  expect_true(all(x$foo == chk))
})
