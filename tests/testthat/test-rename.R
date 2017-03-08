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


mod <- mrgsolve:::house() %>% Req(CENT,RESP) 

data(exTheoph)
df <- exTheoph %>% mutate(KYLE=7)
mod <- mod %>% data_set(df) 



context("Rename via carry.out #30")
test_that("tran item is renamed", {
  out <- mod %>% carry.out(EVID=evid) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,EVID), names(out))))
  out <- mod %>% carry.out(EVID=evid,addl) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,EVID,addl), names(out))))
  out <- mod %>% carry.out(X=addl) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,X), names(out))))
  out <- mod %>% carry.out(EVID=evid,addl) %>% mrgsim
  expect_false(all(is.element(s(evid), names(out))))
})


test_that("Item carried from data set is renamed", {
  out <- mod %>% carry.out(Dose,WEIGHT = WT) %>% mrgsim
  expect_true(all(is.element(s(RESP,CENT,WEIGHT), names(out))))
  expect_true(all(is.element(s(Dose,WEIGHT), names(out))))
})


test_that("Item carried from data set is renamed", {
  out <- mod %>% carry.out(FOO=BAR) %>% mrgsim
  expect_equal(s(ID,time,CENT,RESP), names(out))
})










