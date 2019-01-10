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

context("test-deslist")

test_that("as_deslist", {

  idata <- tibble(ID=c(33,11,88), start=0, end=c(1,2,3))
  des <- as_deslist(idata,descol="ID")
  expect_identical("ID", attr(des, "descol"))

  ##' ID 88
  expect_equal(max(stime(des[[3]])),3)
  ##' ID 11
  expect_equal(max(stime(des[[1]])),2)
  ##' ID 33
  expect_equal(max(stime(des[[2]])),1)
  
  mod <- mrgsolve:::house()
  
  out <- 
    mod %>%
    idata_set(idata) %>%
    design(des) %>%
    mrgsim()
                
  expect_is(out,"mrgsims")
  expect_equal(out$time, c(0,1,0,1,2,0,1,2,3))
  
  out <- mrgsim(mod,idata=idata,deslist=des,descol="ID")
  expect_equal(out$time, c(0,1,0,1,2,0,1,2,3))
  
})

test_that("tgrid_id", {
   idata <- data.frame(ID=c(3,1,2,4))
   ans <- mrgsolve:::tgrid_id("ID",idata)
   expect_equal(ans,c(2,0,1,3))
})

test_that("tgrid_matrix", {
  l <- list(seq(1,3),tgrid(0,4,1),c(tgrid(0,2,1),tgrid(6,12,3)))
  ans <- mrgsolve:::tgrid_matrix(l)
  expect_equal(ans[,1],c(1,2,3,NA,NA,NA))
  expect_equal(ans[,2],c(stime(l[[2]]),NA))
  expect_equal(ans[,3],stime(l[[3]]))
})

