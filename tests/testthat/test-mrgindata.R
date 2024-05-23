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

context("test-mrgindata")

end <- 60
delta <- 3
n <- length(seq(0,end,delta))
mod <- mrgsolve::house() %>% update(end=end, delta=delta)
data(extran3)

test_that("valid_data_set warns for character columns", {
  chr_param <- expand.ev(amt=100,ID=1:4, CL = "A")
  expect_error(
    valid_data_set(chr_param,m=mod), 
    regexp = "data set column: CL (character)", 
    fixed = TRUE
  )
  chr_X <- expand.ev(amt=100,ID=1:4,X="A")
  expect_silent(valid_data_set(chr_X,m=mod))
  chr_rate <- expand.ev(amt=100,rate="A")
  expect_error(
    valid_data_set(chr_rate,m=mod), 
    regexp = "data set column: rate (character)", 
    fixed = TRUE
  )
  chr_cmt <- expand.ev(amt=100,cmt="GUT")
  expect_silent(valid_data_set(chr_cmt,m=mod))
})

test_that("valid_idata_set warns for character columns", {
  chr_param <- expand.idata(FOO = 1, CL = "A")
  expect_error(
    valid_idata_set(chr_param,m=mod), 
    regexp = "data set column: CL (character)", 
    fixed = TRUE
  )
  chr_X <- expand.idata(FOO = 1, X = "A")
  expect_silent(valid_idata_set(chr_X,m=mod))
})


test_that("valid_data_set subs character cmt", {
  dat <- expand.ev(amt=100,ID=1:4,cmt="RESP")
  x <- valid_data_set(dat,mod)
  expect_true(all(x[,"cmt"] ==3))
  
  dat$cmt <- "GUT"
  x <- valid_data_set(dat,mod)
  expect_true(all(x[,"cmt"] ==1))
  
})

test_that("Run with no input", {
  expect_equal(length(stime(mod)),n)
  expect_equal(nrow(mrgsim(mod)),n)
})

test_that("Run with no input - and nid", {
  out <- mrgsim(mod, nid=31)
  expect_equal(nrow(out),n*31)
})

test_that("Run ev event", {
  e <- ev(amt=100)
  out <- mod %>% ev(e) %>% mrgsim
  expect_equal(nrow(out),n+1)
  
  e <- as.ev(expand.ev(time=c(2,4,6),amt=100))
  out <- mod %>% ev(e) %>% mrgsim
  expect_equal(nrow(out),3*n+3)
})

test_that("Run ev event - character cmt", {
  e <- ev(amt=100,cmt="CENT")
  out <- mod %>% ev(e) %>% mrgsim
  expect_equal(nrow(out),n+1)
  expect_equal(out$CENT[2],100)
})

test_that("Run bad data sets", {
  d <- data.frame(amt=100)
  expect_error(mrgsim(mod,data=d))
  d$cmt <- "FOO"
  expect_error(mrgsim(mod,data=d))
  
  d$cmt <- 1
  d$time <- 0
  expect_error(mrgsim(mod,data=d))
  
  d$evid <- 1
  expect_error(mrgsim(mod,data=d))
  
  d$ID <- 10
  expect_is(mrgsim(mod,data=d),"mrgsims")
  
})

test_that("Run ev event - and nid", {
  e <- ev(amt=100)
  out <- mod %>% ev(e) %>% mrgsim(nid=7)
  expect_equal(nrow(out),7*n+7)
  
  e <- as.ev(expand.ev(time=c(2,4,6),amt=100) %>% dplyr::mutate(ID=NULL))
  out <- mod %>% ev(e) %>% mrgsim(nid=5)
  expect_equal(nrow(out),5*n+(3*5))
})


test_that("Run with data set - data.frame", {
  out <- mod %>% data_set(extran3) %>% mrgsim
  expect_equal(nrow(out),nrow(extran3))
})

test_that("Run with data set - tibble", {
  data <- filter(extran3, ID <= 3)
  out <- mod %>% data_set(as_tibble(data)) %>% mrgsim
  expect_equal(nrow(out),nrow(data))
})

data(exidata)

test_that("Run idata set", {
  out <- mod %>% idata_set(exidata) %>% mrgsim
  expect_equal(nrow(out), length(unique(exidata$ID))*n) 
  expect_equal(length(unique(out$ID)),nrow(exidata))
  
})

test_that("Run idata set with ev", {
  e <- ev(amt=100)
  N <- length(unique(exidata$ID))
  N <- N*n + N
  
  out <- mod %>% ev(e) %>% idata_set(exidata) %>% mrgsim
  expect_equal(nrow(out), N) 
  expect_equal(length(unique(out$ID)),nrow(exidata))
})

test_that("Duplicate ID in idata_set gives error", {
  idata <- dplyr::mutate(tibble(ID=rep(seq(10),each=5)),CL=2)
  expect_error(valid_idata_set(idata))
})

test_that("integer64 columns are dropped from data_set [SLV-TEST-0011]", {
  skip_if_not_installed("bit64")  
  skip_if_not_installed("data.table")
  data <- data.table::fread("ID,KA\n1,100000020200")
  data$TIME <- 1
  data$CMT <- 1
  expect_error(
    valid_data_set(data, mod), 
    regexp = "data set column: KA (integer64)", 
    fixed = TRUE
  )
  data$KA <- as.double(data$KA)
  class(data$KA) <- "object"
  expect_error(
    valid_data_set(data, mod), 
    regexp = "data set column: KA (object)", 
    fixed = TRUE
  )
})

test_that("integer64 columns are dropped from idata_set  [SLV-TEST-0012]", {
  skip_if_not_installed("bit64")  
  skip_if_not_installed("data.table")
  data <- data.table::fread("ID,KA\n1,100000020200")
  data$TIME <- 1
  data$CMT <- 1
  expect_error(
    valid_idata_set(data, mod), 
    regexp = "data set column: KA (integer64)", 
    fixed = TRUE
  )
})

test_that("NA in nm-tran data items are converted to zeros [SLV-TEST-0020]", {
  data <- expand.ev(amt = 100, ii = 12, addl = 2, rate = 5, ss = 1, ID = 1:2) 
  tmp <- data
  tmp$ss[2] <- NA_real_

  flagged <- mrgsolve:::check_column_na(tmp, c("ss", "aa", "II", "SS"))
  expect_equal(flagged, "ss")

  tst <- valid_data_set(tmp, house())
  expect_equal(tst[, "ss"], c(1, 0))
    
  tmp2 <- tmp
  tmp2$FOO <- NA_real_
  flagged <- mrgsolve:::check_column_na(tmp2, mrgsolve:::GLOBALS$TRAN_FILL_NA)
  expect_equal(flagged, "ss")

  for(col in c("AMT", "cmt", "ii", "SS", "RATE", "evid")) {
    tmp <- data
    tmp[[col]] <- c(1, NA_real_)
    ans <- mrgsolve:::fill_tran_na(tmp)
    expect_equal(ans[[col]], c(1, 0))
  }
})
