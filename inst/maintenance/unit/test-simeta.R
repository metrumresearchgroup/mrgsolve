# Copyright (C) 2013 - 2022  Metrum Research Group
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

context("test-simeta.R")

code <- '
$PARAM n = 0, m = 0, mode = 1
$OMEGA 2 2 2 1 1 1 1 1 1 1 1
$SIGMA 2 2

$MAIN
if(mode==0) {
  capture a = ETA(1); 
  capture b = ETA(2); 
  capture c = ETA(3);  
  capture d = ETA(11);
}

if(mode==1) { 
  simeta();
  a = ETA(1); 
  b = ETA(2); 
  c = ETA(3);
}

if(mode==2) {
  simeta(n); 
  a = ETA(1); 
  b = ETA(2); 
  c = ETA(3);
}

if(mode==3) {
  simeta(n); 
  simeta(m);
  a = ETA(1); 
  b = ETA(2); 
  c = ETA(3);
}

$ERROR
if(mode==4) {
  simeps(); 
  a = EPS(1); 
  b = EPS(2);
}

if(mode==5) {
  simeps(n);
  a = EPS(1);
  b = EPS(2);
  c = 9;
}
'

mod <- mcode("simeta-n", code, end = 6, delta = 2)

test_that("resimulate all eta", {
  
  # simeta with no argument
  set.seed(1234)
  all <- mrgsim_df(mod) 
  all$ID <- NULL
  all$d <- NULL
  expect_false(any(duplicated(unlist(all))))
  
  # Setting n = 0 is the same as no argument
  set.seed(1234)
  all2 <- mrgsim_df(mod, param = list(n = 0, mode = 2))
  all2$ID <- NULL
  all2$d <- NULL
  expect_identical(all, all2)
  
})

test_that("resimulate all or specific eps", {
  data <- data.frame(amt = 0, evid = 0, time = c(0,0,0), cmt = 0, ID = 1)
  set.seed(87654)
  all <- mrgsim_df(mod, data = data, param = list(mode = 4))
  all$ID <- NULL
  all$c <- NULL
  all$d <- NULL
  all$time <- NULL
  expect_false(any(duplicated(unlist(all))))
})

test_that("warn when simeta(n) is called with off diagonals", {
  code <- '
  $OMEGA @block 
  1 0.1 2
  $MAIN 
  simeta(2);
  ' 
  expect_warning(
    mcode("simeta-n-warn", code, compile = FALSE), 
    regexp = "values is now discouraged and will soon be deprecated", 
    fixed  = TRUE
  )
  code <- '
  $OMEGA @block 
  1 0.1 2
  $MAIN 
  simeta();
  ' 
  expect_silent(mcode("simeta-n-nowarn-1", code, compile = FALSE))
  code <- '
  $ENV MRGSOLVE_RESIM_N_WARN = FALSE
  $OMEGA @block 
  1 0.1 2
  $MAIN 
  simeta(2);
  ' 
  expect_silent(mcode("simeta-n-nowarn-2", code, compile = FALSE))
})

test_that("warn when simeps(n) is called with off diagonals", {
  code <- '
  $SIGMA @block 
  1 0.1 2
  $TABLE
  simeps(2);
  ' 
  expect_warning(
    mcode("simeps-n-warn", code, compile = FALSE), 
    regexp = "values is now discouraged and will soon be deprecated", 
    fixed  = TRUE
  )
  code <- '
  $SIGMA @block 
  1 0.1 2
  $TABLE
  simeps();
  ' 
  expect_silent(mcode("simeps-n-nowarn", code, compile = FALSE))
  code <- '
  $ENV MRGSOLVE_RESIM_N_WARN = FALSE
  $SIGMA @block 
  1 0.1 2
  $TABLE
  simeps(1);
  ' 
  expect_silent(mcode("simeps-n-nowarn-2", code, compile = FALSE))
})

# Fixes issue discovered in #1092
test_that("etasrc works with ETA in first column", {
  mod <- param(mod, mode = 0)
  
  data <- expand.ev(amt = 100, ID = seq(4), cmt = 1)
  data <- dplyr::mutate(data, ETA1 = rev(ID) / 10)
  data <- expand_observations(data, times = seq(5))
  data <- dplyr::mutate(data, cmt = 0)
  
  set.seed(9812)
  expect_identical(
    mrgsim(mod, data, etasrc = "data"),
    mrgsim(mod, dplyr::select(data, "ETA1", tidyselect::everything()), 
           etasrc = "data")
  )
})

test_that("pass ETA on the data set", {
  mod <- param(mod, mode = 0)
  data <- expand.ev(amt = 100, ID = seq(4), cmt = 1)
  data <- mutate(
    data,
    ETA1 = rev(ID)/10,
    ETA3 = ETA1
  )
  data <- expand_observations(data, times = seq(5))
  data <- mutate(data, ETA3 = ifelse(time > 0, -1, ETA3))
  data <- mutate(data, cmt = 0)
  
  set.seed(9812)
  out <- mrgsim(mod, data, etasrc = "data")
  expect_true(all(out$b==0))
  expect_true(all(out$a==out$c))
  summ_out <- count(as.data.frame(out), ID, a, b, c)
  summ_dat <- count(data, ID, ETA1)
  expect_equivalent(summ_out$a, summ_dat$ETA1)
  
  set.seed(123)
  out1 <- mrgsim(mod, data, etasrc = "data")
  set.seed(456)
  out2 <- mrgsim(mod, data, etasrc = "data")
  expect_identical(out1, out2)
  
  expect_error(
    mrgsim(mod, data, etasrc = "foo"),
    regexp = "`etasrc` must be either"
  )
  
  expect_error(
    mrgsim(mod, data, etasrc = c("data", "foo")), 
    regexp = "must be a string"
  )
  
  expect_error(
    mrgsim(mod, data, etasrc = "data.all"), 
    regexp = "all 11 ETAs"
  )
  
  data2 <- data
  data2$ETA1 <- data2$ETA3 <- NULL
  
  expect_error(
    mrgsim(mod, data2, etasrc = "data"), 
    regexp = "at least one ETA must"
  )
  
  data$ETA11 <- 11
  out <- mrgsim(mod, data, etasrc = "data")
  expect_true(all(out$d==11))
  
  data$ET11 <- 1111
  expect_error(
    mrgsim(mod, data, etasrc = "data"), 
    regexp = "Ambiguous ETA names"
  )
  
  data <- data.frame(
    amt = 10, evid = 2, cmt = 0,
    time = c(0,0.5,1,0,0.5,0), 
    ID = c(1,1,1,2,2,3), 
    ETA1 = c(1,1,1,2,2,3)/10,
    ETA2 = c(1,1,1,2,2,3)*2,
    mode = 0
  )
  
  mod <- update(mod, end = 2, delta = 1, start = 1)
  
  out <- mrgsim(mod, data, etasrc = "data")
  expect_equal(nrow(out), 12)
  expect_true(all(out$a == out$ID/10))
  expect_true(all(out$b == out$ID*2))
  
  outq <- mrgsim_q(mod, data, etasrc = "data")
  expect_identical(out@data, outq@data)
})

# Keeping tests simpler here since there is shared code
test_that("pass ETA on the idata set", {
  mod <- param(mod, mode = 0)
  data <- expand.ev(amt = 100, ID = seq(4), cmt = 1)
  idata <- mutate(
    data,
    ETA1 = rev(ID)/10,
    ETA3 = ETA1
  )[, c("ID", "ETA1", "ETA3")]
  
  data <- expand_observations(data, times = seq(5))
  data <- mutate(data, cmt = 0)
  data2 <- merge(data, idata, by = "ID")
  
  # Show that output is identical using data or idata
  set.seed(9812)
  out1 <- mrgsim(mod, data2, etasrc = "data")
  out2 <- mrgsim(mod, data, idata, etasrc = "idata")
  
  expect_identical(out1, out2)
  
  expect_error(
    mrgsim(mod, data, idata, etasrc = "idata.all"), 
    "all 11 ETAs must"
  )
  
  expect_error(
    mrgsim(mod, data, idata = data[, "ID"], etasrc = "idata"), 
    "at least one"
  )
  
  idata$ETA3 <- rev(idata$ETA3)
  out <- mrgsim_i(mod, idata, etasrc = "idata", output = "df")
  out <- out[out$time==4,]
  expect_identical(out$a, idata$ETA1)
  expect_identical(out$c, idata$ETA3)
  expect_identical(out$b, rep(0, nrow(idata)))
  expect_identical(out$d, rep(0, nrow(idata)))
  
  # Confirm that ETA is coming from idata, not data
  data$ETA1 <- -1
  data$ETA3 <- -1
  data$ETA2 <- 100
  data$ETA4 <- 200
  out <- mrgsim_di(mod, data, idata, etasrc = "idata", output = "df")
  out <- out[out$time==4,]
  expect_identical(out$a, idata$ETA1)
  expect_identical(out$c, idata$ETA3)
  expect_identical(out$b, rep(0, nrow(idata)))
  expect_identical(out$d, rep(0, nrow(idata)))
})


test_that("Reproducible EPS with etasrc=data gh-1138", { 
  data <- expand.ev(amt = 0, ETA1 = 0.1)
  mod <- modlib("popex", capture = "EPS(1)", end = 300)
  mod <- smat(mod, matrix(1))
  set.seed(11211)
  out1 <- mrgsim(mod, data)
  set.seed(11211)
  out2 <- mrgsim(mod, data, etasrc = "data")
  expect_identical(out1$EPS_1, out2$EPS_1)
})
