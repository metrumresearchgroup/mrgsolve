# Copyright (C) 2013 - 2021  Metrum Research Group
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

$OMEGA 2 2 2
$SIGMA 2 2

$MAIN

if(mode==1) { 
  simeta();
  capture a = ETA(1); 
  capture b = ETA(2); 
  capture c = ETA(3);
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
  expect_false(any(duplicated(unlist(all))))
  
  # Setting n = 0 is the same as no argument
  set.seed(1234)
  all2 <- mrgsim_df(mod, param = list(n = 0, mode = 2))
  all2$ID <- NULL
  expect_identical(all, all2)
  
})

test_that("resimulate specific eta", {
  
  set.seed(5678)
  # simeta with n = 1 (simeta(3))
  n1 <-  mrgsim_df(mod, param = list(n = 1, mode = 2)) 
  expect_true(all(n1$b==n1$b[1]))
  expect_true(all(n1$c==n1$c[1]))
  expect_false(any(duplicated(n1$a)))
  
  # simeta with n = 2 (simeta(2))
  n2 <-  mrgsim(mod, param = list(n = 2, mode = 2))
  expect_true(all(n2$a==n2$a[1]))
  expect_true(all(n2$c==n2$c[1]))
  expect_false(any(duplicated(n2$b)))
  
  # simeta with n = 3 (simeta(3))
  n3 <-  mrgsim(mod, param = list(n = 3, mode = 2))
  expect_true(all(n3$a==n3$a[1]))
  expect_true(all(n3$b==n3$b[1]))
  expect_false(any(duplicated(n3$c)))
  
  # simeta  with simeta(1) and simeta(3)
  n13 <- mrgsim(mod, param = list(n = 1, m = 3,  mode = 3))
  expect_true(all(n13$b==n13$b[1]))
  expect_false(any(duplicated(n13$a)))
  expect_false(any(duplicated(n13$c)))
  
})

test_that("resimulate all or specific eps", {
  data <- data.frame(amt = 0, evid = 0, time = c(0,0,0), cmt = 0, ID = 1)
  set.seed(87654)
  all <- mrgsim_df(mod, data = data, param = list(mode = 4))
  all$ID <- NULL
  all$c <- NULL
  all$time <- NULL
  expect_false(any(duplicated(unlist(all))))
  
  # simeps with n = 2 (simeps(2))
  n <-  mrgsim(mod, data = data, param = list(n = 2, mode = 5))
  expect_true(all(n$a==n$a[1]))
  expect_false(any(duplicated(n$b)))
})

test_that("invalid value for n when calling simeta or simeps", {
  expect_error(
    mrgsim(mod, param = list(mode = 2, n = 100)), 
    "simeta index out of bounds"
  )
  expect_error(
    mrgsim(mod, param = list(mode = 5, n = 100)), 
    "simeps index out of bounds"
  )
})
