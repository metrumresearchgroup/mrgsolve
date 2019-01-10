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

mod <- mrgsolve:::house() %>% update(atol = 1E-20, rtol = 1E-12, digits = 8)
mod1 <- update(mod %>% param(CL=12, VC=220), delta=33, end=222)
mod2 <- update(mod, param=list(CL=12, VC=220), delta=33, end=222)
x <- sort(runif(100, 0,300))
mod3 <- update(mod, add=x, end=1000, delta=20)
mod4 <- update(mod, init=list(CENT=111))
mod5 <- mod %>% param(VC=999)
mod6 <- mod %>% init(GUT=5566)
mod7 <- update(mod, hmin=111, hmax=222, maxsteps=333, 
               ixpr=444, mxhnil=555, atol=1E-99, rtol=1E-88)
mod8 <- mrgsolve:::mod(mrgsim(mod, delta=33, end=222))
mod9 <- update(mod, delta=33, end=222)
mod10 <- mrgsolve:::mod(mrgsim(mod, param=list(CL=12, VC=220)))
mod11 <- mrgsolve:::mod(mrgsim(mod  %>% param(CL=12, VC=220)))

context("test-update")

test_that("model object updates through update and %>% operator", {
  expect_true(identical(mod1, mod2))
  expect_true(!identical(mod1, mod))
})

test_that("Simulation times update properly via update",{
  expect_equal(stime(mod1), seq(0,mod1@end, mod2@delta))
  expect_equal(stime(mod3), sort(unique(c(x,seq(0,mod3@end, mod3@delta)))))
})

test_that("Simulation times update when passed into mrgsim",{
  expect_identical(mod8,mod9)
})

test_that("Parameter updates when passed to mrgsim",{
  expect_equal(param(mod10)$CL, param(mod2)$CL)
  expect_equal(param(mod10)$VC, param(mod2)$VC)
})

test_that("Parameter updates when added inside mrgsim call",{
  expect_equal(param(mod11)$CL, param(mod1)$CL)
  expect_equal(param(mod11)$VC, param(mod1)$VC)
})

rm(mod8,mod9,mod10,mod11)

test_that("Initials update via init and list",{
  mod1 <- mod %>% init(list(CENT=123,GUT=456))
  expect_equal(init(mod1)$CENT, 123)
  expect_equal(init(mod1)$GUT, 456)
})

test_that("Initials update via init ",{
  mod1 <- mod %>% init(CENT=1123, GUT=1456)
  expect_equal(init(mod1)$CENT, 1123)
  expect_equal(init(mod1)$GUT, 1456)
})

test_that("Initials update via init and data.frame ",{
  mod1 <- mod %>% init(data.frame(CENT=987,GUT=654))
  expect_equal(init(mod1)$CENT, 987)
  expect_equal(init(mod1)$GUT, 654)
})

test_that("Initial conditions update via update()",{
  expect_equal(init(mod6)$GUT,5566)
})

test_that("Solver setting hmin updates", {
  expect_equal(mod7@hmin,111)
})

test_that("Solver setting hmax updates", {
  expect_equal(mod7@hmax,222)
})

test_that("Solver setting maxsteps updates", {
  expect_equal(mod7@maxsteps,333)
})

test_that("Solver setting ixpr updates", {
  expect_equal(mod7@ixpr,444)
})

test_that("Solver setting mxhnil updates", {
  expect_equal(mod7@mxhnil,555)
})

test_that("Solver setting atol updates", {
  expect_equal(mod7@atol,1E-99)
})

test_that("Solver setting rtol updates", {
  expect_equal(mod7@rtol,1E-88)
})

CL <- exp(rnorm(100, log(3),  sqrt(0.5)))
VC <- exp(rnorm(100, log(30), sqrt(0.5))) 
pars <- signif(data.frame(CL=CL,VC=VC),6)
pars$ID <- seq(nrow(pars))

out <- mrgsim(mod, idata=pars, end=8, carry_out="CL,VC")
out <- out %>% as.tbl %>% distinct(ID, .keep_all=TRUE)
out <- signif(as.data.frame(out[,c("CL", "VC", "ID")]),6)

test_that("Recover items from simulated data when passed in as idata",{
  expect_equivalent(out,pars)
})

data <- expand.grid(time=seq(0,12,1), ID=1:100, cmt=1)
data <- merge(data, pars, sort=FALSE)

out <- mrgsim(mod, data=data, carry_out="CL,VC")
out <- out %>% as.tbl %>% distinct(ID, .keep_all=TRUE)
out <- signif(as.data.frame(out)[,c("CL", "VC", "ID")], 6)

test_that("Recover items from simulated data when passed in as data", {
  expect_equivalent(out,pars)
})

events <- ev(time=c(0,24,48), amt=1000, rate=50, 
             addl=c(0,0,10), ii=12,cmt=1)

out1 <- mrgsim(mod %>% ev(events), idata=data.frame(ID=1:20), end=200,
               carry_out="evid,amt,rate,addl,ii,cmt", req="")
data1 <- as.data.frame(out1)

out2 <- mrgsim(mod, data=data1, 
               carry_out="evid,amt,rate,addl,ii,cmt", req="")
data2 <- as.data.frame(out2)

test_that("CP is equal when simulating from events or data", {
  expect_identical(data1$CP, data2$CP)
})

set.seed(11111)
data1$ROW <- sample(1:nrow(data1))

out <- mrgsim(mod, data=data1, carry_out="ROW")

test_that("Time-varying data items in data are properly carried into output", {
  expect_true(all(data1$ROW == out$ROW))
})
