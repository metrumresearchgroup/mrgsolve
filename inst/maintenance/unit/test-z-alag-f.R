# Copyright (C) 2013 - 2023  Metrum Research Group
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

lim <- function(x,...) as.data.frame(dplyr::filter(x, ...))


context("Set F via F_CMT")

code <- '
$PARAM F1=1, ALAG1=0, F2=1, ALAG2=0

$CMT CENT DEPOT

$MAIN
ALAG_CENT = ALAG1;
F_CENT = F1;

ALAG_DEPOT = ALAG2;
F_DEPOT = F2;

'

ev1 <- ev(amt=100, cmt=1)
ev2 <- ev(amt=100, cmt=2,time=1)

mod <- 
  mcode("f_alag_model",code,warn=FALSE) %>% 
  carry_out(evid) %>% 
  update(end=2)

test_that("ss=1 and F_CMT =0 issue-497", {
  mod <- param(mod, F1 = 0)
  out <- mrgsim(mod, ev(amt = 100, ii = 24, ss=1), delta=1,end=6)
  expect_is(out, "mrgsims")
  expect_true(all(out$CENT==0))
})


mod1 <- mod %>% ev(ev1)
mod2 <- mod %>% ev(ev2)

out10 <- mod1 %>% mrgsim(recsort=1)
out11 <- mod1 %>% param(F1 = 0.2) %>% mrgsim(recsort=1) 
out12 <- mod1 %>% param(F1 = 0.8) %>% mrgsim(recsort=1)
out13 <- mod1 %>% param(ALAG1 = 1) %>% mrgsim(recsort=1)
out14 <- mod1 %>% param(ALAG1 = 0.5) %>% mrgsim(recsort=1, add=c(0.49999,0.5))

out20 <- mod2 %>% mrgsim(recsort=1) 
out21 <- mod2 %>% param(F2 = 0.3) %>% mrgsim(recsort=1)
out22 <- mod2 %>% param(F2 = 0.1) %>% mrgsim(recsort=1)
out23 <- mod2 %>% param(ALAG2 = 0.3) %>% mrgsim(recsort=1,add=1.3)
out24 <- mod2 %>% param(ALAG2 = 2) %>% mrgsim(end=5,recsort=1)


test_that("F is set for compartment 1 and 2", {
  expect_true(lim(out10,time==2)$CENT==100)
  expect_true(lim(out11,time==2)$CENT==20)
  expect_true(lim(out12,time==2)$CENT==80)
  
  expect_true(lim(out20,time==2)$DEPOT==100)
  expect_true(lim(out21,time==2)$DEPOT==30)
  expect_true(lim(out22,time==2)$DEPOT==10)
})


context("Set ALAG via ALAG_CMT")
test_that("ALAG is set for compartment 1 and 2", {
  
  expect_true(lim(out10, CENT>0)$time[1]==0)
  # 1 and 2 put doses in a data set after padded observations at the same time;
  expect_true(lim(out13, CENT>0)$time[1]==2)
  expect_true(lim(out14, CENT>0)$time[1]==1)
  
  expect_true(lim(out20, DEPOT>0)$time[1]==1)
  expect_true(lim(out23, DEPOT>0)$time[1]==2)
  expect_true(lim(out24, DEPOT>0)$time[1]==4)
})



test_that("F is set for multiple doses", {
  out1 <- 
    mod1 %>% ev(amt=100, cmt=1, addl=3, ii=1) %>% 
    param(F1 = 1) %>% 
    mrgsim(end=3,recsort=2)
  
  out2 <- 
    mod1 %>% ev(amt=100, cmt=1, addl=3, ii=1) %>% 
    param(F1 = 0.2) %>% 
    mrgsim(end=3,recsort=2)
  
  expect_equivalent(lim(out1, time > 0)$CENT, c(100,200,300))
  expect_equivalent(lim(out2, time > 0)$CENT, c(20,40,60))
})



test_that("F and ALAG are set from idata", {
  idata <- mrgsolve:::expand.idata(ID=1:3, F1=c(0.2, 0.5), ALAG1=c(0.2, 0.5,0.7,0.99))
  out1 <- mod1 %>% ev(amt=100, cmt=1, time=1) %>% idata_set(idata) %>% mrgsim()
  # 1 and 2 put doses in a data set after padded observations at the same time;
  out2 <- mod1 %>% ev(amt=100, cmt=1, time=1) %>% idata_set(idata) %>% mrgsim(add=1+idata$ALAG1,recsort=3)
  out2b <- out2 %>% lim(CENT > 0) %>% as_tibble %>% group_by(ID)%>% slice(1)
  
  expect_equivalent(lim(out1, time==2)$CENT, 100*idata$F1)
  expect_equivalent(out2b$time, 1+idata$ALAG1)
})

data(exTheoph)
exTheoph$FORM <- as.integer(exTheoph$ID >5)
exTheoph$F1 <- mrgsolve:::mapvalues(exTheoph$FORM, c(0,1), c(0.8, 0.3))
exTheoph <- exTheoph %>% group_by(ID) %>% mutate(ALAG1 = round(runif(1,1,3),3))
doses <- subset(exTheoph, evid==1)


test_that("F  is set from data", {
  out1 <- mod %>% data_set(exTheoph) %>% mrgsim() 
  expect_equivalent(lim(out1, !duplicated(ID, fromLast=TRUE))$CENT, doses$amt*doses$F1)
})


test_that("ALAG is set from data", {
  # 1 and 2 put doses in a data set after padded observations at the same time;
  out2 <- 
    mod %>% 
    data_set(exTheoph) %>% 
    mrgsim(recsort=3,add=c(doses$ALAG1),obsaug=TRUE) 
  
  out2b <- 
    out2 %>% 
    dplyr::filter(CENT > 0) %>% 
    group_by(ID) %>% slice(1)
  
  expect_equivalent(out2b$time, doses$ALAG1)
  
})

test_that("ALAG does not change records with EVID 3 [SLV-TEST-0007]", {
  data1 <- c(
    ev(amt = 100), 
    ev(amt = 0, evid = 3, time = 8), 
    ev(amt = 100, time = 12)
  )
  data2 <- mutate(data1, ALAG1 = c(0, 5, 0))
  out1 <- mrgsim(mod, data1, end = 24)
  out2 <- mrgsim(mod, data2, end = 24)
  expect_equal(out1@data, out2@data)
})


code <- '
$PARAM CL = 1, V = 10, KA = 1
$MAIN
ALAG_A = 1;
if(self.amt==100) F_A = 0.7; 
if(self.amt==200) F_A = 0.1;
$PKMODEL cmt = "A,B", depot = TRUE
'
mod <- mcode("gh_1129", code)

test_that("Correct bioavability when it changes over a lag time gh-1129", {
  data <- data.frame(amt = c(100,200), evid = 1, time = 0, ID = 1, cmt = 1)
  out <- mrgsim(mod, data, recsort = 3, end = -1, add = 1)
  expect_equal(out$A[3], 90) # 90 = 100*0.7 + 200 * 0.1 
})


# Set of tests around F, D, ALAG, R for infusions

code <- '
$SET Req = "CP", end = 72, delta = 0.1
$PARAM F1 = 1, D1 = 5, ALAG1 = 0, R1 = 0, V = 10
$PKMODEL cmt = "CENT"
$PK
double CL = 1; 
double KA = 1.2;
F_CENT = F1;
D_CENT = D1;
ALAG_CENT = ALAG1;
R_CENT = R1;
$ERROR capture CP = CENT/V;
'

tmax <- function(data) {
  data <- as.data.frame(data)
  data <- dplyr::filter(data, CP==max(CP))
  data$time
}

mod <- mcode("f-dot-r", code)
dose <- ev(amt = 100, tinf = 5)

test_that("Base case - infusion duration == tinf", {
  a <- mrgsim(mod,dose)
  expect_equal(tmax(a), 5)
})

test_that("Reduce F by 1/2 reduces infusion time by half", {
  b <- mrgsim(mod, dose, param = list(F1=0.5))
  expect_equal(tmax(b), 5/2)
})

test_that("Set infusion rate by R1", {
  doser <- ev(amt = 100, rate = -1, R1 = 50)
  c <- mrgsim(mod, doser)
  expect_equal(tmax(c), 100/50)
})

test_that("Set infusion rate by R1 with new F1", {
  dosefr <- ev(amt = 100, rate = -1, R1 = 50, F1 = 3)
  d <- mrgsim(mod, dosefr)
  expect_equal(tmax(d), 3 * 100/50)
})

test_that("Set infusion rate by D1", {
  dosed <- ev(amt = 100, rate = -2, D1 = 7)
  e <- mrgsim(mod, dosed)
  expect_equal(tmax(e), 7)
})

test_that("Set infusion rate by D1 with new F1", {
  dosefd <- ev(amt = 100, rate = -2, D1 = 7, F1 = 0.2)
  f <- mrgsim(mod, dosefd)
  expect_equal(tmax(f), 7)
})

test_that("Set infusion rate by R1 with ALAG", {
  doserl <- ev(amt = 100, rate = -1, R1 = 50, ALAG1 = 1)
  g <- mrgsim(mod, doserl)
  expect_equal(tmax(g), 100/50 + 1)
})

test_that("Set infusion with ALAG", {
  doseal <- ev(amt = 100, tinf = 10, ALAG1 = 5)
  h <- mrgsim(mod, doseal)
  expect_equal(tmax(h), 10 + 5)
})

test_that("Set infusion with ALAG and D1", {
  doseda <- ev(amt = 100, rate = -2, D1 = 3, ALAG1 = 5)
  i <- mrgsim(mod, doseda)
  expect_equal(tmax(i), 5 + 3)
})

test_that("Set infusion with ALAG and R1", {
  dosera <- ev(amt = 100, rate = -1, R1 = 20, ALAG1 = 2.5)
  j <- mrgsim(mod, dosera)
  expect_equal(tmax(j), 2.5 + 100/20)
})

rm(tmax)

test_that("Bioavailability gets propagated into additional doses", {
  mod <- param(mod, V = 2, R1 = 44.4)
  # Infusions
  dose1 <- ev(amt = 100, ii = 24, addl = 3, rate = -1)
  dose2 <- realize_addl(dose1)
  out1 <- mrgsim(mod, dose1, end = 144, obsonly = TRUE)
  out2 <- mrgsim(mod, dose2, end = 144, obsonly = TRUE)
  expect_identical(out1$CP, out2$CP)
  
  # Bolus
  dose3 <- mutate(dose1, rate = 0)
  dose4 <- mutate(dose2, rate = 0)
  
  out3 <- mrgsim(mod, dose3, end = 144, obsonly = TRUE, recsort = 2)
  out4 <- mrgsim(mod, dose4, end = 144, obsonly = TRUE, recsort = 2)
  expect_identical(out3$CP, out4$CP)  
})

test_that("Bioavailability gets propagated into additional doses at steady-state", {
  mod <- param(mod, R1 = 33.33, ALAG1 = 2.5)
  dose1 <- ev(amt = 100, ii = 24, addl = 3, rate = -1, ss = 1)
  out1 <- mrgsim(mod, dose1, end = 98.5, obsonly = TRUE, recsort = 3, digits = 4)
  
  dose2 <- realize_addl(dose1)
  out2 <- mrgsim(mod, dose2, end = 98.5, obsonly = TRUE, recsort = 3, digits = 4)
  
  f1 <- dplyr::filter(out1, time %in% (2.5 + c(0, 24, 48, 72, 96)))
  f2 <- dplyr::filter(out2, time %in% (2.5 + c(0, 24, 48, 72, 96)))
  
  expect_true(all(f1$CP[1]==f1$CP))
  expect_true(all(f2$CP[1]==f2$CP))
  expect_identical(out2, out1)
})

test_that("Bioavailability scales bolus doses", {
  dose <- ev(amt = 100, ii = 24, addl = 4)
  out1 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 1, ALAG1 = 1.23), 
    end = 144
  )
  out2 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 0.73, ALAG1 = 1.23), 
    end = 144
  )
  a <- out1$CP[out1$CP > 0]
  b <- out2$CP[out2$CP > 0]
  r <- unique(signif((b/a),5))
  expect_identical(r, param(out2)$F1)
})

test_that("Bioavailability scales bolus doses at steady state", {
  dose <- ev(amt = 100, ii = 24, addl = 4, ss = 1)
  out1 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 1, ALAG1 = 1.23), 
    end = 144, 
    recsort = 3
  )
  out2 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 2.67, ALAG1 = 1.23), 
    end = 144, 
    recsort = 3
  )
  a <- out1$CP[out1$CP > 0]
  b <- out2$CP[out2$CP > 0]
  r <- unique(signif((b/a),5))
  expect_identical(r, param(out2)$F1)
  
  ## Check that AUC is correct; giving some extra tolerance here for bolus
  out1 <- filter_sims(out1, time <= 24)
  a <- pmxTools::get_auc(as.data.frame(out1), "time", dv = "CP")
  expect_equal(a$AUC, 100, tolerance = 0.5)
  
  out2 <- filter_sims(out2, time <= 24)
  b <- pmxTools::get_auc(as.data.frame(out2), "time", dv = "CP")
  expect_equal(b$AUC, 100 * param(out2)$F1, tolerance = 0.5)
})

test_that("Bioavailability scales infusions", {
  dose <- ev(amt = 100, ii = 24, addl = 4, rate = -2)
  out1 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 1, ALAG1 = 2.34), 
    end = 144
  )
  out2 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 2.67, ALAG1 = 2.34), 
    end = 144
  )
  a <- out1$CP[out1$CP > 0]
  b <- out2$CP[out2$CP > 0]
  r <- unique(signif((b/a),5))
  expect_identical(r, param(out2)$F1)

})

test_that("Bioavailability scales infusions at steady state", {
  dose <- ev(amt = 100, ii = 24, addl = 4, rate = -2, ss = 1)
  out1 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 1, ALAG1 = 0.97), 
    end = 144, 
    recsort = 3
  )
  out2 <- mrgsim(
    mod, 
    dose, 
    param = list(F1 = 2.67, ALAG1 = 0.97), 
    end = 144,
    recsort = 3
  )
  a <- out1$CP[out1$CP > 0]
  b <- out2$CP[out2$CP > 0]
  r <- unique(signif((b/a),5))
  expect_identical(r, param(out2)$F1)
  
  ## Check that AUC is correct; tighten up tolerance ... it's infusion
  out1 <- filter_sims(out1, time <= 24)
  a <- pmxTools::get_auc(as.data.frame(out1), "time", dv = "CP")
  expect_equal(a$AUC, 100, tolerance = 1e-4)

  out2 <- filter_sims(out2, time <= 24)
  b <- pmxTools::get_auc(as.data.frame(out2), "time", dv = "CP")
  expect_equal(b$AUC, 100 * param(out2)$F1, tolerance = 1e-4)
})
