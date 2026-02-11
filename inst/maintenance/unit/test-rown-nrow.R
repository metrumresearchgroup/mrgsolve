# Copyright (C) 2013 - 2026  Metrum Research Group
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

code_test_rown_nrow <- '
$CMT A
$ERROR 
capture IROWN = self.irown;
capture INROW = self.inrow;
capture ROW = self.rown;
capture NROW = self.nrow;
'

code_test_rown_nrow_pred <- '
$PRED 
capture IROWN = self.irown;
capture INROW = self.inrow;
capture ROW = self.rown;
capture NROW = self.nrow;
'

test_that("individual row counters work", {
  mod <- mcode("test-rown-nrow-1", code_test_rown_nrow)
  
  # 11/14/10 records
  d1 <- ev(amt = 100, ii = 2, addl = 3, ID = 1)
  d1 <- expand_observations(d1, 1:10)
  # 4/4/3 records
  d2 <- ev(amt = 200, ID = 2, ii = 0, addl = 0)
  d2 <- expand_observations(d2, 1:3)
  # 7/9/6 records
  d3 <- ev(amt = 300, ii = 2, addl = 2, ID = 3)
  d3 <- expand_observations(d3, 1:6)
  
  data0 <- rbind(d1, d2)
  data1 <- rbind(data0, d3)
  data2 <- realize_addl(data1)
  
  # No realize_addl, obsonly = FALSE
  out1 <- mrgsim(mod, data1, output = "df") 
  expect_true(all(out1$NROW==22))
  expect_identical(out1$ROW, seq(22)-1)
  
  out11 <- subset(out1, ID==1)
  expect_true(all(out11$INROW==11))
  expect_identical(out11$IROWN, seq(11)-1)
  
  out12 <- subset(out1, ID==2)
  expect_true(all(out12$INROW==4))
  expect_identical(out12$IROWN, seq(4)-1)
  
  out13 <- subset(out1, ID==3)
  expect_true(all(out13$INROW==7))
  expect_identical(out13$IROWN, seq(7)-1)
  
  # No realize_addl, obsonly = TRUE
  out2 <- mrgsim(mod, data1, output = "df", obsonly = TRUE) 
  expect_true(all(out2$NROW==19))
  expect_identical(out2$ROW, seq(19)-1)
    
  out21 <- subset(out2, ID==1)
  expect_true(all(out21$INROW==10))
  expect_identical(out21$IROWN, seq(10)-1)
  
  out22 <- subset(out2, ID==2)
  expect_true(all(out22$INROW==3))
  expect_identical(out22$IROWN, seq(3)-1)
  
  out23 <- subset(out2, ID==3)
  expect_true(all(out23$INROW==6))
  expect_identical(out23$IROWN, seq(6)-1)
  
  # Yes realize_addl, obsonly = FALSE
  out3 <- mrgsim(mod, data2, output = "df") 
  expect_true(all(out3$NROW==27))
  expect_identical(out3$ROW, seq(27)-1)
  
  out31 <- subset(out3, ID==1)
  expect_true(all(out31$INROW==14))
  expect_identical(out31$IROWN, seq(14)-1)
  
  out32 <- subset(out3, ID==2)
  expect_true(all(out32$INROW==4))
  expect_identical(out32$IROWN, seq(4)-1)
  
  out33 <- subset(out3, ID==3)
  expect_true(all(out33$INROW==9))
  expect_identical(out33$IROWN, seq(9)-1)
  
  # Yes realize_addl, obsonly = TRUE
  out4 <- mrgsim(mod, data2, output = "df", obsonly = TRUE) 
  expect_true(all(out4$NROW==19))
  expect_identical(out4$ROW, seq(19)-1)
  
  out41 <- subset(out4, ID==1)
  expect_true(all(out41$INROW==10))
  expect_identical(out41$IROWN, seq(10)-1)
  
  out42 <- subset(out4, ID==2)
  expect_true(all(out42$INROW==3))
  expect_identical(out42$IROWN, seq(3)-1)
  
  out43 <- subset(out4, ID==3)
  expect_true(all(out43$INROW==6))
  expect_identical(out43$IROWN, seq(6)-1)
})

test_that("individual row counters work with PRED model", {
  mod <- mcode("test-rown-nrow-2", code_test_rown_nrow_pred)
  
  # 11/14/10 records
  d1 <- ev(amt = 100, ii = 2, addl = 3, ID = 1)
  d1 <- expand_observations(d1, 1:10)
  # 4/4/3 records
  d2 <- ev(amt = 200, ID = 2, ii = 0, addl = 0)
  d2 <- expand_observations(d2, 1:3)
  # 7/9/6 records
  d3 <- ev(amt = 300, ii = 2, addl = 2, ID = 3)
  d3 <- expand_observations(d3, 1:6)
  
  data0 <- rbind(d1, d2)
  data1 <- rbind(data0, d3)
  data2 <- realize_addl(data1)
  
  data1$cmt <- 0
  data2$cmt <- 0
  
  data3 <- subset(data2, evid==0)
  
  # No realize_addl, obsonly = FALSE
  out1 <- mrgsim(mod, data1, output = "df") 
  expect_true(all(out1$NROW==22))
  expect_identical(out1$ROW, seq(22)-1)
  
  out11 <- subset(out1, ID==1)
  expect_true(all(out11$INROW==11))
  expect_identical(out11$IROWN, seq(11)-1)
  
  out12 <- subset(out1, ID==2)
  expect_true(all(out12$INROW==4))
  expect_identical(out12$IROWN, seq(4)-1)
  
  out13 <- subset(out1, ID==3)
  expect_true(all(out13$INROW==7))
  expect_identical(out13$IROWN, seq(7)-1)
  
  # No realize_addl, obsonly = TRUE
  out2 <- mrgsim(mod, data1, output = "df", obsonly = TRUE) 
  expect_true(all(out2$NROW==19))
  expect_identical(out2$ROW, seq(19)-1)
  
  out21 <- subset(out2, ID==1)
  expect_true(all(out21$INROW==10))
  expect_identical(out21$IROWN, seq(10)-1)
  
  out22 <- subset(out2, ID==2)
  expect_true(all(out22$INROW==3))
  expect_identical(out22$IROWN, seq(3)-1)
  
  out23 <- subset(out2, ID==3)
  expect_true(all(out23$INROW==6))
  expect_identical(out23$IROWN, seq(6)-1)
  
  # Yes realize_addl, obsonly = FALSE
  out3 <- mrgsim(mod, data2, output = "df") 
  expect_true(all(out3$NROW==27))
  expect_identical(out3$ROW, seq(27)-1)
  
  out31 <- subset(out3, ID==1)
  expect_true(all(out31$INROW==14))
  expect_identical(out31$IROWN, seq(14)-1)
  
  out32 <- subset(out3, ID==2)
  expect_true(all(out32$INROW==4))
  expect_identical(out32$IROWN, seq(4)-1)
  
  out33 <- subset(out3, ID==3)
  expect_true(all(out33$INROW==9))
  expect_identical(out33$IROWN, seq(9)-1)
  
  # Yes realize_addl, obsonly = TRUE
  out4 <- mrgsim(mod, data2, output = "df", obsonly = TRUE) 
  expect_true(all(out4$NROW==19))
  expect_identical(out4$ROW, seq(19)-1)
  
  out41 <- subset(out4, ID==1)
  expect_true(all(out41$INROW==10))
  expect_identical(out41$IROWN, seq(10)-1)
  
  out42 <- subset(out4, ID==2)
  expect_true(all(out42$INROW==3))
  expect_identical(out42$IROWN, seq(3)-1)
  
  out43 <- subset(out4, ID==3)
  expect_true(all(out43$INROW==6))
  expect_identical(out43$IROWN, seq(6)-1)

  out5 <- mrgsim(mod, data3, output = "df")
  expect_true(all(out5$NROW==19))
  expect_identical(out5$ROW, seq(19)-1)
  
  out51 <- subset(out5, ID==1)
  expect_true(all(out51$INROW==10))
  expect_identical(out51$IROWN, seq(10)-1)
  
  out52 <- subset(out5, ID==2)
  expect_true(all(out52$INROW==3))
  expect_identical(out52$IROWN, seq(3)-1)
  
  out53 <- subset(out5, ID==3)
  expect_true(all(out53$INROW==6))
  expect_identical(out53$IROWN, seq(6)-1)
})


code_update_on_output <- '
$preamble capture total = 0;
$main self.mtime(12);
$table if(self.rown+1 == self.nrow) ++total;
'

test_that("row counters are only updated on output records", {
  mod <- mcode("row-count-update-only", code, end = 24, delta = 24)
  out <- mrgsim(mod)
  expect_equal(nrow(out), 2)
  # This will be 2 prior to fix
  expect_equal(out$total[2], 1)
})

rm(code_test_rown_nrow, code_test_rown_nrow_pred)
rm(code_update_on_output)
