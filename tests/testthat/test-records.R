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
data(exidata) 


test_that("Run via idata and separate", {
  out1 <- mod %>% 
    mrgsim(idata=exidata,events=ev(amt=100,ii=8,addl=3)) %>%
    as.data.frame
  
  
  out2 <- lapply(seq_along(exidata$ID), function(i) {
    mod %>% 
      mrgsim(idata=exidata[i,],events=ev(amt=100,ii=8,addl=3)) %>%
      as.data.frame
    
  }) %>% bind_rows %>% as.data.frame
  
  expect_identical(out1,out2)
})



