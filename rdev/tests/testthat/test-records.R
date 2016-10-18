
#library(metrumrg)
library(testthat)
#library(MASS)
library(mrgsolve)
#library(magrittr)
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



