library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

test_that("deslist", {
  idata <- data_frame(ID=1:3, start=0, end=c(1,2,3))
  des <- as_deslist(idata,descol="ID")
  expect_identical("ID", attr(des, "descol"))

  expect_equal(max(stime(des[[3]])),3)
  expect_equal(max(stime(des[[1]])),1)
  
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


