library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

end <- 60
delta <- 3
n <- length(seq(0,end,delta))
mod <- mrgsolve:::house() %>% update(end=end, delta=delta)
data(extran3)
tb <- tbl_df(extran3)


context("mrgindata tests") 

test_that("mrgindata warns for character columns", {
  dat <- expand.ev(amt=100,ID=1:4,X="A")
  expect_message(mrgindata(dat,mod=mod), regexp="^Dropping")
  
})

test_that("mrgindata subs character cmt", {
  dat <- expand.ev(amt=100,ID=1:4,cmt="RESP")
  x <- mrgindata(dat,mod)
  expect_true(all(x[,"cmt"] ==3))

  dat$cmt <- "GUT"
  x <- mrgindata(dat,mod)
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

test_that("Run with data set - tbl", {
  out <- mod %>% data_set(dplyr::tbl_df(extran3)) %>% mrgsim
  expect_equal(nrow(out),nrow(extran3))
})


data(exidata)

test_that("Run idata set", {
  out <- 
    mod %>%
    idata_set(exidata) %>%
    mrgsim
  expect_equal(nrow(out), length(unique(exidata$ID))*n) 
  expect_equal(length(unique(out$ID)),nrow(exidata))
  
})

test_that("Run idata set with ev", {
  
  e <- ev(amt=100)
  N <- length(unique(exidata$ID))
  N <- N*n + N
  
  out <- 
    mod %>%
    ev(e) %>%
    idata_set(exidata) %>%
    mrgsim
  expect_equal(nrow(out), N) 
  expect_equal(length(unique(out$ID)),nrow(exidata))
})







