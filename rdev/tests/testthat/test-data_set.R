
library(mrgsolve)
library(testthat)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)


mod <- mrgsolve:::house() %>% update(end=240)


context("Testing data_set")

data(extran1)
lo <- extran1
up <- extran1
names(up) <- toupper(names(up))

test_that("Same result from upper and lower case names", {
  what <- c(1,5:7)
  a <- mod %>% data_set(up) %>% mrgsim
  b <- mod %>% data_set(lo) %>% mrgsim
  expect_identical(as.matrix(a)[,what],as.matrix(b)[,what])
  expect_identical(a$TIME,b$time)
  expect_is(plot(a),"trellis")
  expect_is(plot(b),"trellis")
  
  expect_is(plot(a, CP~.),"trellis")
  expect_is(plot(a, CP~TIME),"trellis")
  expect_is(plot(b, CP~.),"trellis")
  expect_is(plot(b, CP~time),"trellis")
  expect_error(plot(b, CP~TIME))
  
  ldd <- expand.ev(amt=100, ii=24, addl=2, ss=1)
  udd <- ldd
  names(udd) <- toupper(names(udd))
  a <- mod %>% data_set(ldd) %>% mrgsim
  b <- mod %>% data_set(udd) %>% mrgsim
  expect_identical(as.matrix(a)[,what],as.matrix(b)[,what])
})

test_that("Warning is generated when mixed upper/lower names", {
    mix <- lo %>% dplyr::rename(EVID = evid) 
    expect_warning(mod %>% data_set(mix))
})

test_that("Filter out ID", {
  out <- mod %>% data_set(up, ID > 4) %>% mrgsim
  expect_true(all(out$ID > 4))
})

test_that("ID is required", {
    df <- expand.ev(amt=100,ii=12,addl=2) %>% dplyr::select(-ID)
    expect_error(mod %>% data_set(df) %>% mrgsim)
})

test_that("cmt is required", {
  df <- expand.ev(amt=100,ii=12,addl=2) %>% dplyr::select(-cmt)
  expect_error(mod %>% data_set(df) %>% mrgsim)
})

test_that("time is required", {
  df <- expand.ev(amt=100,ii=12,addl=2) %>% dplyr::select(-time)
  expect_error(mod %>% data_set(df) %>% mrgsim)
})


