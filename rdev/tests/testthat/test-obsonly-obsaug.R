library(testthat)
library(mrgsolve)
library(dplyr)
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("Testing obsonly and obsaug arguments")

mod <- mrgsolve:::house(add=c(8.123, 33.3333))

data(exTheoph)

out1 <- mrgsim(mod, obsonly=TRUE, data=exTheoph, carry.out="evid")
out2 <- mrgsim(mod, obsonly=TRUE, data=exTheoph)
out3 <- mrgsim(mod, obsaug=TRUE,  data=exTheoph, carry.out="a.u.g")

test_that("Using of obsonly with data set", {
  expect_true(all(out1$evid==0))
  expect_true(all(sort(unique(exTheoph$evid)) == c(0,1)))
  expect_true(all(out2$time==exTheoph$time[exTheoph$evid==0]))
})

test_that("Using obsonly with events object", {
  expect_identical(mrgsim(mod %>% ev(amt=100, cmt=1), obsonly=TRUE)$time,stime(mod))
})

augtimes <- subset(out3, a.u.g==1)$time
datatimes <- subset(out3, a.u.g==0)$time
alltimes <- lapply(split(exTheoph, exTheoph$ID), function(x){
  return(sort(c(x$time, stime(mod))))
})


test_that("Use of obsaug returns augmented set of observations", {
  alltimes <- unlist(alltimes)
  expect_true(sum(names(out3) %in% "a.u.g")==1)
  expect_identical(augtimes, rep(stime(mod), length(unique(exTheoph$ID))))    
  expect_identical(datatimes, exTheoph$time)
  expect_equivalent(alltimes, out3$time)
})



test_that("Use of obsaug doesn't affect simulation without data", {
  out4 <- mrgsim(mod, obsaug=TRUE)
  expect_identical(out4$time, stime(mod))
})








