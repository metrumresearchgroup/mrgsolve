library(mrgsolve)
library(testthat)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)


context("R RNG respected via set.seed()")
mod <- mrgsolve:::house(omega=diag(c(1,1,1,1)))

out1 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
out2 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
set.seed(333)
out3 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
set.seed(333)
out4 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))

out5 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20),seed=555)
out6 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20),seed=555)


out7 <- simre(out5)
out8 <- simre(out6)
out9 <- simre(out6)


ident <- function(x,y,...) {
  x@date <- y@date <- date()
  identical(x,y) 
}

test_that("Runs with different seeds give different results without call to set.seed()", {
  expect_false(ident(out1,out2))
})

test_that("Runs with different seeds give different results with different calls to set.seed()", {
  expect_false(ident(out3,out5))
})
test_that("Runs with same seeds give same results with call to set.seed()", {
  expect_true(ident(out3,out4))
})
test_that("Runs with same seeds give same results when seed passed to mrgsim()", {
  expect_true(ident(out5,out6))
})
test_that("Random effects are properly recovered from runs with same seed passed", {
  expect_true(identical(out7,out8))
  expect_true(identical(out8,out9))
})
test_that("Random effects are properly recovered by repeated calls to simre", {
  expect_true(identical(out8,out9))
})


