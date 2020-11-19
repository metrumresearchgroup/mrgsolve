library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-tad-cmt")

code <- '
$cmt foo bar

$param CL = 10, V = 20, KA = 1

$plugin tad

$pkmodel depot = TRUE

$global
mrg::tadose z;

$preamble
z.cmt = 2;

$main
static mrg::tadose x(2);
static mrg::tadose y(1);
capture tadx = x.tad(self);
capture tady = y.tad(self);
capture tadose1 = self.tad();

$table
capture tadz = z.tad(self);
capture tados = self.tad();
'

mod <- mcode("adfa", code, end = 48)

ev1 <- ev(amt = 100, time = 1.01, cmt = 2, ii = 4, addl = 9)
ev2 <- ev(amt = 300, time = 4.01, cmt = 1, ii = 8, addl = 4)
dose <- c(ev1, ev2)

a <- mrgsim(mod, ev1,  carry_out = "evid", tad = TRUE)
b <- mrgsim(mod, ev2,  carry_out = "evid", tad = TRUE)
c <- mrgsim(mod, dose, carry_out = "evid", tad = TRUE)

aa <- dplyr::filter(a, evid == 0) %>% dplyr::slice(-2)
bb <- dplyr::filter(b, evid == 0) %>% dplyr::slice(-2)
cc <- dplyr::filter(c, evid == 0) %>% dplyr::slice(-2)

test_that("tad-cmt with default initialization", {
  expect_equal(c$tadz, c$tadx)  
})

test_that("tad-cmt matches tad-no-cmt", {
  # check tad x and y
  check_x <- aa$tadx - cc$tadx
  check_y <- bb$tady - cc$tady
  expect_true(all(check_x==0))
  expect_true(all(check_y==0))
})

test_that("check against tad", {
  # check against tad
  aaa <- filter(aa, tad >= 0)
  bbb <- filter(bb, tad >= 0)
  check_x <- aaa$tad - aaa$tadx
  check_y <- bbb$tad - bbb$tady
  expect_true(all(check_x==0))
  expect_true(all(check_y==0))
})
