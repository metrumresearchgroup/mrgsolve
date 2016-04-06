library(mrgsolve)
library(testthat)
#library(MASS)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

project <- file.path(system.file(package="mrgsolve"), "models")

context("Testing various request settings")

code <- '
$PARAM CL=1
$INIT GUT=100, CENT=5, PERIPH=3


$ODE
dxdt_GUT = 0;
dxdt_CENT = 0;
dxdt_PERIPH = 0;

$TABLE
table(CP) = 1;
table(FLAG) = 2;
table(ETA1) = ETA(1);
table(EPS1) = EPS(1);
'
mrgsolve:::comp_forget()
tmp <- tempdir()
mod <- mread(code=code, project=tmp, model="test3")

out <-mrgsim(mod, request="PERIPH,CENT")

out2 <- mrgsim(update(mod, request="CENT,PERIPH,GUT"))


test_that("Testing request setting", {
  expect_equal(names(out),c("ID", "time","PERIPH","CENT","CP","EPS1", "ETA1", "FLAG"))
  expect_equal(names(out2),c("ID", "time","CENT","PERIPH","GUT","CP","EPS1", "ETA1", "FLAG"))
})

out3 <- mrgsim(mod, Trequest="EPS1,FLAG,ETA1", request="PERIPH,CENT")
out4 <- mrgsim(mod, Treq="FLAG")
test_that("Testing Treq setting", {
  expect_equal(names(out3),c("ID", "time","EPS1", "FLAG", "ETA1"))
  expect_equal(names(out4),c("ID", "time","FLAG"))
})

out5 <- mrgsim(mod, Req="EPS1,CENT,GUT,FLAG", request="PERIPH", Treq="CP")

test_that("Testing Req setting", {
  expect_equal(names(out5),c("ID", "time","CENT","GUT","EPS1", "FLAG"))
})

code <- '
$PARAM CL=1
$INIT GUT=100, CENT=5, PERIPH=3
$SET
req="CENT"

$ODE
dxdt_GUT = 0;
dxdt_CENT = 0;
dxdt_PERIPH = 0;

$TABLE
table(CP) = 1;
table(FLAG) = 2;
table(ETA1) = ETA(1);
table(EPS1) = EPS(1);
'

mrgsolve:::comp_forget()
mod <- mread(code='$INIT CENT=0\n$PARAM CL=1', project=tmp, model="test3c")
test_that("Testing that request is (all) by default", {
  expect_identical(mod@request, "(all)")
})

mrgsolve:::comp_forget()
mod <- mread(code=code, project=tmp, model="test3b")
cols <- names(mrgsim(mod))
test_that("Testing that request is properly set in $SET", {
  expect_identical(mod@request, "CENT")
  expect_identical(update(mod, req=c("PERIPH", "GUT"))@request, c("PERIPH", "GUT"))
  expect_identical(update(mod, req="PERIPH,GUT")@request, "PERIPH,GUT")
  expect_identical(intersect(cols,cmt(mod)), "CENT")
})




