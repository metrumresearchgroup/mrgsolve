
library(testthat)
library(mrgsolve)
library(dplyr)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

context("test-cpp")

code <- '
[ plugin ] nm-vars, mrgx, Rcpp

[ cmt ] @number 1

[ des ]  
DADT(1) = -0.1 * A(1);
'

test_that("build a model with mrgx and nm-vars", {
  expect_is(
    mcode("test-cpp-mrgx-nm-vars", code, quiet = TRUE), 
    "mrgmod"
  )
})

code <- '
$PK
if(NEWIND <= 1) capture FLAG = 0;
$CMT A
$TABLE 
if(TIME==2) {
  self.mevent(2.001,33);
  self.mevent(2.001,33);
}
if(EVID==33) ++FLAG;
'

mod <- mcode("test-cpp-re-event", code, end = -1, add = c(1,2,3))

mrgsim(mod, ev(amt = 0, ID = 1:2, evid = 2)) %>% as.data.frame()



code <- '

$MAIN 
if(NEWIND == 0) { 
  int count = 0;
}
if(NEWIND <= 1) {
  int dosen = 0;
  double dose = 0;
}

$TABLE
if(EVID> 30 && dosen < 11) {
    if(dosen <= 1) dose = 100;
    if(dosen==2)   dose = 200;  
    if(dosen ==3)  dose = 300;
    if(dosen > 3)  dose = 400;
    if(dosen > 5)  dose =   0;
    mrg::evdata ev(TIME,1);
    ev.cmt = 1;
    ev.amt = dose;
    ev.now = true;
    self.mevector.push_back(ev);
    ++dosen;
    self.mevent(TIME + 3, 33);
}

$CMT A
$CAPTURE dosen dose
'

mod <- mcode("foo", code)

data <- expand.ev(amt = 0, evid = 33, time = 0, ID = 1:2)

mrgsim(mod, data = data, end = 33, delta = 0.1)  %>% plot()
