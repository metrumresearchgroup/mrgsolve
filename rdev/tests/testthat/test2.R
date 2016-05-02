library(mrgsolve)
library(testthat)
#library(MASS)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

project <- file.path(system.file(package="mrgsolve"), "models")

context("Testing new model specification file")

code <- '
$PARAM CL=1, VC=20, KA=1.2
VMAX=2, KM=3

$INIT GUT=100, CENT=5

$SET
end=111, delta=1.1, hmax=10, maxsteps=22334

$GLOBAL
#define CP (CENT/VC)
double KE = 0;

$MAIN
KE = CL/VC;

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - KE*CENT;

$TABLE
table(CP) = CP;
table(FLAG) = 2;
table(ETA1) = ETA(1);
table(EPS1) = EPS(1);

$OMEGA name="A"
1
2
3

$OMEGA name="B", corr=TRUE
0.1
0.5 0.2

$SIGMA
0.55

$SIGMA block=TRUE
0.1 0.002 0.3

'
mrgsolve:::comp_forget()
tmp <- tempdir()
mod <- mread(code=code, project=tmp, model="test2", warn=FALSE)

test_that("Parameters are parsed properly with mread", {
  expect_equal(param(mod)$CL,1)
  expect_equal(param(mod)$VC,20)
  expect_equal(param(mod)$KA,1.2)
  expect_equal(pars(mod), c("CL", "VC", "KA", "VMAX", "KM"))
})

test_that("Compartments are parsed properly with mread", {
  expect_equal(init(mod)$GUT,100)
  expect_equal(init(mod)$CENT,5)
  expect_equal(cmt(mod), c("GUT", "CENT"))
})

test_that("Settings are parsed properly with mread", {
    expect_equal(mod@end,111)
    expect_equal(mod@delta,1.1)
    expect_equal(mod@hmax,10)
    expect_equal(mod@maxsteps,22334)  
})

test_that("mread output had class mrgmod", {
    expect_is(mod, "mrgmod")
})

out <- mrgsim(mod)
test_that("Tabled items are in simulated data", {
    expect_true("CP" %in% names(out))
    expect_true(all(out$FLAG==2))
})

mat <- as.matrix(omat(mod))
test_that("Omega matrices are properly parsed", {
  expect_equivalent(mat[2,2],2)
  expect_equivalent(mat[4,4],0.1)
  expect_equivalent(mat[5,5],0.2)
  expect_equivalent(signif(mat[5,4],4),signif(0.07071068,4))
})

mat <- as.matrix(smat(mod))
test_that("Sigma matrices are properly parsed", {
  expect_equivalent(mat[1,1],0.55)
  expect_equivalent(mat[3,3],0.3)
  expect_equivalent(mat[3,2],0.002)
})


set.seed(8282)
out <- mrgsim(mod,end=100000, delta=1)
test_that("EPS values have proper variance", {
  expect_equal(round(var(out$EPS1),2),0.55)
})


mrgsolve:::comp_forget()

code <- '
$PARAM CL=1, VC=20
$INIT  CENT=0

$GLOBAL 
double kyle=5;


$MAIN
double ke=CL/20;
double ke2 = CL/VC;

double a=5;
a = a/1;

bool TRUTH = true;

bool LIE = false;
LIE = false;

$ODE
double set_in_ode =1;
ke2=556.2;

$TABLE
table(ke) = ke;
table(ke2) = ke2;
table(TRUTH) = TRUTH;
table(set_in_ode) = set_in_ode;
'

obj <- try(mread(code=code,basename(tempfile()), project=tempdir(), warn=FALSE))
test_that("User-declared C++ variables are available globally", {
    expect_is(obj, "mrgmod")
})





