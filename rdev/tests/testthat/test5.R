library(mrgsolve)
library(testthat)
#library(MASS)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)
project <- file.path(system.file(package="mrgsolve"), "models")

context("Testing initial conditions")

code <- '
$SET end=1, delta=1
$PARAM CL=1, VC=20, KA=1, ETA1 = 0, ETA2 =0
$INIT GUT=0,CENT=20, RESP = 0, DUM=0

$MAIN
RESP_0 = KIN+ETA1;
DUM_0 = exp(ETA(1));


$ODE 
dxdt_CENT = 0; dxdt_GUT=0;
dxdt_RESP = 0;
dxdt_DUM = 0;

$OMEGA
0.4

$GLOBAL
#define CP (CENT/VC)
#define INH (CP/(IC50+CP))
double KIN=100, KOUT=2, IC50=8;

$TABLE
table(CP) = CP;
table(ETA1) = ETA(1);
'

mrgsolve:::comp_forget()
tmp <- tempdir()

mod <- mread(code=code, project=tmp, model="test5")

set.seed(668855)
base1 <- as.list(mrgsim(mod) %>% as.tbl %>% distinct(ID))
set.seed(23456)
base2 <- as.list(mrgsim(mod %>% param(ETA1 = 2, ETA2=3)) %>% as.tbl %>% distinct(ID))




test_that("Testing initial conditions from base parameters", {
    expect_equivalent(base1$RESP, 100)
    expect_equivalent(base2$RESP, 102)
    expect_equivalent(base2$CENT,20)
})

test_that("Testing initial conditions from random effects", {
    expect_equivalent(round(base1$DUM,4), 0.9325)
    expect_equivalent(round(base2$DUM,4), 0.8847)
    
})

data <- data.frame(ID=1, ETA1=3, ETA2=2, time=0, cmt=1)
base3 <- as.list(mrgsim(mod ,data=data) %>% as.tbl %>% distinct(ID))
test_that("Testing initial conditions from data set", {
    expect_equivalent(base3$RESP, 103)
    expect_equivalent(base3$CENT,20)
})

data <- data.frame(ID=1, ETA1=10, ETA2=2, time=0, cmt=1)
base4 <- as.list(mrgsim(mod ,idata=data) %>% as.tbl %>% distinct(ID))
test_that("Testing initial conditions from idata set", {
    expect_equivalent(base4$RESP, 110)
    expect_equivalent(base4$CENT,20)
})





