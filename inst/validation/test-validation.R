stopifnot(require(testthat))
stopifnot(require(mrgsolve))
stopifnot(require(dplyr))
stopifnot(require(PKPDmisc))
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE, mgsolve.soloc = "build")

test_that("equal sign in annotation issue-576", {
  code <- '
# Compartments
```{cmt}
@annotated
FOO : bar
BAR : baz = zot
```
'
  out <- tempfile(fileext=".Rmd")
  writeLines(code, con = out)
  mod <- mread(out,compile=FALSE)
  expect_is(mod, "mrgmod")
})



code <- '
$PARAM CL = 1, V =20, KA = 1.488,LAG = 1, advance_auc = 1
$CMT DEPOT CENT AUC
$MAIN 
ALAG_DEPOT = LAG;
double SS_FL = 0;

$ODE
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT =   KA*DEPOT - (CL/V)*CENT;
dxdt_AUC = CENT/V;
if(advance_auc==0) dxdt_AUC = 0;
SS_FL = SS_ADVANCE;

$CAPTURE SS_FL
'

test_that("ss bolus with lag and AUC issue-596", {
  first <- ev(amt = 100, ii = 24, ss=1, cmt =1) 
  mod <- mcode_cache("issue_596",code) %>% param(LAG = 13)
  out <- expect_warning(mrgsim_df(mod,first))
  expect_true(all(out[["CENT"]] >=0))
})

test_that("control ss advance issue-598", {
  first <- ev(amt = 100, ii = 24, ss=1, cmt =1) 
  mod <- mcode_cache("issue_596",code) 
  out <- expect_warning(mrgsim(mod,first))
  auci <- out$AUC[2]
  out <- expect_silent(mrgsim(mod,first, param = list(advance_auc = 0)))
  expect_true(all(out$AUC==0))
  mod <- mrgsolve:::set_ss_cmt(mod, "CENT")
  expect_identical(mod@ss_cmt, 1L)
  out <- expect_silent(mrgsim(mod,first))
  expect_true(out$AUC[2] < auci/10)
  expect_equal(out$SS_FL[2],1)
})


