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
$PARAM CL = 1, V =20, KA = 1.488,LAG = 1
$CMT DEPOT CENT AUC
$MAIN ALAG_DEPOT = LAG;
$ODE
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT =   KA*DEPOT - (CL/V)*CENT;
dxdt_AUC = CENT/V;
'

test_that("ss bolus with lag and AUC issue-596", {
  first <- ev(amt = 100, ii = 24, ss=1, cmt =1) 
  mod <- mcode_cache("issue_596",code) %>% param(LAG = 13)
  out <- expect_warning(mrgsim_df(mod,first))
  expect_true(all(out[["CENT"]] >=0))
})


