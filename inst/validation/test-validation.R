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

$PREAMBLE
double SS_FL  = 0;

$MAIN 
ALAG_DEPOT = LAG;

$ODE
dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT =   KA*DEPOT - (CL/V)*CENT;
dxdt_AUC = CENT/V;
if(advance_auc==0) dxdt_AUC = 0;
if(SS_ADVANCE) ++SS_FL;

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
  expect_true(out$SS_FL[2] > 0)
})

test_that("PKG_CXXFLAGS is set issue-603", {
  code <- '$ENV PKG_CXXFLAGS = "-Wdiv-by-zero"'
  expect_output(
    mcode("cxxflags", code, ignore.stdout = FALSE,preclean=TRUE), 
    regexp = "Wdiv-by-zero"
  )
})

test_that("Add N_CMT as plugin issue-606", {
  code <- '$CMT A B\n$PLUGIN N_CMT\n$CAPTURE N_A'
  mod <- mcode("N_CMT_TEST", code)
  expect_is(mod, "mrgmod")
  out <- mrgsim(mod, end = -1)
  expect_true(all(out$N_A==1))
})

test_that("call blocks on model from Rmd issue-608", {
  mod <- modlib("popex.Rmd",compile=FALSE)
  expect_output(
    blocks(mod), 
    regexp = "popex\\.Rmd"
  )
})


test_that("update model object with within issue-616", {
  mod <- mrgsolve:::house()
  mod2 <- within(mod, {
    CL = CL*1.5
    CENT = 101
    end  = 72
  })
  expect_identical(mod2$CL,1.5*mod$CL)
  expect_identical(init(mod2)[["CENT"]],101)
  expect_identical(mod2@end,72)
})

  
