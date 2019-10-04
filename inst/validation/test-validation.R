stopifnot(require(testthat))
stopifnot(require(mrgsolve))
stopifnot(require(dplyr))
stopifnot(require(PKPDmisc))
Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE, mgsolve.soloc = "build")

context("new ODE solver")

test_that("non-stiff problem", {
  mod <- modlib("pk2cmt", end = 240, delta=0.001)
  d <- 331.234
  dose <- ev(amt = d)
  expect_silent(out <- mrgsim_e(mod,dose,ixpr=1))
  auc <- summarise(out, auc= auc_partial(time,CP)) %>% pull(auc) %>% unname
  expect_equal(round(auc),round(d/mod$CL))
})

test_that("stiff problem", {
  mod <- modlib("pbpk", end = 48, delta = 0.001, rtol = 1e-12)
  p <- list(Kpli = 0.4573, fup = 0.6453)
  dose <- 89.123
  e <- ev(amt = dose) 
  cl <- mod$HLM_CLint
  MPPGL <- 45.0
  Vli <- mod$BW * mod$FVli
  fumic <- 1
  clmet <- (cl/fumic)*MPPGL*Vli*60/1000
  mod <- param(mod,p)
  msg <- capture.output(
    out <- mrgsim_e(mod,e,ixpr=1), 
    type = "message"
    )
  expect_true(grepl("a switch to the stiff method has occurred", msg))
  auc <- summarise(out, auc = auc_partial(time,Cp)) %>% pull(auc) %>% unname
  expect_equal(round(auc), round(dose/(clmet*p$Kpli*p$fup)))
})
