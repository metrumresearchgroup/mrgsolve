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


