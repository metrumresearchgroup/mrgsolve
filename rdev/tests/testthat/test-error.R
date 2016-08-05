library(mrgsolve)
library(testthat)


Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

project <- file.path(system.file(package="mrgsolve"), "models")

mod <- mread("firstmodel", project,atol=1E-20, rtol=1E-12, digits=8)

context("Testing error generation ")
test_that("Error when parameters and compartments have same name", {
  expect_error(mrgmod("firstmodel", project, param=list(CENT=2)))
  expect_error(mrgmod("firstmodel", project, init=list(CL=2)))
})

test_that("Error when parameters and compartments have same name introduced via update", {
  expect_error(update(mod, param=list(CENT=2), strict=FALSE))
  expect_error(update(mod, init=list(CL=2), strict=FALSE))
})

test_that("Error when a parameter name is listed in compartments", {
  expect_error(update(mod, init=list(CL=2), strict=FALSE))
  expect_error(mrgmod("firstmodel", project, init=list(CL=2)))
})
test_that("Error  when a compartment names is listed in parameters", {
  expect_error(update(mod, param=list(CENT=2), strict=FALSE))
  expect_error(mrgmod("firstmodel", project, param=list(CENT=2)))
})

bad <- update(mod, param=list(FAKEPARAMETER=22), strict=FALSE)
test_that("Error when the number of parameters changes without recompile",{
    expect_error(mrgsim(bad))
})
