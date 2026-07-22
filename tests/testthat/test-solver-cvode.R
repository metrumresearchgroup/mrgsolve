library(testthat)
library(mrgsolve)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

# Regression coverage for the `solver` slot and the CVODE integration path
# (sundialr C API). Slot / validity / round-trip tests need no solver backend;
# the simulation tests skip when sundialr is not installed.

context("test-solver-cvode")

# ---- slot definition & validity (no backend required) -----------------------

test_that("solver slot defaults to lsoda", {
  mod <- house()
  expect_identical(mod@solver, "lsoda")
  expect_type(mod@solver, "character")
  expect_length(mod@solver, 1L)
})

test_that("update() sets the solver slot both ways", {
  mod <- house()
  expect_identical(update(mod, solver = "cvode")@solver, "cvode")
  expect_identical(update(update(mod, solver = "cvode"), solver = "lsoda")@solver,
                   "lsoda")
})

test_that("$SET solver is honored at compile time", {
  code <- paste(
    "$PARAM CL=1, V=20, KA=1",
    "$CMT GUT CENT",
    "$ODE",
    "dxdt_GUT = -KA*GUT;",
    "dxdt_CENT = KA*GUT - (CL/V)*CENT;",
    "$SET solver = \"cvode\"",
    sep = "\n"
  )
  mod <- mcode("test_set_solver", code, compile = FALSE)
  expect_identical(mod@solver, "cvode")
})

test_that("validObject rejects an invalid solver", {
  mod <- house()
  mod@solver <- "nope"
  expect_error(validObject(mod), "solver must be")
})

# ---- round-trip (no backend required) ---------------------------------------

test_that("solver round-trips through as.list and within", {
  mod <- update(house(), solver = "cvode")
  expect_identical(as.list(mod)$solver, "cvode")
  expect_identical(within(mod, { })@solver, "cvode")
  expect_identical(within(house(), { solver <- "cvode" })@solver, "cvode")
})

test_that("solver round-trips through mwrite_yaml / mread_yaml", {
  skip_if_not_installed("yaml")
  mod <- update(house(), solver = "cvode")
  tf <- tempfile(fileext = ".yaml")
  on.exit(unlink(tf), add = TRUE)
  mwrite_yaml(mod, tf)
  back <- mread_yaml(tf, model = "cvode_roundtrip", compile = FALSE)
  expect_identical(back@solver, "cvode")

  modl <- house() # default lsoda also round-trips
  tf2 <- tempfile(fileext = ".yaml")
  on.exit(unlink(tf2), add = TRUE)
  mwrite_yaml(modl, tf2)
  expect_identical(mread_yaml(tf2, model = "lsoda_roundtrip", compile = FALSE)@solver,
                   "lsoda")
})

# ---- simulation via the CVODE backend (needs sundialr) ----------------------

test_that("lsoda and cvode agree on an advan-13 model", {
  skip_if_not_installed("sundialr")
  mod  <- house()
  dose <- ev(amt = 100)
  out_l <- as.data.frame(mrgsim(mod, events = dose, end = 48, delta = 1))
  out_c <- as.data.frame(mrgsim(update(mod, solver = "cvode"),
                                events = dose, end = 48, delta = 1))
  expect_equal(dim(out_l), dim(out_c))
  num <- vapply(out_l, is.numeric, logical(1))
  expect_equal(as.matrix(out_c[num]), as.matrix(out_l[num]), tolerance = 1e-4)
})

test_that("$SET solver = cvode simulates through the CVODE path", {
  skip_if_not_installed("sundialr")
  code <- paste(
    "$PARAM CL=1, V=20, KA=1",
    "$CMT GUT CENT",
    "$ODE",
    "dxdt_GUT = -KA*GUT;",
    "dxdt_CENT = KA*GUT - (CL/V)*CENT;",
    "$SET solver = \"cvode\"",
    sep = "\n"
  )
  mod <- mcode("test_set_cvode_sim", code)
  expect_identical(mod@solver, "cvode")
  out <- as.data.frame(mrgsim(mod, events = ev(amt = 100, cmt = 1), end = 24))
  expect_true(all(is.finite(out$CENT)))
  expect_gt(max(out$CENT), 0)
})

test_that("an invalid solver errors at simulation time", {
  mod <- house()
  mod@solver <- "bogus"
  expect_error(mrgsim(mod, events = ev(amt = 100), end = 5), "must be")
})

test_that("cvode rejects per-compartment tolerances (itol > 1)", {
  skip_if_not_installed("sundialr")
  mod <- update(use_custom_tol(house()), solver = "cvode")
  expect_equal(mod@itol, 4)
  expect_error(
    mrgsim(mod, events = ev(amt = 100), end = 5),
    "scalar tolerances only"
  )
})
