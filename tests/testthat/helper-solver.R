# Suite-wide solver selection, so the test suite can be run under either ODE
# backend. Set the environment variable MRGSOLVE_TEST_SOLVER=cvode to route every
# model through CVODE: mread() -- which mcode(), house() and modlib() all reach --
# is wrapped to stamp solver="cvode" on each model it returns. The default,
# "lsoda", changes nothing (the wrapper is not installed), so ordinary test runs
# and CI behave exactly as before.
#
# Tests that assert behavior specific to one solver (e.g. the exact LSODA
# "negative istate" diagnostic, or LSODA's loose-tolerance numerics) branch on
# test_solver() or skip_if(test_solver() == "cvode", ...).
test_solver <- function() {
  s <- Sys.getenv("MRGSOLVE_TEST_SOLVER", unset = "lsoda")
  if(!nzchar(s) || !s %in% c("lsoda", "cvode")) "lsoda" else s
}

if(identical(test_solver(), "cvode")) {
  local({
    ns <- asNamespace("mrgsolve")
    .orig_mread <- get("mread", ns)
    assignInNamespace("mread", function(...) {
      x <- .orig_mread(...)
      if(isS4(x) && methods::.hasSlot(x, "solver")) {
        x@solver <- "cvode"
      }
      x
    }, ns = "mrgsolve")
  })
}
