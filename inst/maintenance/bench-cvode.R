# Copyright (C) 2013 - 2026  Metrum Research Group
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.

# Benchmark: the default LSODA solver vs CVODE (SUNDIALS BDF, solver = "cvode")
# on a stiff ODE system. CVODE is an implicit BDF integrator that scales well on
# large, stiff systems, whereas the LSODA implementation slows sharply as the
# stiff system grows. The benchmark therefore uses a scalable stiff problem so
# the crossover is visible; on small or non-stiff models LSODA is usually as fast
# or faster, and CVODE is opt-in.
#
# The stiff test problem is M independent copies of the Robertson chemical
# kinetics system -- the classic stiff ODE benchmark -- giving neq = 3 * M.
#
# Requires the sundialr package (>= 0.2.0), which provides the CVODE backend.

library(mrgsolve)
stopifnot(require("bench"))
stopifnot(requireNamespace("sundialr", quietly = TRUE))

# Build M stacked Robertson systems as an Advan-13 ($ODE) model.
robertson_code <- function(M) {
  ode <- unlist(lapply(seq_len(M), function(k) c(
    sprintf("dxdt_y1_%d = -0.04*y1_%d + 1e4*y2_%d*y3_%d;",                 k, k, k, k),
    sprintf("dxdt_y2_%d =  0.04*y1_%d - 1e4*y2_%d*y3_%d - 3e7*y2_%d*y2_%d;", k, k, k, k, k, k),
    sprintf("dxdt_y3_%d =  3e7*y2_%d*y2_%d;",                              k, k, k))))
  cmt  <- paste(unlist(lapply(seq_len(M), function(k) paste0(c("y1_", "y2_", "y3_"), k))),
                collapse = " ")
  main <- paste(sprintf("y1_%d_0 = 1.0;", seq_len(M)), collapse = "\n")   # y1(0)=1 per copy
  paste(c("$CMT", cmt, "$MAIN", main, "$ODE", ode), collapse = "\n")
}

# Robertson relaxes over many decades; sample log-spaced out to t = 1e6.
otimes <- 10^seq(-3, 6, length.out = 50)

bench_one <- function(M) {
  mod   <- update(mcode(paste0("cvode_bench_rob", M), robertson_code(M), compile = TRUE),
                  rtol = 1e-6, atol = 1e-10, maxsteps = 100000)
  lsoda <- mod
  cvode <- update(mod, solver = "cvode")
  sim   <- function(m) mrgsim(m, add = otimes, end = -1)

  # Correctness: LSODA vs CVODE agree (Robertson also conserves y1+y2+y3 = 1).
  ol <- as.data.frame(sim(lsoda)); oc <- as.data.frame(sim(cvode))
  num <- vapply(ol, is.numeric, logical(1))
  max_absdiff <- max(abs(as.matrix(ol[num]) - as.matrix(oc[num])))

  res <- mark(lsoda = sim(lsoda), cvode = sim(cvode), min_iterations = 5, check = FALSE)
  nm  <- as.character(res$expression); med <- as.numeric(res$median)
  data.frame(
    neq         = 3L * M,
    lsoda_ms    = 1000 * med[nm == "lsoda"],
    cvode_ms    = 1000 * med[nm == "cvode"],
    speedup     = med[nm == "lsoda"] / med[nm == "cvode"],
    max_absdiff = max_absdiff,
    row.names   = NULL)
}

cat("=== LSODA vs CVODE on a stiff system (M x Robertson) ===\n")
cat("times are per-simulation medians; speedup > 1 means CVODE is faster;\n")
cat("max_absdiff checks that LSODA and CVODE agree.\n\n")
out <- do.call(rbind, lapply(c(2, 10, 30), bench_one))
print(format(out, digits = 3), row.names = FALSE)
