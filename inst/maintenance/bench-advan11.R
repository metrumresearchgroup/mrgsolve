library(mrgsolve)
stopifnot(require("bench"))

# --- Model definitions -------------------------------------------------------

ode_code <- '
$PARAM CL=1, VC=20, Q=4, KA=1.1, VP=300, Q2=0.8, VP2=50
$CMT GUT CENT PER1 PER2
$ODE
double k10 = CL/VC;
double k12 = Q/VC;
double k21 = Q/VP;
double k13 = Q2/VC;
double k31 = Q2/VP2;

dxdt_GUT  = -KA*GUT;
dxdt_CENT = KA*GUT - (k10+k12+k13)*CENT + k21*PER1 + k31*PER2;
dxdt_PER1 = k12*CENT - k21*PER1;
dxdt_PER2 = k13*CENT - k31*PER2;
'

pk_code <- '
$PARAM CL=1, V2=20, KA=1.1, Q3=4, V3=300, Q4=0.8, V4=50
$PKMODEL cmt = "GUT CENT PER1 PER2", depot = TRUE
'

mod_ode <- mcode("bench_ode3", ode_code, compile = TRUE)
mod_pk  <- mcode("bench_pk3",  pk_code,  compile = TRUE)

# --- Scenario 1: Single dose, dense sampling ---------------------------------
cat("=== Scenario 1: Single oral dose, dense sampling (1 ID) ===\n")
e1 <- ev(amt = 100)
print(mark(
  ODE    = mrgsim(mod_ode, e1, end = 72, delta = 0.1),
  ADVAN12 = mrgsim(mod_pk,  e1, end = 72, delta = 0.1),
  min_iterations = 50,
  check = FALSE
))

# --- Scenario 2: Multiple doses with addl ------------------------------------
cat("\n=== Scenario 2: Multiple oral doses (10 IDs, ii=24, addl=27) ===\n")
data2 <- expand.ev(ID = 1:10, amt = 100, ii = 24, addl = 27)
stime2 <- seq(0, 672, by = 1)
print(mark(
  ODE     = mrgsim(mod_ode, data = data2, stime = stime2),
  ADVAN12 = mrgsim(mod_pk,  data = data2, stime = stime2),
  min_iterations = 20,
  check = FALSE
))

# --- Scenario 3: Heavy addl --------------------------------------------------
cat("\n=== Scenario 3: Heavy addl (10 IDs, ii=12, addl=100) ===\n")
data3 <- expand.ev(ID = 1:10, amt = 100, ii = 12, addl = 100)
stime3 <- seq(0, 1200, by = 1)
print(mark(
  ODE     = mrgsim(mod_ode, data = data3, stime = stime3),
  ADVAN12 = mrgsim(mod_pk,  data = data3, stime = stime3),
  min_iterations = 20,
  check = FALSE
))

# --- Scenario 4: Infusion with addl ------------------------------------------
cat("\n=== Scenario 4: Infusion + addl (10 IDs, rate=10, addl=50) ===\n")
data4 <- expand.ev(ID = 1:10, amt = 100, rate = 10, ii = 24, addl = 50)
stime4 <- seq(0, 1200, by = 1)
print(mark(
  ODE     = mrgsim(mod_ode, data = data4, stime = stime4),
  ADVAN12 = mrgsim(mod_pk,  data = data4, stime = stime4),
  min_iterations = 20,
  check = FALSE
))

# --- Scenario 5: Large population --------------------------------------------
cat("\n=== Scenario 5: Large population (1000 IDs, simple dosing) ===\n")
data5 <- expand.ev(ID = 1:1000, amt = 100, ii = 24, addl = 6)
stime5 <- seq(0, 168, by = 0.5)
print(mark(
  ODE     = mrgsim(mod_ode, data = data5, stime = stime5),
  ADVAN12 = mrgsim(mod_pk,  data = data5, stime = stime5),
  min_iterations = 10,
  check = FALSE
))

# --- Scenario 6: IV bolus into central (no depot) ----------------------------
cat("\n=== Scenario 6: IV bolus, no depot (100 IDs, addl=27) ===\n")

ode_nd_code <- '
$PARAM CL=1, VC=20, Q=4, VP=300, Q2=0.8, VP2=50
$CMT CENT PER1 PER2
$ODE
double k10 = CL/VC;
double k12 = Q/VC;
double k21 = Q/VP;
double k13 = Q2/VC;
double k31 = Q2/VP2;

dxdt_CENT = -(k10+k12+k13)*CENT + k21*PER1 + k31*PER2;
dxdt_PER1 = k12*CENT - k21*PER1;
dxdt_PER2 = k13*CENT - k31*PER2;
'

pk_nd_code <- '
$PARAM CL=1, V1=20, Q2=4, V2=300, Q3=0.8, V3=50
$PKMODEL cmt = "CENT PER1 PER2", depot = FALSE
'

mod_ode_nd <- mcode("bench_ode3nd", ode_nd_code, compile = TRUE)
mod_pk_nd  <- mcode("bench_pk3nd",  pk_nd_code,  compile = TRUE)

data6 <- expand.ev(ID = 1:100, amt = 100, cmt = 1, ii = 24, addl = 27)
stime6 <- seq(0, 672, by = 1)
print(mark(
  ODE     = mrgsim(mod_ode_nd, data = data6, stime = stime6),
  ADVAN11 = mrgsim(mod_pk_nd,  data = data6, stime = stime6),
  min_iterations = 10,
  check = FALSE
))

# --- Scenario 7: 3000 IDs, daily oral doses over 4 weeks --------------------
cat("\n=== Scenario 7: 3000 IDs, daily oral x 28 days ===\n")
data7 <- expand.ev(ID = 1:3000, amt = 100, ii = 24, addl = 27)
stime7 <- seq(0, 672, by = 1)
print(mark(
  ODE     = mrgsim(mod_ode, data = data7, stime = stime7),
  ADVAN12 = mrgsim(mod_pk,  data = data7, stime = stime7),
  min_iterations = 5,
  check = FALSE
))
