//* MRGSOLVE file
#include "modelheader.h"
#ifndef MODELINCLUDEGUARD
#define INITFUN___ _model_pkpdExample_main__
#define ODEFUN___ _model_pkpdExample_ode__
#define TABLECODE___ _model_pkpdExample_table__
#define _nEQ 3
#define _nPAR 6
#define GUT _A_[0]
#define CENT _A_[1]
#define RESP _A_[2]
#define GUT_0 _A_0_[0]
#define CENT_0 _A_0_[1]
#define RESP_0 _A_0_[2]
#define dxdt_GUT _DADT_[0]
#define dxdt_CENT _DADT_[1]
#define dxdt_RESP _DADT_[2]
#define KA _THETA_[0]
#define CL _THETA_[1]
#define VC _THETA_[2]
#define KIN _THETA_[3]
#define KOUT _THETA_[4]
#define EC50 _THETA_[5]
#define MODELINCLUDEGUARD
#endif

// GLOBAL VARIABLES:


typedef double localdouble;
typedef int localint;
typedef bool localbool;
double CP;
double INH;

// MAIN CODE BLOCK:
BEGIN_main
RESP_0 = KIN/KOUT;
END_main

// DIFFERENTIAL EQUATIONS:
BEGIN_ode
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/VC)*CENT;
CP = CENT/VC;
INH = CP/(EC50+CP);
dxdt_RESP = KIN*(1-INH) - RESP*KOUT;
END_ode

// TABLE CODE BLOCK:
BEGIN_table
capture(CP);
END_table
