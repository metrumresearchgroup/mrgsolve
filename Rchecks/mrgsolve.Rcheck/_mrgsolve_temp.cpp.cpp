//* MRGSOLVE file
#include "modelheader.h"
#ifndef MODELINCLUDEGUARD
#define INITFUN___ _model___mrgsolve__temp_main__
#define ODEFUN___ _model___mrgsolve__temp_ode__
#define TABLECODE___ _model___mrgsolve__temp_table__
#define _nEQ 2
#define _nPAR 3
#define GUT _A_[0]
#define CENT _A_[1]
#define GUT_0 _A_0_[0]
#define CENT_0 _A_0_[1]
#define dxdt_GUT _DADT_[0]
#define dxdt_CENT _DADT_[1]
#define CL _THETA_[0]
#define VC _THETA_[1]
#define KA _THETA_[2]
#define MODELINCLUDEGUARD
#endif

// GLOBAL VARIABLES:


typedef double localdouble;
typedef int localint;
typedef bool localbool;
double CLi;
double VCi;
double ke;

// MAIN CODE BLOCK:
BEGIN_main
CLi = CL*exp(ETA(1));
VCi = VC*exp(ETA(2));
ke = CLi/VCi;

END_main

// DIFFERENTIAL EQUATIONS:
BEGIN_ode
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - ke*CENT;
END_ode

// TABLE CODE BLOCK:
BEGIN_table
table(CP) = CENT/VC;
 
END_table
