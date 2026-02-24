// Source MD5: 8b31b620adebe9145ea11a12f5d6800b

#include "housemodel-mread-header.h"

// PREAMBLE CODE BLOCK:
__BEGIN_config__
CHECK_MODELED_INFUSIONS=false;
#include "mrgsolve-unused-variable-start.h"
const double& CL = _THETA_[0];
const double& VC = _THETA_[1];
const double& KA = _THETA_[2];
const double& F1 = _THETA_[3];
const double& D1 = _THETA_[4];
const double& WTCL = _THETA_[5];
const double& WTVC = _THETA_[6];
const double& SEXCL = _THETA_[7];
const double& SEXVC = _THETA_[8];
const double& KIN = _THETA_[9];
const double& KOUT = _THETA_[10];
const double& IC50 = _THETA_[11];
const double& WT = _THETA_[12];
const double& SEX = _THETA_[13];
#include "mrgsolve-unused-variable-end.h"
__END_config__

// MAIN CODE BLOCK:
__BEGIN_main__
#include "mrgsolve-unused-variable-start.h"
const double& CL = _THETA_[0];
const double& VC = _THETA_[1];
const double& KA = _THETA_[2];
const double& F1 = _THETA_[3];
const double& D1 = _THETA_[4];
const double& WTCL = _THETA_[5];
const double& WTVC = _THETA_[6];
const double& SEXCL = _THETA_[7];
const double& SEXVC = _THETA_[8];
const double& KIN = _THETA_[9];
const double& KOUT = _THETA_[10];
const double& IC50 = _THETA_[11];
const double& WT = _THETA_[12];
const double& SEX = _THETA_[13];
double& GUT_0 = _A_0_[0];
double& CENT_0 = _A_0_[1];
double& RESP_0 = _A_0_[2];
const double& GUT = _A_[0];
const double& CENT = _A_[1];
const double& RESP = _A_[2];
double& F_GUT = _F_[0];
double& F_CENT = _F_[1];
double& R_GUT = _R_[0];
double& R_CENT = _R_[1];
double& D_GUT = _D_[0];
double& D_CENT = _D_[1];
double& ALAG_GUT = _ALAG_[0];
double& ALAG_CENT = _ALAG_[1];
#include "mrgsolve-unused-variable-end.h"
F_GUT = F1;
D_CENT = D1;
CLi   = exp(log(CL)   + WTCL*log(WT/70) + log(SEXCL)*SEX + ECL);
VCi   = exp(log(VC)   + WTVC*log(WT/70) + log(SEXVC)*SEX + EVC);
KAi   = exp(log(KA)   + EKA);
KOUTi = exp(log(KOUT) + EKOUT);
RESP_0 = KIN/KOUTi;
__END_main__

// DIFFERENTIAL EQUATIONS:
__BEGIN_ode__
#include "mrgsolve-unused-variable-start.h"
const double& CL = _THETA_[0];
const double& VC = _THETA_[1];
const double& KA = _THETA_[2];
const double& F1 = _THETA_[3];
const double& D1 = _THETA_[4];
const double& WTCL = _THETA_[5];
const double& WTVC = _THETA_[6];
const double& SEXCL = _THETA_[7];
const double& SEXVC = _THETA_[8];
const double& KIN = _THETA_[9];
const double& KOUT = _THETA_[10];
const double& IC50 = _THETA_[11];
const double& WT = _THETA_[12];
const double& SEX = _THETA_[13];
const double& GUT = _A_[0];
const double& CENT = _A_[1];
const double& RESP = _A_[2];
double& dxdt_GUT = _DADT_[0];
double& dxdt_CENT = _DADT_[1];
double& dxdt_RESP = _DADT_[2];
const double& GUT_0 = _A_0_[0];
const double& CENT_0 = _A_0_[1];
const double& RESP_0 = _A_0_[2];
#include "mrgsolve-unused-variable-end.h"
dxdt_GUT = -KAi*GUT;
dxdt_CENT = KAi*GUT - (CLi/VCi)*CENT;
dxdt_RESP = KIN*(1-INH) - KOUTi*RESP;
__END_ode__

// MODELED EVENTS:
__BEGIN_event__
#include "mrgsolve-unused-variable-start.h"
const double& CL = _THETA_[0];
const double& VC = _THETA_[1];
const double& KA = _THETA_[2];
const double& F1 = _THETA_[3];
const double& D1 = _THETA_[4];
const double& WTCL = _THETA_[5];
const double& WTVC = _THETA_[6];
const double& SEXCL = _THETA_[7];
const double& SEXVC = _THETA_[8];
const double& KIN = _THETA_[9];
const double& KOUT = _THETA_[10];
const double& IC50 = _THETA_[11];
const double& WT = _THETA_[12];
const double& SEX = _THETA_[13];
const double& GUT_0 = _A_0_[0];
const double& CENT_0 = _A_0_[1];
const double& RESP_0 = _A_0_[2];
const double& GUT = _A_[0];
const double& CENT = _A_[1];
const double& RESP = _A_[2];
const double& F_GUT = _F_[0];
const double& F_CENT = _F_[1];
const double& R_GUT = _R_[0];
const double& R_CENT = _R_[1];
const double& D_GUT = _D_[0];
const double& D_CENT = _D_[1];
const double& ALAG_GUT = _ALAG_[0];
const double& ALAG_CENT = _ALAG_[1];
#include "mrgsolve-unused-variable-end.h"
__END_event__

// TABLE CODE BLOCK:
__BEGIN_table__
#include "mrgsolve-unused-variable-start.h"
const double& CL = _THETA_[0];
const double& VC = _THETA_[1];
const double& KA = _THETA_[2];
const double& F1 = _THETA_[3];
const double& D1 = _THETA_[4];
const double& WTCL = _THETA_[5];
const double& WTVC = _THETA_[6];
const double& SEXCL = _THETA_[7];
const double& SEXVC = _THETA_[8];
const double& KIN = _THETA_[9];
const double& KOUT = _THETA_[10];
const double& IC50 = _THETA_[11];
const double& WT = _THETA_[12];
const double& SEX = _THETA_[13];
const double& GUT_0 = _A_0_[0];
const double& CENT_0 = _A_0_[1];
const double& RESP_0 = _A_0_[2];
const double& GUT = _A_[0];
const double& CENT = _A_[1];
const double& RESP = _A_[2];
const double& F_GUT = _F_[0];
const double& F_CENT = _F_[1];
const double& R_GUT = _R_[0];
const double& R_CENT = _R_[1];
const double& D_GUT = _D_[0];
const double& D_CENT = _D_[1];
const double& ALAG_GUT = _ALAG_[0];
const double& ALAG_CENT = _ALAG_[1];
#include "mrgsolve-unused-variable-end.h"
DV = CP*exp(EXPO);
_capture_[0] = DV;
_capture_[1] = CP;
__END_table__

