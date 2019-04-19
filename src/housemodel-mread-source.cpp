// Source MD5: 509e24de6401c4c7d8c72c3487b55e52

#include "housemodel-mread-header.h"

// PREAMBLE CODE BLOCK:
__BEGIN_config__
__END_config__

// MAIN CODE BLOCK:
__BEGIN_main__
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
dxdt_GUT = -KAi*GUT;
dxdt_CENT = KAi*GUT - (CLi/VCi)*CENT;
dxdt_RESP = KIN*(1-INH) - KOUTi*RESP;
__END_ode__

// TABLE CODE BLOCK:
__BEGIN_table__
DV = CP*exp(EXPO);
_capture_[0] = DV;
_capture_[1] = CP;
__END_table__
