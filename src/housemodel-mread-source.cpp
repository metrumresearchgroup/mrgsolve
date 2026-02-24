// Source MD5: 8b31b620adebe9145ea11a12f5d6800b

#include "housemodel-mread-header.h"

// PREAMBLE CODE BLOCK:
__BEGIN_config__
CHECK_MODELED_INFUSIONS=false;
#include "mread-housemodel-decode-preamble.h"
__END_config__

// MAIN CODE BLOCK:
__BEGIN_main__
#include "mread-housemodel-decode-main.h"
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
#include "mread-housemodel-decode-ode.h"
dxdt_GUT = -KAi*GUT;
dxdt_CENT = KAi*GUT - (CLi/VCi)*CENT;
dxdt_RESP = KIN*(1-INH) - KOUTi*RESP;
__END_ode__

// MODELED EVENTS:
__BEGIN_event__
#include "mread-housemodel-decode-event.h"
__END_event__

// TABLE CODE BLOCK:
__BEGIN_table__
#include "mread-housemodel-decode-table.h"
DV = CP*exp(EXPO);
_capture_[0] = DV;
_capture_[1] = CP;
__END_table__

