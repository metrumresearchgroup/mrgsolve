$PARAM
KPT = 0.064, KTP = 0.123, VC=0.032, KA1 = 0.142, KA2 = 0.6
KEL = 0.106
R0 = 64.31, KDEG = 0.079, KINT = 2, KON=0.101, KOFF = 10.1

$CMT EV1 CENT TISS REC RC EV2

$GLOBAL
namespace tmdd {
  double _dxdt_CP=0;
  double TMDDR0 = 0;  
}
#define KSYN (R0*KDEG)
#define CP (CENT/VC)

$MAIN
REC_0 = R0;
tmdd::TMDDR0 = _R(3);


$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
tmdd::_dxdt_CP = (tmdd::TMDDR0+KA1*EV1 + KA2*EV2)/VC - (KEL+KPT)*CP - KON*CP*REC + KOFF*RC + KTP*TISS/VC;
dxdt_CENT = tmdd::_dxdt_CP * VC;
dxdt_TISS = KPT*CP*VC - KTP*TISS;
dxdt_REC = KSYN - KDEG*REC - KON*CP*REC + KOFF*RC;
dxdt_RC = KON*CP*REC - (KINT+KOFF)*RC;

$TABLE
double TOTAL = REC+RC;

$CAPTURE CP TOTAL

