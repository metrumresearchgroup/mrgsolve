$PARAM
KA = 1.2, CL = 1, VC = 25
KIN = 100, KOUT = 2, EC50 = 4

$CMT GUT CENT RESP

$SET delta= 0.25

$GLOBAL
#define CP (CENT/VC)

$MAIN
RESP_0 = KIN/KOUT;

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/VC)*CENT;

double INH = CP/(EC50+CP);
dxdt_RESP = KIN*(1-INH) - RESP*KOUT;

$CAPTURE CP




