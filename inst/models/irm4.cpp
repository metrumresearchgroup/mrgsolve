$PARAM
CL=1, VC=10, KA1=0.5, KA2=0.5
Q = 0, VP=10
KIN = 10, KOUT=2, EC50 = 2, EMAX=1
VMAX = 0, KM=2, n=1

$CMT EV1 CENT PERIPH RESP EV2

$GLOBAL
#define CP (CENT/VC)
#define CT (PERIPH/VP)
#define CLNL (VMAX/(KM+CP))
#define STIM (EMAX*pow(CP,n)/(pow(EC50,n)+pow(CP,n)))

$MAIN
RESP_0 = KIN/KOUT;

$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA1*EV1 + KA2*EV2 - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH = Q*CP - Q*CT;
dxdt_RESP = KIN - KOUT*(1+STIM)*RESP;

$CAPTURE CP