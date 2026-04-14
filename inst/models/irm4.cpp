$PARAM
CL=1, V2=10, KA=0.5, KA2=0.5
Q = 0, V3=10
KIN = 10, KOUT=2, EC50 = 2, EMAX=1
VMAX = 0, KM=2, n=1

$CMT EV CENT PERIPH RESP EV2

$GLOBAL
#define CP (CENT/V2)
#define CT (PERIPH/V3)
#define CLNL (VMAX/(KM+CP))
#define STIM (EMAX*pow(CP,n)/(pow(EC50,n)+pow(CP,n)))

$MAIN
RESP_0 = KIN/KOUT;

$ODE
dxdt_EV     = -KA*EV;
dxdt_EV2    = -KA2*EV2;
dxdt_CENT   =  KA*EV + KA2*EV2 - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH =  Q*CP - Q*CT;
dxdt_RESP   =  KIN - KOUT*(1+STIM)*RESP;

$CAPTURE CP
