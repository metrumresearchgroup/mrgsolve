$PARAM
CL=1, V2=10, KA=0.5, KA2=0.5
Q=0, V3=10, VMAX=0, KM=2
E0=0, EMAX=10, EC50=30, n=1

$CMT EV1 CENT PERIPH EV2

$GLOBAL
#define CP (CENT/V2)
#define CT (PERIPH/V3)
#define CLNL (VMAX/(KM+CP))
#define RESP (E0+EMAX*pow(CP,n)/(pow(EC50,n)+pow(CP,n)))

$ODE
dxdt_EV1 = -KA *EV1;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA *EV1 + KA2*EV2 - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH = Q*CP - Q*CT;

$CAPTURE CP RESP
  