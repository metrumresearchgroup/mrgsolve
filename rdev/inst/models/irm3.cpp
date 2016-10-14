$PROB
# Model: `irm3`
  - Indirect response model, type 3
      - Stimulation of response production
      - Two-compartment PK model
      - Optional nonlinear clearance
  - Source: `mrgsolve` internal library
  - Date: `r Sys.Date()`
  - Version: `r packageVersion("mrgsolve")`

$PARAM @annotated
CL   :  1  : Clearance (volume/time)
VC   : 20  : Central volume (volume)
Q    :  2  : Inter-compartmental clearance (volume/time)
VP   : 10  : Peripheral volume of distribution (volume)
KA1  :  1  : Absorption rate constant 1 (1/time)
KA2  :  1  : Absorption rate constant 2 (1/time)
KIN  : 10  : Response in rate constant (1/time)
KOUT :  2  : Response out rate constant (1/time)
EC50 :  2  : Concentration for 50% of max effect (mass/volume)
EMAX :  1  : Maximum inhibition 
n    :  1  : Emax model sigmoidicity
VMAX :  0  : Maximum reaction velocity (mass/time)
KM   :  2  : Michaelis constant (mass/volume)

$CMT  @annotated
EV1    : First extravascular compartment (mass)
CENT   : Central compartment (mass)
PERIPH : Peripheral compartment (mass)
RESP   : Response compartment  
EV2    : Second extravascular compartment (mass)
  
$GLOBAL
#define CP (CENT/VC)
#define CT (PERIPH/VP)
#define CLNL (VMAX/(KM+CP))
#define STIM (EMAX*pow(CP,n)/(pow(EC50,n)+pow(CP,n)))

$MAIN
RESP_0 = KIN/KOUT;

$ODE
dxdt_EV1    = -KA1*EV1;
dxdt_EV2    = -KA2*EV2;
dxdt_CENT   =  KA1*EV1 + KA2*EV2 - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH =  Q*CP - Q*CT;
dxdt_RESP   =  KIN*(1+STIM) - KOUT*RESP;

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
