$PROB
# Model: `pk3cmt`
  - Three-compartment PK model
      - Dual first-order absorption
      - Optional nonlinear clearance from `CENT`
  - Source: `mrgsolve` internal library 
  - Date: `r Sys.Date()`
  - Version: `r packageVersion("mrgsolve")`
  
$PARAM @annotated
CL   :   1 : Clearance (volume/time)
VC   :  20 : Central volume (volume)
Q    :   2 : First inter-compartmental clearance (volume/time)
VP   :  10 : First peripheral volume (volume)
Q2   :   2 : Second inter-compartmental clearance (volume/time)
VP2  : 100 : Second peripheral volume (volume) 
KA1  :   1 : Absorption rate constant 1 (1/time)
KA2  :   1 : Absorption rate constant 2 (1/time)
VMAX :   0 : Maximum velocity (mass/time)
KM   :   2 : Michaelis Constant (mass/volume)

$CMT  @annotated
EV1     : First extravascular compartment (mass)
CENT    : Central compartment (mass)
PERIPH  : First peripheral compartment (mass) 
PERIPH2 : Second peripheral compartment (mass)
EV2     : Second extravascular compartment (mass)
  
$GLOBAL
#define CP (CENT/VC)
#define CT (PERIPH/VP)
#define CT2 (PERIPH2/VP2)
#define CLNL (VMAX/(KM+CP))

$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA1*EV1 + KA2*EV2 - (CL+CLNL+Q+Q2)*CP  + Q*CT + Q2*CT2;
dxdt_PERIPH = Q*CP - Q*CT;
dxdt_PERIPH2 = Q2*CP - Q2*CT2;

$CAPTURE @annotated 
CP : Plasma concentration (mass/volume)
