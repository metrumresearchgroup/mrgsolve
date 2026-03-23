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
V2   :  20 : Central volume (volume)
Q3   :   2 : First inter-compartmental clearance (volume/time)
V3   :  10 : First peripheral volume (volume)
Q4   : 0.5 : Second inter-compartmental clearance (volume/time)
V4   :  50 : Second peripheral volume (volume) 
KA1  :   1 : Absorption rate constant 1 (1/time)
KA2  :   1 : Absorption rate constant 2 (1/time)
VMAX :   0 : Maximum velocity (mass/time)
KM   :   2 : Michaelis Constant (mass/volume)

$CMT  @annotated
EV1     : First extravascular compartment (mass)
CENT    : Central compartment (mass)
PERIPH1 : First peripheral compartment (mass) 
PERIPH2 : Second peripheral compartment (mass)
EV2     : Second extravascular compartment (mass)
  
$GLOBAL
#define CP (CENT/V2)
#define CT (PERIPH1/V3)
#define CT2 (PERIPH2/V4)
#define CLNL (VMAX/(KM+CP))

$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA1*EV1 + KA2*EV2 - (CL+CLNL+Q3+Q4)*CP  + Q3*CT + Q4*CT2;
dxdt_PERIPH1 = Q3*CP - Q3*CT;
dxdt_PERIPH2 = Q4*CP - Q4*CT2;

$CAPTURE @annotated 
CP : Plasma concentration (mass/volume)
