$PROB
# Model: `pk2cmt`
  - Two-compartment PK model
      - Dual first-order absorption
      - Optional nonlinear clearance from `CENT`
  - Source: `mrgsolve` internal library
  - Date: `r Sys.Date()`
  - Version: `r packageVersion("mrgsolve")`
  
$PARAM @annotated
CL   :  1  : Clearance (volume/time)
V2   : 20  : Central volume (volume)
Q    :  2  : Inter-compartmental clearance (volume/time)
V3   : 10  : Peripheral volume of distribution (volume)
KA   :  1  : Absorption rate constant 1 (1/time)
KA2  :  1  : Absorption rate constant 2 (1/time)
VMAX :  0  : Maximum velocity (mass/time)
KM   :  2  : Michaelis Constant (mass/volume)

$CMT  @annotated
EV     : First extravascular compartment (mass)
CENT   : Central compartment (mass)
PERIPH : Peripheral compartment (mass) 
EV2    : Second extravascular compartment (mass)
  
$GLOBAL 
#define CP (CENT/V2)
#define CT (PERIPH/V3)
#define CLNL (VMAX/(KM+CP))

$ODE
dxdt_EV = -KA*EV;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA*EV + KA2*EV2 - (CL+CLNL+Q)*CP  + Q*CT;
dxdt_PERIPH = Q*CP - Q*CT;

$CAPTURE  @annotated
CP : Plasma concentration (mass/time)
  
