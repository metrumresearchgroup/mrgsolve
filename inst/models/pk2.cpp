$PARAM @annotated
CL   :  1 : Clearance (volume/time)
V2   : 20 : Central volume (volume)
Q    :  2 : Inter-compartmental clearance (volume/time)
V3   : 10 : Peripheral volume of distribution (volume)
KA   :  1 : Absorption rate constant (1/time)

$CMT @annotated
EV     : Extravascular compartment (mass)
CENT   : Central compartment (mass)
PERIPH : Peripheral compartment (mass) 

$GLOBAL
#define CP (CENT/V2)

$PKMODEL ncmt = 2, depot = TRUE

$CAPTURE @annotated
CP : Plasma concentration (mass/time)
  
