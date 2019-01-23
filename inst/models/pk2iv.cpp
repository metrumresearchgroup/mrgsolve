$PARAM @annotated
CL   :  1 : Clearance (volume/time)
V1   : 20 : Central volume (volume)
Q    :  2 : Inter-compartmental clearance (volume/time)
V2   : 10 : Peripheral volume of distribution (volume)

$CMT @annotated
CENT   : Central compartment (mass)
PERIPH : Peripheral compartment (mass) 

$GLOBAL
#define CP (CENT/V2)

$PKMODEL ncmt = 2, depot = FALSE

$CAPTURE @annotated
CP : Plasma concentration (mass/time)
  
