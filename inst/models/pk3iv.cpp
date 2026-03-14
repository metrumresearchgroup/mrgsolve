$PARAM @annotated
CL   :  1 : Clearance (volume/time)
V1   : 20 : Central volume (volume)
Q2   :  2 : Inter-compartmental clearance 1 (volume/time)
V2   : 10 : Peripheral volume of distribution 1 (volume)
Q3   : 0.5 : Inter-compartmental clearance 2 (volume/time)
V3   : 50 : Peripheral volume of distribution 2 (volume)

$CMT @annotated
CENT    : Central compartment (mass)
PERIPH1 : Peripheral compartment 1 (mass)
PERIPH2 : Peripheral compartment 2 (mass)

$GLOBAL
#define CP (CENT/V1)

$PKMODEL ncmt = 3, depot = FALSE

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
