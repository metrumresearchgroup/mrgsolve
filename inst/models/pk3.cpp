$PARAM @annotated
CL   :  1 : Clearance (volume/time)
V2   : 20 : Central volume (volume)
Q3   :  2 : Inter-compartmental clearance 1 (volume/time)
V3   : 10 : Peripheral volume of distribution 1 (volume)
Q4   : 0.5 : Inter-compartmental clearance 2 (volume/time)
V4   : 50 : Peripheral volume of distribution 2 (volume)
KA   :  1 : Absorption rate constant (1/time)

$CMT @annotated
EV      : Extravascular compartment (mass)
CENT    : Central compartment (mass)
PERIPH1 : Peripheral compartment 1 (mass)
PERIPH2 : Peripheral compartment 2 (mass)

$GLOBAL
#define CP (CENT/V2)

$PKMODEL ncmt = 3, depot = TRUE

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
