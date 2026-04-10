$PARAM @annotated @input
CL   :  1 : Clearance (volume/time)
V    : 20 : Central volume (volume)
KA   :  1 : Absorption rate constant (1/time)

$CMT  @annotated
EV   : Extravascular compartment
CENT : Central compartment

$GLOBAL
#define CP (CENT/V)

$PKMODEL advan = 2

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)
