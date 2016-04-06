
$PARAM CL=1, VC=10, KA=1.1
$INIT GUT=0, CENT=0
$SET end=48, delta=0.25

$MAIN
double CLi = CL*exp(ETA(1));
double VCi = VC*exp(ETA(2));
double ke = CLi/VCi;

$OMEGA corr=TRUE
0.04 0.6 0.09

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - ke*CENT;

$TABLE
table(CP) = CENT/VC;

 
