$PARAM  KA = 0.5, TVCL = 1, TVVC = 30, WT=70
$CMT GUT CENT
$SET end=36, delta=0.5

$MAIN
double CLi = exp(log(TVCL) + 0.75*log(WT/70) + ECL);
double VCi = exp(log(TVVC) +      log(WT/70) + EVC);

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CLi/VCi)*CENT;

$OMEGA
labels=s(ECL,EVC)
0 0

$SIGMA
0

$TABLE
double IPRED = CENT/VCi;
double DV = IPRED*exp(EPS(1));

$CAPTURE CLi VCi IPRED DV ECL
