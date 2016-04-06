$PARAM  KA = 0.5, TVCL = 1, TVVC = 30, WT=70
$CMT GUT CENT
$SET end=36, delta=0.5

$MAIN
double CLi = exp(log(TVCL) + 0.75*log(WT/70) + ETA(1));
double VCi = exp(log(TVVC) +      log(WT/70) + ETA(2));

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CLi/VCi)*CENT;

$OMEGA
0 0

$SIGMA
0

$TABLE
table(IPRED) = CENT/VCi;
table(DV) = table(IPRED)*exp(EPS(1));
table(ETA1)  = ETA(1);

$CAPTURE CLi VCi
