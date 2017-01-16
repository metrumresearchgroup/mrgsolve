$PROB
# Example population PK model

$PARAM TVKA = 0.5, TVCL = 1, TVV = 24, WT=70

$PKMODEL cmt="GUT CENT", depot=TRUE

$SET end=240, delta=0.5

$MAIN
double CL = exp(log(TVCL) + 0.75*log(WT/70) + ECL);
double V  = exp(log(TVV)  +      log(WT/70) + EV );
double KA = exp(log(TVKA)                   + EKA);

$OMEGA @labels ECL EV EKA
0.3 0.1 0.5

$SIGMA 0

$TABLE
double IPRED = CENT/V;
double DV = IPRED*exp(EPS(1));

$CAPTURE CL V IPRED DV ECL

  