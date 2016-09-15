$PARAM CL = 1, V = 20, KA=1.1
$CMT DEPOT CENT
$PKMODEL ncmt=1, depot = TRUE,trans=1
$OMEGA 0 0 0
labels=s(ECL,EV,EKA)
$TABLE
double CP = CENT/pred_V;
$CAPTURE CP
$MAIN
pred_CL   = CL*exp(ETA(1));
pred_V    = V *exp(ETA(2));
pred_KA   = KA*exp(ETA(3));
