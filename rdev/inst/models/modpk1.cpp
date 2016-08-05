$PARAM CL = 1, V = 20

$CMT CENT

$PKMODEL ncmt=1, depot = FALSE,trans=1

$TABLE
table(CP) = CENT/pred_V;

$OMEGA 0 0
labels=s(ECL,EV)

$MAIN
pred_CL = CL*exp(ETA(1));
pred_V  = V *exp(ETA(2));

