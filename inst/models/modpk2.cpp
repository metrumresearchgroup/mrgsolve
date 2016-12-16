$PARAM CL = 1, V1 = 8, Q = 1, V2 = 100

$CMT CENT PERIPH

$PKMODEL ncmt=2, depot = FALSE,trans=1

$OMEGA 0 0 0 0
labels=s(ECL,EV2,EQ,EV3)

$TABLE
double CP = CENT/pred_V2;

$CAPTURE CP

$MAIN
pred_CL = CL*exp(ETA(1));
pred_V2 = V1*exp(ETA(2));
pred_Q  = Q *exp(ETA(3));
pred_V3 = V2*exp(ETA(4));
