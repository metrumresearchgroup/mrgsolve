$PARAM CL = 1, V2 = 8, Q = 1, V3 = 100, KA=1.1

$CMT DEPOT CENT PERIPH

$PKMODEL ncmt=2, depot = TRUE,trans=1

$OMEGA 0 0 0 0 0
labels=s(ECL,EV2,EQ,EV3,EKA)

$TABLE
double CP = CENT/pred_V2;

$CAPTURE CP

$MAIN
pred_CL = CL*exp(ETA(1));
pred_V2 = V2*exp(ETA(2));
pred_Q =  Q *exp(ETA(3));
pred_V3 = V3*exp(ETA(4));
pred_KA = KA*exp(ETA(5));