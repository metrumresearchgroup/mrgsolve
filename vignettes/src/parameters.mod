$PLUGIN autodec

$INPUT
WT   =  70
SEX  =   0
EGFR = 100

$PARAM
THETA1 = log(1)
THETA2 = log(20)
THETA3 = log(1.3)
THETA4 = 0.331
THETA5 = log(0.81)

$PK
MU_1 = THETA1 + 0.75*log(WT/70.0) + THETA4*log(EGFR/100.0) + THETA5*SEX;
CL   = exp(MU_1 + ETA(1));

MU_2 = THETA2 * 1.00*log(WT/70.0);
V    = exp(MU_2 + ETA(2));

MU_3 = THETA3;
KA   = exp(MU_3 + ETA(3));

$OMEGA 0.1 0.2 0.5

$PKMODEL cmt = "A1,A2", depot = TRUE

$ERROR 
capture CP = A2/V;

$CAPTURE WT CL
