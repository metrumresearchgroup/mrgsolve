
$GLOBAL 
#define CT (PERIPH/VP)
#define CP (CENT/VC)

$PARAM
WT = 70, SEX=0, EGFR=100, BMI = 20, ALT = 0.5,BLACK=0,FORM=1,FBIO=1


$THETA
  0.57 1.6 4.34
1.24 -0.078 0.3656 0.4720 0.0216 0.480
-0.0638141 0.79283 4.61 3.82 2.22 0.72 

$CMT GUT CENT  PERIPH

$MAIN

_F(1) = 1;
if(FORM==2) _F(1) = FBIO;


double LTVCL = THETA1 + THETA6*log(BMI/25) + THETA8*SEX + THETA7*log(EGFR/100);
double LTVVC = THETA2 + THETA9*log(BMI/25) + THETA10*SEX;
double LTVVP = THETA3 + THETA11*log(BMI/25);
double LTVQ =  THETA4;
double LTVKA = THETA5;

double CL =   exp(LTVCL + ETA(1));
double VC =   exp(LTVVC);
double KA =   exp(LTVKA + ETA(3));
double Q =    exp(LTVQ );
double VP =   exp(LTVVP + ETA(2));

$OMEGA 0 0 0 0

$SIGMA 0 0

$ODE
dxdt_GUT  = -KA*GUT;
dxdt_CENT =  KA*GUT - (CL+Q)*CP + Q*CT;
dxdt_PERIPH = Q*(CP - CT);


$TABLE
double DV = CP*exp(EPS(1));

$CAPTURE CP DV

  