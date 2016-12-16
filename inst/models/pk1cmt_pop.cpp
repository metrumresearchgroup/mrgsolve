$PARAM
CL=1, VC=10, KA1=1, KA2=1, F1=1
TVCL = 1, TVVC = 10, TVKA1 = 1, TVKA2 = 1
VMAX = 0, KM=2

$CMT EV1 CENT EV2

$GLOBAL
#define CT (PERIPH/VP)
#define CLNL (VMAX/(KM+pop1::CP))

namespace pop1 {
  double CLi=0, VCi = 0, KA1i = 0, KA2i= 0, CP = 0; 
}

$OMEGA 0.1 0.2 0.3 0.4

$MAIN
  pop1::CLi = TVCL*exp(ETA(1));
  pop1::VCi = TVVC*exp(ETA(2));
  pop1::KA1i = TVKA1*exp(ETA(3));
  pop1::KA2i = TVKA2*exp(ETA(4));
  F_EV1 = F1;

$ODE
pop1::CP = CENT/pop1::VCi;

dxdt_EV1 = -pop1::KA1i*EV1;
dxdt_EV2 = -pop1::KA2i*EV2;

dxdt_CENT = pop1::KA1i*EV1 + pop1::KA2i*EV2 - (pop1::CLi+CLNL)*pop1::CP;

$TABLE
double CP = pop1::CP*(1+EPS(1)); 

$CAPTURE CP



