
$PROB housemodel

$PARAM
CL=1, VC=20, KA=1.2
  F1=1, WT=70, SEX=0
  WTCL=0.75, WTVC=1
  SEXCL=0.7, SEXVC=0.85
  KIN=100, KOUT=2, IC50=10

$CMT GUT CENT RESP

$OMEGA
0 0 0 0

$SIGMA
0

$SET end=120, delta=0.25

$GLOBAL
#define CP (CENT/hm::VCi)
#define INH (CP/(IC50+CP))

typedef double localdouble;

namespace hm {
  localdouble CLi = 0;
  localdouble VCi = 0;
  localdouble KAi = 0;
  localdouble KOUTi = 0;
  localdouble lWT = 0;
}

$MAIN
_F(1) = F1;

hm::CLi   = exp(log(CL)   + WTCL*log(WT/70) + log(SEXCL)*SEX + ETA(1));
hm::VCi   = exp(log(VC)   + WTVC*log(WT/70) + log(SEXVC)*SEX + ETA(2));
hm::KAi   = exp(log(KA)   + ETA(3));
hm::KOUTi = exp(log(KOUT) + ETA(4));

RESP_0 = KIN/hm::KOUTi;

$ODE
dxdt_GUT = -hm::KAi*GUT;
dxdt_CENT = hm::KAi*GUT - (hm::CLi/hm::VCi)*CENT;
dxdt_RESP = KIN*(1-INH) - hm::KOUTi*RESP;

$TABLE
capture(CP);

table(DV) = CP*exp(EPS(1));

