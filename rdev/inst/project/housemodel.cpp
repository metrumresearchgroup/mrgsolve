
$PROB housemodel

$PLUGIN base

$PARAM >> annotated=TRUE
CL   : 1    : Clearance  (L/hr)
VC   : 20   : Volume of distribution (L)
KA   : 1.2  : Absorption rate constant (1/hr)
F1   : 1.0  : Bioavailability fraction (.)
WT   : 70   : Weight (kg)
SEX  : 0    : Covariate female sex
WTCL : 0.75 : Exponent WT on CL
WTVC : 1.00 : Exponent WT on VC
SEXCL: 0.7  : Prop cov effect on CL
SEXVC: 0.85 : Prop cov effect on VC
KIN  : 100  : Resp prod rate constant (1/hr)
KOUT : 2    : Resp elim rate constant (1/hr)
IC50 : 10   : Conc giving 50% max resp (ng/ml)

$CMT >> annotated=TRUE
GUT  : Dosing compartment (mg)
CENT : Central compartment (mg)
RESP : Response (unitless)

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
F_GUT = F1;

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
double DV = CP*exp(EPS(1));

$CAPTURE >> annotated=TRUE
DV: Dependent variable (ng/ml)
CP: Plasma concentration (ng/ml)

