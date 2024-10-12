$PROB
# Example model using evtools plugin

- Regimen 1 is a single infusion dose to DEPOT
- Regimen 2 is steady state bolus dosing
- Regimen 3 is a dosing regimen using `regimen` object

$PLUGIN evtools

$GLOBAL
evt::regimen reg;

$SET end = 264, delta = 0.5

$PARAM 
TVKA = 1.2, TVCL = 1, TVV = 20
Amt = 100, Dur = 0, Ii = 24, Addl = 9, Ss = 1
REG = 1, Until = 24*8

$INPUT WT = 70

$PKMODEL cmt = "DEPOT CENT", depot = TRUE

$PK 
if(REG==3 && NEWIND <= 1) reg.init(self);

double CL = exp(log(TVCL) + 0.75*log(WT/70) + ETA(1));
double V  = exp(log(TVV)  +      log(WT/70) + ETA(2));
double KA = exp(log(TVKA)                   + ETA(3));

double Rate = Dur > 0 ? Amt / Dur : 0;

$OMEGA 0.09 0.1 0.01

$SIGMA 0

$EVENT
if(TIME != 0) return;

if(REG==1) {
  evt::infuse(self, Amt, 2, Rate);  
}

if(REG==2) {
  evt::ev dose = evt::bolus(Amt, 1); 
  evt::ii(dose, Ii);
  evt::addl(dose, Addl); 
  evt::ss(dose, Ss); 
  self.push(dose);
}

if(REG==3) {
  reg.amt(Amt); 
  reg.cmt(1);
  reg.rate(Rate);
  reg.ii(Ii);
  reg.until(Until);  
}

$TABLE
capture IPRED = CENT/V;
capture DV = IPRED*exp(EPS(1));

if(REG==3) reg.execute();
