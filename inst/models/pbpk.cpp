[ PROB ] 

# HUMAN PBPK MODEL

1: Jones H, Rowland-Yeo K. Basic concepts in physiologically based
pharmacokinetic modeling in drug discovery and development. CPT Pharmacometrics
Syst Pharmacol. 2013 Aug 14;2:e63. doi: 10.1038/psp.2013.41. PubMed PMID:
23945604; PubMed Central PMCID: PMC3828005.

[ SET ] end = 24, delta = 0.25

[ CMT ] 
D     //; dose
Aad   //; adipose
Abo   //; bone
Abr   //; brain
Agu   //; gut
Ahe   //; heart
Aki   //; kidney
Ali   //; liver
Alu   //; lung
Amu   //; muscle
Ask   //; skin
Asp   //; spleen
Ate   //; testes
Ave   //; venous blood
Aar   //; arterial blood
Are   //; rest of body


[ PARAM ] 

BW = 70	//; BW (kg)

// {Fractional tissue volumes}

FVad = 0.213     //; adipose
FVbo = 0.085629  //; bone
FVbr = 0.02      //; brain
FVgu = 0.0171    //; gut
FVhe = 0.0047    //; heart
FVki = 0.0044    //; kidney
FVli = 0.021     //; liver 
FVlu = 0.0076    //; lung
FVmu = 0.4       //; muscle
FVsk = 0.0371    //; skin
FVsp = 0.0026    //; spleen 
FVte = 0.01      //; testes 
FVve = 0.0514    //; venous
FVar = 0.0257    //; arterial 
FVpl = 0.0424    //; plasma 
FVrb = 0.0347    //; erythrocytes
FVre = 0.099771  //; rest of body 

// {Fractional tissue blood flows}

FQad = 0.05      //; adipose 
FQbo = 0.05      //; bone
FQbr = 0.12      //; brain 
FQgu = 0.146462  //; gut
FQhe = 0.04      //; heart 
FQki = 0.19      //; kidney 
FQh  = 0.215385  //; hepatic (venous side) 
FQlu = 1         //; lung
FQmu = 0.17      //; muscle 
FQsk = 0.05      //; skin 
FQsp = 0.017231  //; spleen
FQte = 0.01076   //; testes
FQre = 0.103855  //; rest of body

// {COMPOUND SPECIFIC PARAMETERS}

// {Tissue to plasma partition coefficients}

Kpad = 0.191  //; adipose
Kpbo = 0.374  //; bone
Kpbr = 0.606  //; brain 
Kpgu = 0.578  //; gut
Kphe = 0.583  //; heart
Kpki = 0.597  //; kidney
Kpli = 0.570  //; liver
Kplu = 0.620  //; lung
Kpmu = 0.622  //; muscle
Kpsk = 0.600  //; skin
Kpsp = 0.591  //; spleen
Kpte = 0.600  //; testes
Kpre = 0.600  //; rest of body

// {In vitro binding data}

fup   = 0.681  //; fraction unbound in plasma
BP    = 0.98  //; blood to plasma ratio
fumic = 1  //; fraction unbound in microsomes

// {Clearances}

HLM_CLint =  8   //; HLM CLint apparent (ul/min/mg) 
CLrenal   =  0   //; CLint renal (L/hr)

// {Absorption}

Ka = 2.18     //; Ka (hr-1)
F  = 1.00     //; fraction absorbed
CO = 108.33   //; cardiac output (ml/s)

[ MAIN  ]

// {Total tissue volumes - L}
double Vad = BW*FVad;  // adipose 
double Vbo = BW*FVbo;  // bone 
double Vbr = BW*FVbr;  // brain 
double Vgu = BW*FVgu;  // gut 
double Vhe = BW*FVhe;  // heart 
double Vki = BW*FVki;  // kidney
double Vli = BW*FVli;  // liver 
double Vlu = BW*FVlu;  // lung
double Vmu = BW*FVmu;  // muscle
double Vsk = BW*FVsk;  // skin 
double Vsp = BW*FVsp;  // spleen
double Vte = BW*FVte;  // testes
double Vve = BW*FVve;  // venous blood
double Var = BW*FVar;  // arterial blood
double Vpl = BW*FVpl;  // plasma 
double Vrb = BW*FVrb;  // erythrocytes 
double Vre = BW*FVre;  // rest of body

double Vplas_ven = Vpl*Vve/(Vve + Var) 	;  // venous plasma
double Vplas_art = Vpl*Var/(Vve + Var) 	;  // arterial plasma

// {Total tissue blood flows - L/hr}

double QC  = CO/1000*60*60 ;  // cardiac output (L/hr)
double Qad = QC*FQad       ;  // adipose 
double Qbo = QC*FQbo       ;  // bone 
double Qbr = QC*FQbr       ;  // brain 
double Qgu = QC*FQgu       ;  // gut
double Qhe = QC*FQhe       ;  // heart 
double Qki = QC*FQki       ;  // kidney 
double Qh  = QC*FQh        ;  // hepatic (venous side)
double Qha = Qh - Qgu - Qsp;  // hepatic artery 
double Qlu = QC*FQlu       ;  // lung 
double Qmu = QC*FQmu       ;  // muscle 
double Qsk = QC*FQsk       ;  // skin 
double Qsp = QC*FQsp       ;  // spleen 
double Qte = QC*FQte       ;  // testes 
double Qre = QC*FQre       ;  // rest of body


[ ODE ]

double Cadipose  = Aad/Vad;  // adipose 
double Cbone     = Abo/Vbo;  // bone 
double Cbrain    = Abr/Vbr;  // brain
double Cgut      = Agu/Vgu;  // gut
double Cheart    = Ahe/Vhe;  // heart 
double Ckidney   = Aki/Vki;  // kidney 
double Cliver    = Ali/Vli;  // liver 
double Clung     = Alu/Vlu;  // lung 
double Cmuscle   = Amu/Vmu;  // muscle
double Cskin     = Ask/Vsk;  // skin 
double Cspleen   = Asp/Vsp;  // spleen 
double Ctestes   = Ate/Vte;  // testes 
double Cvenous   = Ave/Vve;  // venous blood
double Carterial = Aar/Var;  // arterial blood
double Crest     = Are/Vre;  // rest of body

// {Calculation of free concentrations - mg/L}

double Cliverfree  = Cliver*fup;  // liver 
double Ckidneyfree = Ckidney*fup; // kidney 

// {Clearance calculations}

double MPPGL = 45; // mg microsomal protein per g liver
double CLmet = (HLM_CLint/fumic)*MPPGL*Vli*60/1000; // CLint scaled (L/hr)

double Venous = 
  Qad*(Cadipose/Kpad*BP) + Qbo*(Cbone/Kpbo*BP)   + 
  Qbr*(Cbrain/Kpbr*BP)   + Qhe*(Cheart/Kphe*BP)  + Qki*(Ckidney/Kpki*BP) + 
  Qh*(Cliver/Kpli*BP)    + Qmu*(Cmuscle/Kpmu*BP) + Qsk*(Cskin/Kpsk*BP) + 
  Qte*(Ctestes/Kpte*BP)  + Qre*(Crest/Kpre*BP);

double Absorption = Ka*D; //*F

dxdt_Aad = Qad*(Carterial - Cadipose/Kpad*BP);    // adipose
dxdt_Abo = Qbo*(Carterial - Cbone/Kpbo*BP);       // bone
dxdt_Abr = Qbr*(Carterial - Cbrain/Kpbr*BP);      // brain
dxdt_Agu = Absorption + 
           Qgu*(Carterial - Cgut/Kpgu*BP);        // gut
dxdt_Ahe = Qhe*(Carterial - Cheart/Kphe*BP);      // heart
dxdt_Aki = Qki*(Carterial - Ckidney/Kpki*BP) - 
           CLrenal*Ckidneyfree;                   // kidney
dxdt_Ali = Qha*Carterial + 
           Qgu*(Cgut/Kpgu*BP) + 
           Qsp*(Cspleen/Kpsp*BP) - 
           Qh*(Cliver/Kpli*BP) - 
           Cliverfree*CLmet;                      // liver 
dxdt_Alu = Qlu*Cvenous - Qlu*(Clung/Kplu*BP);     // lung
dxdt_Amu = Qmu*(Carterial - Cmuscle/Kpmu*BP);     // muscle
dxdt_Ask = Qsk*(Carterial - Cskin/Kpsk*BP);       // skin
dxdt_Asp = Qsp*(Carterial - Cspleen/Kpsp*BP);     // spleen
dxdt_Ate = Qte*(Carterial - Ctestes/Kpte*BP);     // testes
dxdt_Ave = Venous - Qlu*Cvenous;                  // venous blood
dxdt_Aar = Qlu*(Clung/Kplu*BP) - Qlu*Carterial;   // arterial blood
dxdt_Are = Qre*(Carterial - Crest/Kpre*BP);       // rest of body
dxdt_D   = - Absorption;                          // oral dosing

[ CAPTURE ] Cvenous = Ave/Vve

[ TABLE ] 
capture Cp = Cvenous/BP	; // venous plasma 

  