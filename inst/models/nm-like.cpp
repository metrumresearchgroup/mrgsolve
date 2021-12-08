$PROB Model written with some nonmem-like syntax features

$PLUGIN nm-vars autodec

$PARAM
THETA1 = 1, THETA2 = 21, THETA3 = 1.3, WT = 70, F1I = 0.5, D2I = 2
KIN = 100, KOUT = 0.1, IC50 = 10, IMAX = 0.9

$CMT @number 3

$PK
CL = THETA(1) * pow(WT/70, 0.75); 
V  = THETA(2); 
KA = THETA(3);

F1 = F1I;
D2 = D2I;
A_0(3) = KIN / KOUT;

$DES 
CP = A(2)/V;
INH = IMAX*CP/(IC50 + CP);
  
DADT(1) = -KA*A(1);
DADT(2) =  KA*A(1) - (CL/V)*A(2);
DADT(3) =  KIN * (1-INH) - KOUT * A(3);

$ERROR
CP = A(2)/V;
