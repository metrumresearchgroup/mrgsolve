[ PROB ]
1005 phase1 2 CMT like 1004 but diff. initial on V3

Run 

file.show(system.file("nonmem", "1005", "1005.ctl",package="mrgsolve")) 

for equivalent NONMEM control stream

[ PKMODEL ] cmt = "GUT CENT PERIPH", depot = TRUE

[ PARAM ] SEX = 0, WT = 70

[ NMXML ] 
project = system.file("nonmem", package = "mrgsolve")
run = 1005

[ MAIN ] 
double CL=THETA1*exp(ETA(1)) * pow(THETA6,SEX) * pow(WT/70.0,THETA7);
double V2=THETA2*exp(ETA(2));
double KA=THETA3*exp(ETA(3));
double Q =THETA4;
double V3=THETA5;
double S2=V2;

[ TABLE ] 
double F = CENT/S2;
double Y=F*(1+EPS(1)) + EPS(2); 
capture IPRED=F; 

[ CAPTURE ]
CL Q V2 V3 KA ETA(1) ETA(2) ETA(3) IPRED
