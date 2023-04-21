[ PROB ]
1005 phase1 2 CMT like 1004 but diff. initial on V3

Run 
file.show(system.file("nonmem", "1005", "1005.ctl", package = "mrgsolve")) 
for equivalent NONMEM control stream. 

[ PKMODEL ] cmt = "GUT CENT PERIPH", depot = TRUE

[ INPUT ] SEX = 0, WT = 70

[ NMXML ] 
project = system.file("nonmem", package = "mrgsolve")
run = "@cppstem"

[ PK ] 
double CL = THETA(1)*exp(ETA(1)) * pow(THETA(6),SEX) * pow(WT/70.0,THETA(7));
double V2 = THETA(2)*exp(ETA(2));
double KA = THETA(3)*exp(ETA(3));
double Q  = THETA(4);
double V3 = THETA(5);
double S2 = V2;

[ ERROR ] 
double F = CENT/S2;
double Y = F*(1+EPS(1)) + EPS(2); 
double IPRED = F; 

[ CAPTURE ] 
CL Q V2 V3 KA ETA(1) ETA(2) ETA(3) IPRED
