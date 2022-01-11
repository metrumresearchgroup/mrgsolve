[ PROB ]
1005 phase1 2 CMT like 1004 but diff. initial on V3

Run 
file.show(system.file("nonmem", "1005", "1005.ctl", package = "mrgsolve")) 
for equivalent NONMEM control stream. 

Updated 10 Jan 2022 to use autodec and nm-vars plugins.

[ PLUGIN ] autodec nm-vars
  
[ CMT ] @number 3
  
[ PKMODEL ] ncmt = 2, depot = TRUE

[ PARAM ] SEX = 0, WT = 70

[ NMXML ] 
project = system.file("nonmem", package = "mrgsolve")
run = 1005

[ PK ] 
CL = THETA(1)*exp(ETA(1)) * pow(THETA(6),SEX) * pow(WT/70.0,THETA(7));
V2 = THETA(2)*exp(ETA(2));
KA = THETA(3)*exp(ETA(3));
Q  = THETA(4);
V3 = THETA(5);
S2 = V2;

[ ERROR ] 
F = A2/S2;
Y = F*(1+EPS(1)) + EPS(2); 
IPRED = F; 

[ CAPTURE ]
CL Q V2 V3 KA ETA(1) ETA(2) ETA(3) IPRED
