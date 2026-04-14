[ PROB ]
1005 phase1 2 CMT like 1004 but diff. initial on V3

Run 
file.show(system.file("nonmem", "1005", "1005.ctl", package = "mrgsolve")) 
for equivalent NONMEM control stream. 

[ NMXML ] 
project = system.file("nonmem", package = "mrgsolve")
run = "@cppstem"

[ PLUGIN ] autodec

[ PKMODEL ] advan = 4

[ CMT ] GUT CENT PERIPH

[ INPUT ] SEX = 0, WT = 70

[ PK ] 
CL = THETA(1)*exp(ETA(1))*THETA(6)**SEX*(WT/70)**THETA(7);
V2 = THETA(2)*exp(ETA(2));
KA = THETA(3)*exp(ETA(3));
Q  = THETA(4);
V3 = THETA(5);
S2 = V2;

[ ERROR ] 
F = CENT/S2;
Y = F*(1+EPS(1)) + EPS(2);
IPRED = F;

[ CAPTURE ] 
CL Q V2 V3 KA ETA(1) ETA(2) ETA(3) IPRED
