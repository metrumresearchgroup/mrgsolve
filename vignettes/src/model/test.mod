$PARAM CL = 1, V = 20, TVKA = 1.2

$OMEGA 0.1 0.2

$PKMODEL cmt = "GUT CENT", depot = TRUE

$MAIN
double KA = TVKA + ETA(1);

$TABLE
capture CP = CENT/V;


