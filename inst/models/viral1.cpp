
$PARAM
p=25, c=8, delta=0.4, s=61.7E3, d=1/300
beta = 5.5E-7
AUC = 0, IC50=2, eta=0

$INIT expos=0,T = 1126801, I = 1126801, V = 8148974

$SET delta = 02.5

$GLOBAL
#define eps (expos/(IC50+expos))

$MAIN
T_0 = c*delta/(beta*p);
V_0 = (s*p*beta - d*c*delta)/(delta*c*beta);
I_0 = (s*p*beta - d*c*delta)/(delta*p*beta);
expos_0 = 0;

$ODE
dxdt_T = s - d*T - (1-eta)*beta*V*T;
dxdt_I = (1-eta)*beta*V*T - delta*I;
dxdt_V = (1-eps)*p*I - c*V;
dxdt_expos = 0;

$TABLE
double logV = log10(V);
double logChange = log10(V) - log10(V_0);

$CAPTURE logV logChange


