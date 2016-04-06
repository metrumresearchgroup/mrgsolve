#include "viralExample.h"

// @param: p= 45, fit=0.5, c = 6
// @param: delta=0.4, s=0, d=1/300, beta=5.5E-8
// @param: eps1 = 0.995, eps2 = 0.75
// @param: N = 6.5E6
// @param: mu=6.14E-5, Tmax = 1.3E7, rho = 1.25
// @init: T = 1126801, I = 1126801, V = 8148974
// @init: VM = 87.8, IM = 12.1

double a = 0, b=1;

BEGIN_main
   T_0 = c*delta/((1-mu)*beta*p);

   a = s + rho*T_0*(1-((T_0+N)/Tmax)) - d*T_0;
   b = beta*T_0 * ((1-fit)/(1-mu-fit)) * (1+(rho*T_0)/(delta*Tmax));

   V_0 = a/b;

   I_0 = (beta/delta)*V_0 * T_0;
   VM_0 = V_0*(mu)/(1-mu-fit);
   IM_0 = (beta/delta) * VM_0 * T_0;

END_main


BEGIN_ode
   dxdt_T = s+rho*T*(1-((T+I+IM+N)/Tmax)) - d*T - beta*V*T - beta*VM*T;
   dxdt_I = beta*V*T - delta*I;
   dxdt_V = (1-mu)*(1-eps1)*p*I - c*V;
   dxdt_IM = beta*VM*T - delta*IM;
   dxdt_VM = mu*(1-eps1)*p*I + (1-eps2)*fit*p*IM - c*VM;
END_ode

BEGIN_table
  table(VT) = log10(V+VM);
  table(logVM) = log10(VM);
  table(logIT) = log10(IM+I);
END_table

