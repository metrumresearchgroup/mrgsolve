library(dplyr)
library(mrgsolve)
library(ggplot2)


code <- '
$SET delta=1, end=120

$PARAM TVCL=5.05, TVVC=33.6, TVQ=1.32, TVVP=12.9, AGE50=0.4524, AGE=6, BW=20
inftype=0, LINF=1, MINF=24, Tlag1=0, Tlag2=2

$MAIN

double BSA = (4 * BW +7)/(BW+90);

double CL=TVCL*BSA*(AGE/(AGE+AGE50));
double VC=TVVC*BSA;
double Q=TVQ*BSA;
double VP=TVVP*BSA;

_F(1) = 1.11;

_D(1) =    inftype == 1 ? LINF : MINF;
_ALAG(1) = inftype == 1 ? Tlag1 : Tlag2;

$CMT CENT PERI

$ODE
double CP =  (CENT/VC);
dxdt_CENT = -(CL/VC)*CENT -(Q/VC)*CENT + (Q/VP)*PERI;
dxdt_PERI = (Q/VC)*CENT - (Q/VP)*PERI;

$CAPTURE CP

'

mod <- mread(code=code)


bd1 <- ev(amt=20000, inftype=1)
# setting delta to 1 gives me a one hour infusion
mod %>% ev(bd1) %>% param(AGE=72,BW=20) %>% mrgsim(delta=1,end=4) %>% plot(type='b')
# setting delta to 0.1 gives me a 0.1 hour infusion
mod %>% ev(bd1) %>% param(AGE=72,BW=20) %>% mrgsim(delta=0.1,end=4) %>% plot(type='b')



