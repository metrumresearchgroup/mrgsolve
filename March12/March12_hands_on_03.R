library(mrgsolve)


##' 
##'
##' Correct $ODE for 1 compartment model with 
##' first order absorption.
##' 
##' Add $OMEGA with 20% variability in CL
##' and 30% variability in VC and 50% variability in 
##' KA
##' 
##' What fraction of patients have concentration 
##' greater than 50 after 825 mg Q12h x 6
##' 
##' 

code <- '

$THETA 0.1 2.3 -0.6

$MAIN
double CLi = exp(THETA1 + ETA(1));
double VCi = exp(THETA2 + ETA(2));
double KAi = exp(THETA3 + ETA(3));

$CMT SC CENT

$ODE
dxdt_SC = 0;
dxdt_CENT = 0;


$TABLE
table(CP) = CENT/VCi;
'

mod <- mread("handson3",tempdir(),code)



