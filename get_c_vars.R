##' ---
##' output: 
##'   md_document:
##'     variant: markdown_github
##' ---

#+ message=FALSE
library(dplyr)
library(magrittr)
library(mrgsolve)

x <- "double x = 2;long double a= 2;  bool foo_yak = 34 + pow(THETA1,THETA2);"

a <- mrgsolve:::get_sep_tokens(x) %>% unlist



fails <- '
$PARAM CL = 0.3, VC = 1.5, wt = 5
$CMT CENT
$MAIN
double CL_i = CL*exp(ETA(1))*wt/5;
double Vi = VC*exp(ETA(2));
$ODE
dxdt_CENT = 0;
 double setinode = pow(a,b);
$OMEGA
0.1
0.04
$TABLE
bool  IPRED = 2/3;
double b = 2;
localdouble T = 1;
$CAPTURE CL_i Vi wt
'
mod <- mcode("fails", fails,project='.', compile=FALSE)






