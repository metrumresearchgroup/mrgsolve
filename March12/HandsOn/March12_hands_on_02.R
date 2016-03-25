
library(mrgsolve)


##' 
##' Turn the following skeleton in to a 
##' 1-compartment PK model first order 
##' absorption.  
##' 
##' Allow for bioavailability fraction < 1
##' 
##' Make sure to output the concentration
##' in the central compartment.
##' 

code <- '

$PARAM
CL=1

$MAIN

$CMT GUT

$ODE

'

mod <- mread("handson2", tempdir(),code)




