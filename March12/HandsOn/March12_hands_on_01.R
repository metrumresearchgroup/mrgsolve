
library(mrgsolve)

mod <- mrgsolve:::house()

mod

param(mod)


##' 
##' Update clearance to 1.5 L/hr 
##' 


##' 
##' Simulate 750 mg Q12h for 10 days
##' 
##' Use an events object
##' 




##' 
##' Check out main mrgsolve help
##' file (?mrgsolve) and find 
##' the topic for example input data sets
##' 
##' Load the theophylline data set
##' 
##' Simulate and plot the data set
##' 



##' 
##' Remove observation records from exThepoh
##' theo <- exTheoph %>% filter(evid==1)
##' Modify the data set to be a 4 hour infusion
##' every 8 hours for 3 days
##' 


##' 
##' Load the model called `poppk` 
##' in the archive directory
##' 
##' Find the name of the parameter 
##' that controls bioavailability
##' 
##' Invent a dose/regimen and 
##' run some simulations at different
##' estimates of bioavailability
##' 


