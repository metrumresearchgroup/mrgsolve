##' ---
##' output:
##'   md_document:
##'     variant: markdown_github
##' ---


##' # Sensitivity Analysis

library(mrgsolve)
library(MASS)

#+ echo=FALSE
library(knitr)
opts_chunk$set(comment='.',fig.path="img/sensi-")

##' A model PK/PD model.  
##'  
##' * Only interested in `CP` and `RESP` (the response from indirect response model) for now.
##' 
mod <- mrgsolve:::house() %>% Req(CP,RESP)


##' Sensitivity analysis on random values
##' 
##' * Get the current parameter values for `CL`, `VC`, `KA`, `IC50`, and `KOUT`
##' * Convert to numeric vector (just a regular `R` object`)
##' * Log-transform
mu <- log(as.numeric(param(mod))[s(CL,VC,KA,IC50,KOUT)])
mu

##' 
##' Variance; this gives the same CV for each parameter.  But feel free 
##' to customize this as you like, for each parameter.
##' 
Sigma <- diag(rep(0.1,length(mu)))

Sigma

##' Simulate log-normal parameters
set.seed(111)
pars <- exp(mvrnorm(100, mu, Sigma)) %>% as.data.frame

head(pars)



##' 
##' Simulate the response
##' 
##' * We pass the parameters in as `idata`
##' 
mod %>%
  ev(amt=1000) %>%
  idata_set(pars) %>%
  mrgsim(end=72,delta=0.1) %>%
  plot


##' For fixed parameter combinations you can look at all combinations
##' like this:
pars <- expand.idata(CL=seq(0.5,1.5,0.1), IC50=seq(0.2,2,0.2), VC=seq(5,50,5))
head(pars)
##' You can also code the exact combinations you want to look at.

#+
sessionInfo()














