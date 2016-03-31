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

##' A model
mod <- mrgsolve:::house() %>% Req(CP,RESP)


##' Sensitivity analysis on random values

##' log Mean
mu <- log(as.numeric(param(mod))[s(CL,VC,KA,IC50,KOUT)])
mu

##' Variance
Sigma <- diag(rep(0.1,length(mu)))

Sigma

##' Simulate log-normal parameters
set.seed(111)
pars <- exp(mvrnorm(100, mu, Sigma)) %>% as.data.frame

head(pars)



##' Simulate the response
mod %>%
  ev(amt=1000) %>%
  idata_set(pars) %>%
  mrgsim(end=48,delta=0.1) %>%
  plot


##' For fixed parameter combinations you can look at all combinations
##' like this:
pars <- expand.idata(CL=seq(0.5,1.5,0.1), IC50=seq(0.2,2,0.2), VC=seq(5,50,5))
head(pars)

##' Or code the exact combinations you want to look at.

#+
sessionInfo()














