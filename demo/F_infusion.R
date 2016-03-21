##' ---
##' output: 
##'   md_document:
##'     variant: markdown_github
##' ---
#+ message=FALSE
library(mrgsolve, lib.loc="~/Rlibs/0.5.11")
library(dplyr)
library(magrittr)

#+ echo=FALSE
library(knitr)
opts_chunk$set(fig.path="img/F_infusion-",comment='.')


##' # Infusions with varying `F`

##' A model with infusion
code <- '
$SET delta=0.1, end=24

$GLOBAL
// In next version, we wont have to do this explicitly to get the nice aliases
#define F_CENT _F(1)
#define R_CENT _R(1)
#define D_CENT _D(1)

$PARAM CL = 1, VC = 5, F1 = 1, D1 = 5, R1 = 20

$CMT CENT

$MAIN
F_CENT = F1;
D_CENT = D1;
R_CENT = R1;

$ODE
dxdt_CENT  = -(CL/VC)*CENT;

'

##'
#+ message=FALSE
mod <- mread("infusion", tempdir(), code)

##' Run with `rate=-1`: `F` and `amt` determine duration.  Only `F1==1` gets the 5 hour infusion.
data <- expand.ev(amt=100,F1=seq(0.1,1,0.1),rate=-1)
#+
mod %>%
  data_set(data) %>%
  mrgsim %>% 
  plot

##' Run with `rate=-2`: duration is set by `D`.  Now, `D` is enforced ... everyone gets 5 hour infusion
data <- expand.ev(amt=100,F1=seq(0.1,1,0.1),rate=-2)
#+
mod %>%
  data_set(data) %>%
  mrgsim %>% 
  plot





##' We can't do something like this ...
data <- expand.ev(amt=100,F1=seq(0.1,1,0.1)) %>% mutate(rate=amt/5)
#+
head(data)
##' ... and expect to get a 5 hour infusion

mod %>%
  data_set(data) %>%
  mrgsim %>% 
  plot


##' But we **could** do this to ensure duration is 5
data <- expand.ev(amt=100,F1=seq(0.1,1,0.1),D1 = 5)
data %<>% mutate(amt=amt*F1, rate = amt/D1) %>% select(-F1,-D1) %>% mutate(F1=1)

#+
data
#+

mod %>%
  data_set(data) %>%
  mrgsim %>% 
  plot

##' 
##' The key is to make all of the adjustments __either__ (1) in the control stream __or__ (2) in the data.  
##' 


##' `sessionInfo`
#+echo=FALSE
sessionInfo()


