##' ---
##' title: "ISoP StudyGroup mrgsolve demo"
##' date: "13 May 2016"
##' author: "Kyle Baron"
##' output: 
##'   html_document: 
##'     theme: united
##' ---

##' Load:
##' mrgsolve,modmrg, magrittr,dplyr
#+ message=FALSE
library(mrgsolve) #
library(modmrg)  #
library(magrittr)  #
library(dplyr) #

##' Source:
##' functions.R, data.R
#+ message=FALSE
source("functions.R")  #

##' Load a model from `modmrg`
##' 1-compartment PK model
mod <- pk1cmt()






##' Basics: 
##' 
##' * show
##' * param/init
##' * Update
##' * Simulate / plot
##' * Check class
##' 

mod

param(mod)

l <- list(CL = 222)
mod %>% param(CL = 333) %>% param
mod %>% param(l) %>% param

init(mod)

mod %>% mrgsim

see(mod)

mod %>% mrgsim %>% plot

##' Add dosing event: 100 mg PO x1
mod %>% ev(amt=100) %>% mrgsim() %>% plot


##' Dose to cmt=2, dose to "EV2"
mod %>% ev(amt=100, cmt=2) %>% mrgsim() %>% plot
mod %>% ev(amt=100, cmt="EV2") %>% mrgsim() %>% plot

##' Items you can have in `ev`
##' 
##' * time
##' * amt
##' * rate
##' * cmt
##' * evid 
##' * ii / addl
##' * ss
##' 

##' Events `ev`
##' 
##' * 300 mg PO Q24H x 4, then 100 mg Q8H x14
e1 <-  ev(amt=300, ii=24, addl=3)
e2 <- ev(amt=100, ii=8, addl=13)
e <- e1 %then% e2


##' Simulate from `e`, plot/both
mod %>% ev(e) %>% mrgsim(end=240, delta=0.1) %>% plot

##' Persistent update: end/delta
mod %<>% update(end=240, delta=0.1)

##' Request certain outputs
mod %>% ev(e) %>% Req(EV1, CP) %>% mrgsim() %>% plot

##' Request CP, end --> 96
mod %<>% update(end=96) %>% Req(CP)

##' `data_set`
##' 

##' data:
##' 
##' * 100/300/1000 over 10H Q24H x3
data <- expand.ev(ID=1,amt=c(100, 300, 1000),cmt=2,
                  ii=24, addl=100)
data %<>% mutate(rate=amt/10)                


##' Simulate
mod %>% data_set(data) %>% mrgsim %>% plot

##' Filter and simulate
mod %>% data_set(data, ID > 1) %>% mrgsim %>% plot

##' data: extran1
##' 
##' plot: CP~time|ID
##' 
data(extran1)

extran1

mod %>% data_set(extran1) %>% mrgsim(end=240) %>% plot

##' data: 
##' 
##' * 1000 mg doses IV over 10H
##' * CL ~ 0.5 --> 2.5
##' 

data <- expand.ev(ID=1,amt=c(1000),cmt=2,
                  ii=24, addl=100,
                  CL = seq(0.5,2.5,0.25))
data %<>% mutate(rate = amt/10)


##' Simulate
mod %>% data_set(data) %>% mrgsim %>% plot

##' 
##' * data:
##' * exTheoph
##' 
data(exTheoph)


##' Simulate from exTheoph
mod %>% data_set(exTheoph) %>% mrgsim %>% plot(type='b')

mod %>% 
  data_set(exTheoph) %>% 
  obsaug %>% 
  carry.out(a.u.g,WT,amt,evid,addl) %>%
  mrgsim %>% 
  plot(type='b')


##' Filter doses, then simulate

##' Switch back to demo.R?
##' 
##' Model specification
##' 
##' * Parameters and compartments
##' * Set initial conditions
##' * Covariates and random effects
##' * Bioavailability / Lag time / Infusion D/R
##' * ODEs
##' * Output variables
##' 

##' 
##' * 1-cmt model, first-order absorption
##' * Parameters: TVCL, TVVC, KA, WT, WTCL
##' * Compartments: GUT CENT
##' * Covariate model: CL~WT, VC~WT
##' * Output: CP = CENT/V, KA
code <- '
$PARAM TVCL = 1, TVVC = 35, KA = 1.2
WT = 70 , WTCL  = 0.75

$CMT GUT CENT

$MAIN
double CL = TVCL*pow(WT/70,WTCL);
double V =  TVVC*pow(WT/70,1);

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/V)*CENT;

$TABLE
table(CP) = CENT/V;

$CAPTURE CL 
'

##' Parse, compile and load
#+ message=FALSE
mod <- mcode("simplepk", code)


##' Simulate  / init()
mod %>% init(GUT =1000) %>% mrgsim %>% plot

##' data:
##' 
##' * 1000 mg po x1
##' * WT from 20 to 140 by 10 kg
##' 

data <- expand.ev(ID=1,amt=c(1000),cmt=1,
                  WT = seq(20,140,10))
                  
##' Simulate / plot logCP ~ time by ID
mod %>% 
  data_set(data) %>% 
  mrgsim(end=240) %>% 
  plot(log(CP)~.)


##' What happens to half-life when WTCL=1?
mod %>% 
  param(WTCL = 1) %>% 
  data_set(data) %>% 
  mrgsim(end=240) %>% 
  plot(log(CP)~.)


##' Add KIN, KOUT, IC50, FBIO
code <- '
$PARAM TVCL = 1, TVVC = 35, KA = 1.2
WT = 70 , WTCL  = 0.75
KIN  = 100, KOUT = 2, IC50 = 0.5
FBIO = 0.5

$CMT GUT CENT RESP

$MAIN
double CL = TVCL*pow(WT/70,WTCL);
double V =  TVVC*pow(WT/70,1);

F_CENT = FBIO;

RESP_0 = KIN/KOUT;


$ODE
double CP = CENT/V;
double INH = CP/(IC50+CP);

dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/V)*CENT;
dxdt_RESP = KIN*(1-INH) - KOUT*RESP;


$TABLE
table(CP) = CENT/V;

$CAPTURE CL 
'
#+ message=FALSE
mod <- mcode("pkpd", code)
##' Check initial conditions

init(mod)
mod %>% param(KIN = 200) %>% init

##' Simulate:
##' 
##' * IV bolus 100 mg x1
##' * Look at FBIO from 0 to 1 by 0.1 / knobs
##' * Plot response ~ time grouped by FBIO
##' 
mod %>%
  ev(amt=100, cmt=2) %>%
  knobs(FBIO = seq(0,1,0.1),
        delta=0.1) %>% 
  plot(RESP~.)


##' Add random effects and $OMEGA
code <- '
$PARAM TVCL = 1, TVVC = 35, KA = 1.2
WT = 70 , WTCL  = 0.75
KIN  = 100, KOUT = 2, IC50 = 0.5
FBIO = 0.5, FLAG = 0

$CMT GUT CENT RESP

$OMEGA cor=TRUE
labels = s(ECL,EV)
0.3 0.67 0.2


$MAIN
double CL = TVCL*exp(ECL);
double V =  TVVC*exp(EV);

if(FLAG > 0) V = V*2;

F_CENT = FBIO;

RESP_0 = KIN/KOUT;


$ODE
double CP = CENT/V;
double INH = CP/(IC50+CP);

dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/V)*CENT;
dxdt_RESP = KIN*(1-INH) - KOUT*RESP;


$TABLE
table(CP) = CENT/V;

$CAPTURE ECL EV
'
##' Compile with mcode
#+ message=FALSE
mod <- mcode("pop", code)


##' * Simulate 50 patients at 1000 mg dose, 100 kg
##' * end --> 120, delta --> 1
data <- expand.ev(amt=100, ID=1:50, WT = 100)

out <- 
  mod %>%
  data_set(data) %>%
  mrgsim(end=120,delta=1)

plot(out)

##' `drop.re`
out <- 
  mod %>%
  drop.re %>% 
  data_set(data) %>%
  mrgsim(end=120,delta=1)

plot(out)
