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
make <- function() make_worksheet("demo.R")


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

init(mod)

mod  %>% mrgsim

mod  %>% mrgsim %>% class

mod %>% mrgsim %>% plot

##' Add dosing event: 100 mg PO x1
mod %>% ev(amt=100) %>% mrgsim %>% plot

##' Dose to cmt=2, dose to "EV2"
mod %>% ev(amt=100, cmt=2) %>% mrgsim %>% plot
mod %>% ev(amt=100, cmt="EV2") %>% mrgsim %>% plot

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
e1 <- ev(amt=300, ii=24, addl=3)
e2 <- ev(amt=100, ii=8,  addl=15)
e <- e1 %then% e2

##' Simulate from `e`, plot/both
mod %>% ev(e) %>% mrgsim %>% plot(type='b')

mod %>% ev(e) %>% mrgsim(end=240,delta=0.1) %>% plot

##' Persistent update: end/delta
mod %<>% update(end=240,delta=0.1)

##' Request certain outputs
mod %>% Req(EV1,CP) %>% ev(e) %>% mrgsim %>% plot

##' Request CP, end --> 96
mod %<>% Req(CP) %>% update(end=96)


##' `data_set`
##' 

##' data:
##' 
##' * 100/300/1000 over 10H Q24H x3
data <- expand.ev(amt=c(100,300,1000), 
                  ii=24, addl=2, cmt=2) 

data %<>% mutate(rate = amt/10)

data

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

mod %>% 
  data_set(extran1) %>%
  mrgsim(end=220) %>%
  plot(CP~time|ID)


##' data: 
##' 
##' * 1000 mg doses IV over 10H
##' * CL ~ 0.5 --> 2.5
##' 
data <- expand.ev(amt=1000, ii=24, addl=100, 
                  rate = 100, cmt=2,
                  CL = seq(0.5,2.5,0.25))

##' Simulate
mod %>% 
  data_set(data) %>% 
  mrgsim(end=240) %>% 
  plot(CP~time,scales="same")


##' 
##' * data:
##' * exTheoph
##' 
data(exTheoph)
head(exTheoph)

##' Simulate from exTheoph
mod %>% data_set(exTheoph) %>% mrgsim(delta=0.1) %>% plot(type='b')

##' Filter doses, then simulate
mod %>% data_set(exTheoph,subset=evid==1) %>% mrgsim(delta=0.1) %>% plot

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
WT = 70, WTCL = 0.75

$CMT GUT CENT 

$MAIN
double CL = TVCL*pow(WT/70,WTCL);
double V  = TVVC*pow(WT/70,1);

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/V)*CENT;

$TABLE
table(CP) = CENT/V;

$CAPTURE KA
'


##' Parse, compile and load
#+ message=FALSE
mod <- mcode("spec", code)

##' Simulate  / init()
mod %>% init(GUT=100) %>% mrgsim %>% plot

##' data:
##' 
##' * 1000 mg po x1
##' * WT from 20 to 140 by 10 kg
##' 
data <- expand.ev(WT = seq(20,140,10), amt=1000)

##' Simulate / plot logCP ~ time by ID
mod %>% 
  data_set(data) %>%
  mrgsim(delta=0.1, end=240) %>% 
  plot(log(CP) ~time)

##' What happens to half-life when WTCL=1?
mod %>% 
  data_set(data) %>%
  param(WTCL = 1) %>%
  mrgsim(delta=0.1, end=240) %>% 
  plot(log(CP)~time)



##' Add KIN, KOUT, IC50, FBIO
code <- '
$PARAM TVCL = 1, TVVC = 35, KA = 1.2
WT = 70, WTCL = 0.75
KIN = 100, KOUT = 2, IC50 = 4, FBIO = 0.6

$CMT GUT CENT RESP

$MAIN
double CL = TVCL*pow(WT/70,WTCL);
double V  = TVVC*pow(WT/70,1);

RESP_0 = KIN/KOUT;

F_CENT = FBIO;

$ODE
double CP = CENT/V;
double INH = CP/(IC50+CP);

dxdt_GUT  = -KA*GUT;
dxdt_CENT =  KA*GUT - (CL/V)*CENT;
dxdt_RESP =  KIN*(1-INH) - KOUT*RESP;

$TABLE
table(CP) = CENT/V;

$CAPTURE CL
'

#+ message=FALSE
mod <- mcode("specpd", code)

##' Check initial conditions
init(mod)

##' Simulate:
##' 
##' * IV bolus 100 mg x1
##' * Look at FBIO from 0 to 1 by 0.1 / knobs
##' * Plot response ~ time grouped by FBIO
##' 
mod %>% 
  ev(amt=100, cmt=2) %>% 
  update(delta=0.1) %>%
  Req(RESP) %>%
  knobs(FBIO = seq(0,1,0.2)) %>% 
  plot()


##' Add random effects and $OMEGA
code <- '
$PARAM TVCL = 1, TVVC = 35, KA = 1.2
KIN = 100, KOUT=2, IC50 = 2

$CMT GUT CENT RESP

$OMEGA  0.1 0.5 0.9

$MAIN
double CL = TVCL*exp(ETA(1));
double V  = TVVC*exp(ETA(2));

RESP_0 = KIN/KOUT;

$ODE
double CP = CENT/V;
double INH = CP/(IC50+CP);

dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - (CL/V)*CENT;
dxdt_RESP = KIN*(1-INH) - KOUT*RESP;

$CAPTURE CL V
'

##' Compile with mcode
#+ message=FALSE
mod <- mcode("specpop", code)

##' * Simulate 50 patients at 1000 mg dose, 100 kg
##' * end --> 120, delta --> 1
data <- expand.ev(ID=1:50, amt=1000, cmt=2, rate=100)

out <- 
  mod %>% 
  data_set(data) %>%
  mrgsim(delta=1,end=120)   

plot(out)

##' `drop.re`
mod %>% 
  data_set(data) %>%
  zero.re %>%
  mrgsim(delta=1,end=120) %>%
  plot()


devtools::session_info()
