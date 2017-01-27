$PROB
# Model: `pk1cmt`
  - One-compartment PK model
      - Dual first-order absorption
      - Optional nonlinear clearance from `CENT`
  - Source: `mrgsolve` internal library
  - Date: `r Sys.Date()`
  - Version: `r packageVersion("mrgsolve")`
  

# Demo
```{r,echo=TRUE}
mod %>% 
  ev(object="e") %>% 
  mrgsim(end=288, delta=0.1) %>% 
  plot
```

$PARAM @annotated
CL   :  1  : Clearance (volume/time)
VC   : 20  : Central volume (volume)
KA1  :  1  : Absorption rate constant 1 (1/time)
KA2  :  1  : Absorption rate constant 2 (1/time)
VMAX :  0  : Maximum velocity (mass/time)
KM   :  2  : Michaelis Constant (mass/volume)

$CMT @annotated
EV1  : First extravascular compartment
CENT : Central compartment
EV2  : Second extravascular compartment

$GLOBAL
#define CP (CENT/VC)
#define CT (PERIPH/VP)
#define CLNL (VMAX/(KM+CP))

$ODE
dxdt_EV1 = -KA1*EV1;
dxdt_EV2 = -KA2*EV2;
dxdt_CENT = KA1*EV1 + KA2*EV2 - (CL+CLNL)*CP;

$CAPTURE @annotated
CP : Plasma concentration (mass/volume)

$ENV
e <- ev(amt=100, ii=24, addl=9)

cama <- function(mod,...) {
  mod %>% 
    update(...) %>%
    mrgsim(events=e,end=288,delta=0.1) 
}
  
  