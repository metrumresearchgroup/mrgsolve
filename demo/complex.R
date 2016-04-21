
code <- '
$SET delta=0.1, end=120

$PARAM Ffast = 0.33, Fslow1 = 0.33, Fslow2 = 1-0.33-0.33, inftype=0
KA=1.5, CL=1, VC=30, Dslow1 = 10, Dslow2 = 20, Tlag1 = 2, Tlag2 = 4

$MAIN

_F(1) = Ffast;

_F(2) = 1;

if(inftype !=0) {
  _F(2) = 1 - Ffast - ((inftype==1) ? Fslow2 : Fslow1);
}

_D(2) =    inftype == 1 ? Dslow1 : Dslow2;
_ALAG(2) = inftype == 1 ? Tlag1 : Tlag2;

$CMT DEPOT CENT

$DES

dxdt_DEPOT = -KA*DEPOT;
dxdt_CENT = KA*DEPOT - (CL/VC)*CENT;

'

mod <- mread("complex", tempdir(), code)

# Just the first-order
d1 <- ev(amt=100)
# Just infusion 1
d2 <- ev(amt=100, inftype=1, cmt=2, rate=-2)
# First-order and infusion 1
d12 <- d1+d2
# Just infusion 2
d3 <- ev(amt=100, inftype=2, cmt=2, rate=-2)
# all 3
d4 <- d1 + d2 + d3
# Just the two infusions
d5 <- d2 + d3


mod %>% ev(d1) %>% mrgsim(capture="foo.txt") %>% plot

mod %>% ev(d2) %>% mrgsim(capture="foo.txt") %>% plot
mod %>% ev(d3) %>% mrgsim %>% plot
mod %>% ev(d4) %>% mrgsim %>% plot

mod %>% ev(d5) %>% mrgsim %>% plot


