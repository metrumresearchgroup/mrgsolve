
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

library(dplyr)
library(pipeR)


system.time({
  foo <- lapply(1:10000, function(i) {
    mod %>% 
      Req(CENT) %>%
      obsonly %>%
      ev(amt=100,cmt=2) %>%
      mrgsim %>%
      summarise(mean=mean(CENT))
  })
})



system.time({
  foo2 <- lapply(1:10000, function(i) {
    mod %>>% 
      Req(CENT) %>>%
      obsonly %>>%
      ev(amt=100,cmt=2) %>>%
      mrgsim %>>%
      summarise(mean=mean(CENT))
  })
})



check <- function(x,what="rstats") {
  x==what
}

system.time({
  lapply(1:1000, function(i) {
    sample(letters,6,replace = TRUE) %>>%
      paste(collapse = "") %>>%
      check(.)
  })
})


system.time({
  lapply(1:1000, function(i) {
    sample(letters,6,replace = TRUE) %>%
      paste(.,collapse = "") %>%
      check(.)
  })
})



