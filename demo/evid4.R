##' ---
##' output: 
##'   md_document:
##'     variant: markdown_github
##' ---

library(knitr)
opts_chunk$set(fig.path="img/evid4-")

library(mrgsolve)
options(mrgsolve_mread_quiet=TRUE)
#+
e1 <- ev(time=0,evid=1,amt=1000, rate=200)
e2 <- ev(time=10,evid=4,amt=1000,rate=200)
e <- e1 + e2
e


##' # Fine, but we expect compartments to start at zero
code <- '
$PARAM KE=0.3
$CMT CMT
$ODE
dxdt_CMT = -KE*CMT;
'

mod <- mcode("reset",code)

out <- 
  mod %>% 
  ev(e) %>%
  mrgsim(delta=0.1)

out %>% plot



##' # This actually works
##' I said it wouldn't work, but it does.  It's sort of an accident ... but it happens correctly due to the 
##' way `init_call` is coded up.
##' 
##' 
out <- 
  mod %>% 
  init(CMT = 400) %>%
  ev(e) %>%
  mrgsim(delta=0.1)

out %>% plot




##' # Actually, this works too
idata <- expand.idata(CMT_0 = seq(0,600,100))
out <- 
  mod %>% 
  idata_set(idata) %>%
  init(CMT = 400) %>%
  ev(e) %>%
  mrgsim(delta=0.1)

out %>% plot



##' # Fine .... the initial comes from `$MAIN`
code <- '
$PARAM KE=0.1,BASE=200

$CMT CMT

$MAIN
CMT_0 = BASE;

$ODE
dxdt_CMT = -KE*CMT;
'

mod <- mcode("base",code)

out <- 
  mod %>% 
  ev(e) %>%
  mrgsim(delta=0.1, end=72)

out %>% plot


