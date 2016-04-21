
library(mrgsolve)

code <- '
$PARAM CL=1, VC=80, KA=1.2 , Q=8, VP=125

$CMT GUT CENT URINE

$GLOBAL
#define CP (CENT/VC)

$ADVAN2 // 4

$MAIN
pred_CL = CL;
pred_VC = VC;
pred_KA = KA;

// pred_Q  = Q;
// pred_VP = VP;

$CAPTURE CP
'

code <- '
$PARAM CL=1, VC=80, KA=1.2
$CMT GUT CENT URINE


$GLOBAL
#define CP (CENT/VC)
$ODE

dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT- (CL/VC)*CENT;
dxdt_URINE = CP;

$CAPTURE CP

'

mod <- mread("accum", tempdir(),code) %>% Req(CP) %>% update(end=480,delta=0.1)

e1 <- ev(amt=1000)
mod %>% ev(e1) %>% mrgsim(end=96) %>% plot


t1 <- seq(0,480,96)
t2.1 <- seq(0,24,6)
t2.2 <- seq(0,2,0.25)
t3 <- seq(456,528,8)
tall <- sort(c(t1,t2.1,t2.2,t3))



mod %>% 
  ev(amt=1000,ii=24,addl=19) %>% 
  mrgsim(end=-1, add=tall) %>% 
  plot(type='b')

mod %>% 
  ev(amt=1000,ii=24,addl=19) %>% 
  carry.out(cmt,ii,addl,rate,amt) %>%
  mrgsim(end=-1, add=tall,recsort=3)  


data <- 
  mod %>% 
  ev(amt=1000,ii=24,addl=19) %>% 
  carry.out(cmt,ii,addl,rate,amt,evid,ss) %>%
  mrgsim(end=-1, add=tall,recsort=3)   %>%
  as.data.frame



e1 <- ev(amt=1000,ii=24,addl=4)
e2 <- ev(evid=3)
e3 <- ev(amt=500,ii=48,addl=4)
e <- e1 %then% e2
e <- e %then% e3

e <- e1 %then% ev(evid=4,amt=500) %then% ev(amt=500,ii=24,addl=3)

mod %>%
  ev(e) %>%
  Req(GUT,CENT) %>%
  mrgsim %>% plot


e1 <- ev(amt=1000,rate=1,cmt=2, ii=0,addl=0)
e2 <- ev(evid=2, cmt=c(-3,3),time=c(500,600))
mod %>%
  ev(e1 + e2) %>%
  Req(GUT,CENT,URINE) %>%
  mrgsim(end=2000) %>% plot






code <- '
$PARAM CL=1, VC=80, KA=1.2
$CMT GUT CENT URINE

$MAIN
_ALAG(1) = 5.1;

$GLOBAL
#define CP (CENT/VC)

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT- (CL/VC)*CENT;
dxdt_URINE = CP;

$CAPTURE CP
'

mod <- mread("acum2", tempdir(),code) %>% update(end=72,delta=0.1)

mod %>%
  ev(amt=1000) %>% 
  carry.out(evid,amt,ii,addl,rate) %>%
  mrgsim(delta=0.5) %>% as.data.frame  %>% head(n=20)







