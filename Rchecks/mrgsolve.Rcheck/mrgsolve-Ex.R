pkgname <- "mrgsolve"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('mrgsolve')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Req")
### * Req

flush(stderr()); flush(stdout())

### Name: Req
### Title: Set the 'Request' argument for 'mrgsim'.
### Aliases: Req Req,mrgmod-method req req,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()

mod %>% Req(CP,RESP) %>% ev(amt=1000) %>%  mrgsim




cleanEx()
nameEx("as.matrix")
### * as.matrix

flush(stderr()); flush(stdout())

### Name: as.matrix.list
### Title: Coerce a list to a matrix
### Aliases: as.matrix.list

### ** Examples

x <- list(a=1, b=2, c=3)
as.matrix(x,nrow=3)



cleanEx()
nameEx("as_bmat")
### * as_bmat

flush(stderr()); flush(stdout())

### Name: as_bmat
### Title: Coerce R objects to block or diagonal matrices.
### Aliases: as_bmat as_bmat,ANY-method as_bmat,data.frame-method
###   as_bmat,list-method as_bmat,numeric-method as_dmat as_dmat,ANY-method
###   as_dmat,data.frame-method as_dmat,list-method as_dmat,numeric-method

### ** Examples


df <- data.frame(OMEGA1.1 = c(1,2),
                 OMEGA2.1 = c(11,22),
                 OMEGA2.2 = c(3,4),
                 SIGMA1.1 = 1,
                 FOO=-1)

as_bmat(df, "OMEGA")
as_dmat(df,"SIGMA")
as_dmat(df[1,],"OMEGA")




cleanEx()
nameEx("blocks")
### * blocks

flush(stderr()); flush(stdout())

### Name: blocks
### Title: Return the code blocks from a model specification file.
### Aliases: blocks blocks,character-method blocks,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()
mod %>% blocks
mod %>% blocks(PARAM,TABLE)




cleanEx()
nameEx("bmat")
### * bmat

flush(stderr()); flush(stdout())

### Name: bmat
### Title: Create matrices from vector input
### Aliases: BLOCK bmat cmat dmat

### ** Examples


dmat(1,2,3)/10

bmat(0.5,0.01,0.2)

cmat(0.5, 0.87,0.2)





cleanEx()
nameEx("chain")
### * chain

flush(stderr()); flush(stdout())

### Name: chain
### Title: Functions for chaining commands together.
### Aliases: chain

### ** Examples


mod <- mrgsolve:::house()

data(exidata)
data(exTheoph)

out <- mod %>% data_set(exTheoph) %>% mrgsim()
out <- mod %>% carry.out(evid) %>% ev(amt=100, cmt=1) %>% mrgsim()
out <- mod %>% Req(CP,RESP) %>% mrgsim()




cleanEx()
nameEx("cmt")
### * cmt

flush(stderr()); flush(stdout())

### Name: cmt
### Title: Get the names of model compartments.
### Aliases: cmt cmt,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()

cmt(mod)





cleanEx()
nameEx("cmtn")
### * cmtn

flush(stderr()); flush(stdout())

### Name: cmtn
### Title: Get the compartment number from a compartment name.
### Aliases: cmtn cmtn,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()
mod %>% cmtn("CENT")



cleanEx()
nameEx("cvec")
### * cvec

flush(stderr()); flush(stdout())

### Name: cvec
### Title: Create create character vectors.
### Aliases: ch cvec cvec,character-method s

### ** Examples


cvec("A,B,C")
ch(A,B,C)
s(A,B,C)




cleanEx()
nameEx("data_set")
### * data_set

flush(stderr()); flush(stdout())

### Name: data_set
### Title: Set the 'data' argument for 'mrgsim'.
### Aliases: data_set data_set,mrgmod,ANY-method
###   data_set,mrgmod,data.frame-method

### ** Examples


data <- expand.ev(ID=1:3, amt=c(10,20))

data <- expand.ev(amt=c(10,20), rate=c(1,2))





cleanEx()
nameEx("dllname")
### * dllname

flush(stderr()); flush(stdout())

### Name: dllname
### Title: Return the model name.
### Aliases: dllname dllname,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()
dllname(mod)




cleanEx()
nameEx("events")
### * events

flush(stderr()); flush(stdout())

### Name: events
### Title: Get model events
### Aliases: as.data.frame,ev-method as.ev as.ev,data.frame-method
###   as.matrix,ev-method ev ev,ev-method ev,missing-method
###   ev,mrgmod-method events events,mrgmod-method events,mrgsims-method
###   show,ev-method
### Keywords: events

### ** Examples

mod <- mrgsolve:::house()
mod <- mod %>% ev(amt=1000, time=0, cmt=1)
events(mod)

loading <- ev(time=0, cmt=1, amt=1000)
maint <- ev(time=12, cmt=1, amt=500, ii=12, addl=10)
loading + maint


ev(ID=1:10, cmt=1, time=0, amt=100)





cleanEx()
nameEx("exdatasets")
### * exdatasets

flush(stderr()); flush(stdout())

### Name: exdatasets
### Title: Example input data sets
### Aliases: exBoot exTheoph exdatasets exidata extran1 extran2 extran3
### Keywords: datasets

### ** Examples


mod <- mrgsolve:::house() %>% update(end=240) %>% Req(CP)

## Full data set
data(exTheoph)
out <- mod %>% data_set(exTheoph) %>% mrgsim
out
plot(out)
## Condensed: mrgsolve fills in the observations
data(extran1)
out <- mod %>% data_set(extran1) %>% mrgsim
out
plot(out)
## Add a parameter to the data set
stopifnot(require(dplyr))
data <- extran1 %>% distinct(ID) %>% select(ID) %>%
  mutate(CL=exp(log(1.5) + rnorm(nrow(.), 0,sqrt(0.1)))) %>%
  left_join(extran1,.)
data
out <- mod %>% data_set(data) %>% carry.out(CL) %>%  mrgsim
out
plot(out)
## idata
data(exidata)
out <- mod %>% idata_set(exidata) %>% ev(amt=100,ii=24,addl=10) %>% mrgsim
plot(out, CP~time|ID)









cleanEx()
nameEx("expand.idata")
### * expand.idata

flush(stderr()); flush(stdout())

### Name: expand.idata
### Title: Create data sets.
### Aliases: expand.ev expand.idata

### ** Examples

idata <- expand.idata(CL=c(1,2,3), VC=c(10,20,30))

doses <- expand.ev(amt=c(300,100), ii=c(12,24), cmt=1)




cleanEx()
nameEx("init")
### * init

flush(stderr()); flush(stdout())

### Name: as.init
### Title: Get and set model initial conditions.
### Aliases: as.init as.init,NULL-method as.init,cmt_list-method
###   as.init,list-method as.init,missing-method as.init,numeric-method
###   callinit init init,ANY-method init,list-method init,missing-method
###   init,mrgmod-method init,mrgsims-method show,cmt_list-method

### ** Examples

## example("init")
mod <- mrgsolve:::house()

init(mod)
init(mod, .pat="^C") ## may be useful for large models

class(init(mod))

init(mod)$CENT

as.list(init(mod))
as.data.frame(init(mod))



cleanEx()
nameEx("knobs")
### * knobs

flush(stderr()); flush(stdout())

### Name: knobs
### Title: Run sensitivity analysis on model settings
### Aliases: as.data.frame,batch_mrgsims-method
###   as.matrix,batch_mrgsims-method batch batch,batch_mrgsims-method knobs
###   knobs,batch_mrgsims,ANY-method knobs,mrgmod,batch_mrgsims-method
###   knobs,mrgmod,missing-method moving moving,batch_mrgsims-method
###   show,batch_mrgsims-method

### ** Examples

## example("knobs")

mod <- mrgsolve:::house(end=72)

events <- ev(amt=1000, cmt=1, addl=3, ii=12)

out <- mod %>% ev(events) %>% knobs(CL=c(1,2,3))
plot(out)

out
moving(out)
batch(out)


out <- mod %>% ev(events) %>% knobs(CL=c(1,2,3), VC=c(5,20,50))
plot(out)
plot(out,CP~.)
plot(out, CP~time|VC, groups=CL, lty=2)

out <- knobs(mod, amt=c(100,300,500), cmt=1,time=0)
plot(out)

out <- mod %>% knobs(amt=c(100,300), CL=c(1,3),VC=c(5,20), cmt=1, time=0)
plot(out)
plot(out, CP~.)
plot(out, CP~time|CL*VC, groups=Amt)

out <- knobs(mod, CL=c(1,2,3), drop="all")
out

out <- knobs(mod, CL=c(1,2,3), drop="none")
out



cleanEx()
nameEx("modMATRIX")
### * modMATRIX

flush(stderr()); flush(stdout())

### Name: modMATRIX
### Title: Create a matrix.
### Aliases: modMATRIX

### ** Examples

modMATRIX("1 2.2 333")
modMATRIX("1 1.1 2.2", block=TRUE)
modMATRIX("23 234 234 5234", use=FALSE)

ans <- modMATRIX("1.1 0.657 2.2", correlation=TRUE, block=TRUE)
ans
cov2cor(ans)




cleanEx()
nameEx("model")
### * model

flush(stderr()); flush(stdout())

### Name: model
### Title: Return the model name.
### Aliases: model model,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()
model(mod)



cleanEx()
nameEx("modelspec")
### * modelspec

flush(stderr()); flush(stdout())

### Name: modelspec
### Title: Model Specification File
### Aliases: modelspec

### ** Examples


code <- '

$PROB 1-cmt model with first order absorption

$SET delta =0.1, end=120, verbose=TRUE
preclean=TRUE

$OMEGA block=TRUE
0.1
0.001 0.3

$OMEGA corr=TRUE
0.1
0.67 0.2

$SIGMA
1 0.1

$PARAM
CL=1, VC=10, KA=0.1

$INIT GUT=0
CENT=1

$GLOBAL
bool cool=true;
#define KE (CL/VC)

$ODE
double CP = CENT/VC;

dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - KE*CENT;

$TABLE

table(ke) = CL/VC;

capture(CP);

'



mod <- mread(code=code, project=tempdir())

smat(mod)
omat(mod)
as.matrix(omat(mod))

see(mod)


code <- '
$PARAM CL = 1.5, VC=35, KA=1.2

$CMT DEPOT CENT

$ADVAN2

$MAIN
pred_CL = CL;
pred_VC = VC;
pred_KA = KA;
'





mod <- mread(code=code, "ADVAN2", tempdir())

mod %>%
  ev(amt=100,ii=24, addl=9) %>%
  mrgsim(delta=0.1,end=240) %>%
  plot





cleanEx()
nameEx("mread")
### * mread

flush(stderr()); flush(stdout())

### Name: mread
### Title: Read a model specification file
### Aliases: mread

### ** Examples



code <- '
$PARAM CL = 1, VC = 5
$CMT CENT
$ODE dxdt_CENT = -(CL/VC)*CENT;
'

mod <- mread(code=code)

mod

mod %>% init(CENT=1000) %>% mrgsim %>% plot




cleanEx()
nameEx("mrgsim")
### * mrgsim

flush(stderr()); flush(stdout())

### Name: mrgsim
### Title: Simulate from a model object.
### Aliases: mrgsim mrgsim,mrgmod-method mrgsim,mrgsims-method

### ** Examples

## example("mrgsim")

mod <- mrgsolve:::house() %>%  ev(amt=1000, cmt=1)
out <- mrgsim(mod)
plot(out)

out <- mrgsim(mod, end=22)
out


data(exTheoph)

out <- mrgsim(mod, data=exTheoph)
out

out <- mrgsim(mod, data=exTheoph, obsonly=TRUE)
out

out <- mrgsim(mod, data=exTheoph, obsaug=TRUE, carry.out="a.u.g")
out

out <- mrgsim(mod, req="CENT")
out

out <- mrgsim(mod, Req="CP,RESP")
out






cleanEx()
nameEx("mrgsims")
### * mrgsims

flush(stderr()); flush(stdout())

### Name: mrgsims
### Title: Methods for working with 'mrgsims' objects.
### Aliases: $,mrgsims-method as.data.frame,mrgsims-method
###   as.matrix,mrgsims-method as.tbl.mrgsims dim,mrgsims-method
###   do_.mrgsims filter_.mrgsims group_by_.mrgsims head,mrgsims-method
###   mrgsims mutate_.mrgsims names,mrgsims-method select_.mrgsims
###   show,mrgsims-method slice_.mrgsims subset,mrgsims-method
###   summarise.each summarise_.mrgsims summary,mrgsims-method
###   tail,mrgsims-method

### ** Examples


## example("mrgsims")

mod <- mrgsolve:::house() %>% init(GUT=100)

out <- mrgsim(mod)
class(out)

out
head(out)
tail(out)

mod(out)

dim(out)
names(out)

mat <- as.matrix(out)
df <- as.data.frame(out)

df <- subset(out, time < 12) ## a data frame
out$CP

plot(out)
plot(out, CP~.)
plot(out, CP+RESP~time, scales="same", xlab="Time", main="Model sims")

out <- label(out, DOSE=100)
head(out)



cleanEx()
nameEx("mrgsolve_example")
### * mrgsolve_example

flush(stderr()); flush(stdout())

### Name: mrgsolve_example
### Title: Extract example model from system library
### Aliases: mrgsolve_example

### ** Examples

## example("mrgsolve_example", package="mrgsolve")

mrgsolve_example("pkpdExample", project=getwd())

mod <- mread("pkpdExample", project=getwd()) %>% ev(amt=1000, time=0, cmt=1)

see(mod)

out <- mod %>% mrgsim(end=48,delta=0.1)

out

plot(out)

out <- mod %>%
  ev(amt=1000, ii=24, cmt=1, addl=10)  %>%
  mrgsim(end=300)

plot(out)
plot(out, CP~time)




cleanEx()
nameEx("mrgsolve_package")
### * mrgsolve_package

flush(stderr()); flush(stdout())

### Name: mrgsolve
### Title: mrgsolve
### Aliases: mrgsolve mrgsolve-package

### ** Examples


## example("mrgsolve")

mod <- mrgsolve:::house(delta=0.1)  %>% param(CL=0.5)

events <-  ev(amt=1000, cmt=1, addl=5, ii=24)

mod
cfile(mod)
see(mod)
events
stime(mod)

param(mod)
init(mod)

out <- mod %>% ev(events) %>% mrgsim(end=168)
out <- label(out, TRT=1)

out
head(out)
tail(out)
dim(out)

mod(out)
param(out)

plot(out, GUT+CP~.)

sims <- as.data.frame(out)

t72 <- subset(sims, time==72)
str(t72)



idata <- data.frame(ID=c(1,2,3), CL=c(0.5,1,2),VC=12)
out <- mod %>% ev(events) %>% mrgsim(end=168, idata=idata, req="")
plot(out)

out <- mod %>% ev(events) %>% mrgsim(carry.out="amt,evid,cmt,CL")
head(out)


out <- mod %>% ev() %>% knobs(CL=c(0.5, 1,2), amt=c(100,300,1000), cmt=1,end=48)
plot(out, CP~., scales="same")
plot(out, RESP+CP~time|CL, groups=Amt)


ev1 <- ev(amt=500, cmt=2,rate=10)
ev2 <- ev(amt=100, cmt=1, time=54, ii=8, addl=10)
events <- ev1+ev2
events

out <- mod %>% ev(ev1+ev2) %>% mrgsim(end=180, req="")
plot(out)




## Full NMTRAN data set
data(exTheoph)
head(exTheoph)

mod <- mrgsolve:::house(delta=0.1)

out <- mod %>% data_set(exTheoph) %>% mrgsim

plot(out,CP~time|factor(ID),type='b', scales="same")


## "Condensed" data set
data(extran1)
extran1

out <- mod %>% data_set(extran1) %>% mrgsim(end=200)

plot(out,CP~time|factor(ID))


## idata
data(exidata)
exidata

out <- mod %>% ev(amt=1000, cmt=1) %>% idata_set(exidata) %>%  mrgsim(end=72)

plot(out, CP~., as="log10")



code <- '
$PARAM CL=1, VC=10, KA=1.1
$INIT GUT=0, CENT=0
$SET end=48, delta=0.25

$MAIN
double CLi = CL*exp(ETA(1));
double VCi = VC*exp(ETA(2));
double ke = CLi/VCi;

$OMEGA corr=TRUE
0.04 0.6 0.09

$ODE
dxdt_GUT = -KA*GUT;
dxdt_CENT = KA*GUT - ke*CENT;

$TABLE
table(CP) = CENT/VC;

'

mod <- mread(code=code) %>% ev(amt=1000, cmt=1, addl=2, ii=8)

out <- mod %>% mrgsim

out

plot(out)







cleanEx()
nameEx("neq")
### * neq

flush(stderr()); flush(stdout())

### Name: neq
### Title: Return the number of compartments / equations.
### Aliases: neq neq,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()
neq(mod)




cleanEx()
nameEx("numericlist")
### * numericlist

flush(stderr()); flush(stdout())

### Name: numericlist
### Title: Methods for numericlist
### Aliases: $,numericlist-method [,numericlist-method
###   as.data.frame,numericlist-method as.list,numericlist-method
###   as.numeric,numericlist-method length,numericlist-method
###   names,numericlist-method numericlist

### ** Examples

## Not run: 
##D   mod <- mrgmod(...)
##D   as.list(param(mod))
##D   as.numeric(init(mod))
## End(Not run)



cleanEx()
nameEx("omega")
### * omega

flush(stderr()); flush(stdout())

### Name: omega
### Title: Manipulate 'OMEGA' matrices.
### Aliases: OMEGA omat omat,list-method omat,matrix-method
###   omat,missing-method omat,mrgmod-method omat,mrgsims-method
###   omat,omegalist-method omega

### ** Examples

## example("omega")
mat1 <- matrix(1)
mat2 <- diag(c(1,2,3))
mat3 <- matrix(c(0.1, 0.002, 0.002, 0.5), 2,2)
mat4 <- dmat(0.1, 0.2, 0.3, 0.4)

omat(mat1)
omat(mat1, mat2, mat3)
omat(A=mat1, B=mat2, C=mat3)

mod <- mrgsolve:::house() %>% omat(mat4)

omat(mod)
omat(mod, make=TRUE)


## Not run: 
##D 
##D $OMEGA
##D 1 2 3
##D 
##D $OMEGA block=TRUE
##D 1 0.1 2
##D 
##D $OMEGA cor=TRUE
##D prefix="ETA_"
##D labels=s(CL,VC,KA)
##D 0.1
##D 0.67 0.2
##D 0 0 0.3
##D 
## End(Not run)



cleanEx()
nameEx("param")
### * param

flush(stderr()); flush(stdout())

### Name: param
### Title: Get and set model parameters
### Aliases: allparam as.param as.param,list-method as.param,missing-method
###   as.param,numeric-method as.param,parameter_list-method param
###   param,ANY-method param,list-method param,missing-method
###   param,mrgmod-method param,mrgsims-method show,parameter_list-method
### Keywords: param

### ** Examples

## example("param")
mod <- mrgsolve:::house()

param(mod)
param(mod, .pat="^(C|F)") ## may be useful when large number of parameters

class(param(mod))

param(mod)$KA

as.list(param(mod))
as.data.frame(param(mod))




cleanEx()
nameEx("pars")
### * pars

flush(stderr()); flush(stdout())

### Name: pars
### Title: Return the names of model parameters.
### Aliases: pars pars,mrgmod-method

### ** Examples

mod <- mrgsolve:::house()
pars(mod)




cleanEx()
nameEx("plot_mrgsims")
### * plot_mrgsims

flush(stderr()); flush(stdout())

### Name: plot_mrgsims
### Title: Generate a quick plot of simulated data.
### Aliases: plot,mrgsims,formula-method plot,mrgsims,missing-method
###   plot_mrgsims

### ** Examples


mod <- mrgsolve:::house(end=48, delta=0.2) %>% init(GUT=1000)

out <- mrgsim(mod)

plot(out)

plot(out, subset=time <=24)

plot(out, GUT+CP~.)

plot(out, CP+RESP~time, col="black", scales="same", lty=2)




cleanEx()
nameEx("project")
### * project

flush(stderr()); flush(stdout())

### Name: project
### Title: Return the name of the project directory.
### Aliases: project project,mrgmod-method project,mrgsims-method
###   project,packmod-method

### ** Examples

mod <- mrgsolve:::house()
project(mod)




cleanEx()
nameEx("reserved")
### * reserved

flush(stderr()); flush(stdout())

### Name: reserved
### Title: Reserved words in 'mrgsolve'.
### Aliases: reserved

### ** Examples

mrgsolve:::reserved()




cleanEx()
nameEx("sigma")
### * sigma

flush(stderr()); flush(stdout())

### Name: sigma
### Title: Manipulate 'SIGMA' matrices.
### Aliases: SIGMA sigma smat smat,list-method smat,matrix-method
###   smat,missing-method smat,mrgmod-method smat,mrgsims-method
###   smat,sigmalist-method

### ** Examples

## example("sigma")
mat1 <- matrix(1)
mat2 <- diag(c(1,2))
mat3 <- matrix(c(0.1, 0.002, 0.002, 0.5), 2,2)
mat4 <- dmat(0.1, 0.2, 0.3, 0.4)

smat(mat1)
smat(mat1, mat2, mat3)
smat(A=mat1, B=mat2, C=mat3)

mod <- mrgsolve:::house() %>% smat(mat1)

smat(mod)
smat(mod, make=TRUE)





cleanEx()
nameEx("sodll")
### * sodll

flush(stderr()); flush(stdout())

### Name: sodll
### Title: Return the name of the shared object file.
### Aliases: sodll sodll,lockedmod-method sodll,mrgmod-method
###   sodll,packmod-method

### ** Examples

mod <- mrgsolve:::house()
sodll(mod)




cleanEx()
nameEx("soloc")
### * soloc

flush(stderr()); flush(stdout())

### Name: soloc
### Title: Return the location of the model shared object.
### Aliases: soloc

### ** Examples

mod <- mrgsolve:::house()
soloc(mod)




cleanEx()
nameEx("stime")
### * stime

flush(stderr()); flush(stdout())

### Name: tgrid-class
### Title: Get the times at which the model will be evaluated.
### Aliases: stime tgrid-class tgrids-class

### ** Examples


## example("stime", package="mrgsolve")

mod <- mrgsolve:::house(end=12, delta=2, add=c(11,13,15))

stime(mod)

out <- mrgsim(mod, end=-1, add=c(2,4,5))

stime(out)

out$time



cleanEx()
nameEx("tgrid")
### * tgrid

flush(stderr()); flush(stdout())

### Name: stime,mrgmod-method
### Title: Create a simtime object.
### Aliases: *,tgrid,numeric-method *,tgrids,numeric-method
###   +,tgrid,numeric-method +,tgrids,numeric-method c,tgrid-method
###   c,tgrids-method show,tgrid-method show,tgrids-method
###   stime,mrgmod-method stime,mrgsims-method stime,numeric-method
###   stime,tgrid-method stime,tgrids-method tgrid tgrid_*_numeric
###   tgrid_+_numeric tgrids_*_numeric tgrids_+_numeric

### ** Examples


peak <- tgrid(0,6,0.2)
sparse <- tgrid(0,24,4)

day1 <- c(peak,sparse)

design <- c(day1, day1+72, day1+240)

mod <- mrgsolve:::house()

out <- mod %>% ev(amt=1000, ii=24, addl=10) %>% mrgsim(tgrid=design)

plot(out,CP~., type='b')




cleanEx()
nameEx("update")
### * update

flush(stderr()); flush(stdout())

### Name: update
### Title: Update the model object
### Aliases: update update,ev-method update,mrgmod-method
###   update,omegalist-method update,parameter_list-method
###   update,sigmalist-method

### ** Examples

 mod <- mrgsolve:::house()

 mod <- update(mod, end=120, delta=4, param=list(CL=19.1))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
