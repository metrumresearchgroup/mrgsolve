library(mrgsolve)
library(testthat)
#library(MASS)

Sys.setenv(R_TESTS="")
options("mrgsolve_mread_quiet"=TRUE)

project <- file.path(system.file(package="mrgsolve"), "models")

oral <- function(dose,par,time) {
  par <- as.list(par)
  par$K <- par$CL/par$VC
  with(par, {((KA*dose)/(VC*(KA-K)))*(exp(-K*time) - exp(-KA*time))})
}
iv <- function(dose,par,time) {
  par <- as.list(par)
  par$K <- par$CL/par$VC
  with(par, (dose/VC)*exp(-K*time))

}

mrgsolve::comp_forget()

context("Loading a model via mread")
mod <- suppressMessages(mread("firstmodel", project,atol=1E-20, rtol=1E-12, digits=8))

out <- mrgsim(mod )
comparepo <- signif(oral(as.list(init(mod))$DEPOT, param(mod), stime(mod)), digits=8)

modiv <- mod %>% init(DEPOT=0, CENT=1000)
outiv <- mrgsim(modiv)
compareiv <- signif(iv(as.list(init(modiv))$CENT, param(modiv), stime(modiv)), digits=8)

context("Running a 1 cmt PK model with initial value of 1000")

test_that("Simulation output is of class mrgsims", {
    expect_is(out, "mrgsims")
})
test_that("The simulation model can be recovered from output", {
    expect_identical(mod, mrgsolve::mod(out))
    expect_is(mrgsolve::mod(out), "mrgmod")
})

test_that("CP from oral model is identical to closed form result", {
    expect_true(all(comparepo ==out$CP))
})
test_that("CP from iv model is identical to closed form result", {
    expect_true(all(compareiv == outiv$CP))
})
events1000 <- ev(amt=100, cmt=1000)
test_that("Error on dosing into non-existant compartment", {
    expect_error(mod %>% ev(events10000) %>% mrgsim())
})


imod <- mrgsolve:::house()
out3 <-  mrgsim(imod %>% param(KIN=33, KOUT=11), end=-1, add=numeric())$RESP
out25 <- mrgsim(imod %>% param(KIN=100,KOUT=4), end=-1, add=numeric())$RESP

names(out3) <- NULL
names(out25) <- NULL

imod <- imod %>% init(CENT=12345)

context("Initial conditions are properly calculated")
test_that("Response baseline gets calculated from KIN and KOUT", {
    expect_equal(init(imod %>% param(KIN=33,KOUT=11))$RESP,3)
    expect_equal(init(imod %>% param(KIN=100,KOUT=4))$RESP,25)
    expect_equal(out3,3)
    expect_equal(out25,25)
})

test_that("Fixed initial conditions are properly retreived", {
    expect_equivalent(init(imod)$CENT,12345)
    expect_equivalent(init(imod %>% init(CENT=334455))$CENT,334455)
})

test_that("Calculated initial condition overrides the fixed value", {
    expect_equal(init(imod %>% init(RESP=456789) %>% param(KIN=99, KOUT=3))$RESP,33)
})








## Make some updates to parameters and simulation times
mod1 <- update(mod %>% param(CL=12, VC=220), delta=33, end=222)
mod2 <- update(mod, param=list(CL=12, VC=220), delta=33, end=222)
x <- sort(runif(100, 0,300))
mod3 <- update(mod, add=x, end=1000, delta=20)
mod4 <- update(mod, init=list(CENT=111))
mod5 <- mod %>% param(VC=999)
mod6 <- mod %>% init(DEPOT=5566)
mod7 <- update(mod, hmin=111, hmax=222, maxsteps=333, ixpr=444, mxhnil=555, atol=1E-99, rtol=1E-88)
mod8 <- mrgsolve::mod(mrgsim(mod, delta=33, end=222))
mod9 <- update(mod, delta=33, end=222)
mod10 <- mrgsolve::mod(mrgsim(mod, param=list(CL=12, VC=220)))
mod11 <- mrgsolve::mod(mrgsim(mod  %>% param(CL=12, VC=220)))


context("Test updates: general and simulation times")
test_that("Model object updates through update and %>% operator", {
  expect_true(identical(mod1, mod2))

  expect_true(!identical(mod1, mod))
})

test_that("Simulation times update properly via update",{
  expect_equal(stime(mod1), seq(0,mod1@end, mod2@delta))
  expect_equal(stime(mod3), sort(unique(c(x,seq(0,mod3@end, mod3@delta)))))
})
test_that("Simulation times update properly when passed into mrgsim",{
    expect_identical(mod8,mod9)
})



context("Test updates: parameters and initials")


test_that("Parameters update properly via %>% operator",{
  expect_equal(param(mod1)$CL, 12)
})


test_that("Parameters update properly via update()",{
  expect_equal(param(mod2)$VC, 220)
})

test_that("A parameter that isn't updated remains the same",{
  expect_equal(param(mod2)$KA, param(mod)$KA)
})

test_that("Parameter updates properly when passed to mrgsim",{
  expect_equal(param(mod10)$CL, param(mod2)$CL)
  expect_equal(param(mod10)$VC, param(mod2)$VC)
})

test_that("Parameter updates properly when added inside mrgsim call",{
  expect_equal(param(mod11)$CL, param(mod1)$CL)
  expect_equal(param(mod11)$VC, param(mod1)$VC)
})

rm(mod8,mod9,mod10,mod11)


test_that("Parameters update via param and list",{

  mod1 <- mod %>% param(list(CL=123,VC=456))
  expect_equal(param(mod1)$CL, 123)
  expect_equal(param(mod1)$VC, 456)
})

test_that("Parameters update via param ",{
  
  mod1 <- mod %>% param(CL=1123, VC=1456)
  expect_equal(param(mod1)$CL, 1123)
  expect_equal(param(mod1)$VC, 1456)
})


test_that("Parameters update via param and data.frame ",{
  
  mod1 <- mod %>% param(data.frame(CL=987,VC=654))
  expect_equal(param(mod1)$CL, 987)
  expect_equal(param(mod1)$VC, 654)
})

test_that("Initials update via init and list",{
  
  mod1 <- mod %>% init(list(CENT=123,DEPOT=456))
  expect_equal(init(mod1)$CENT, 123)
  expect_equal(init(mod1)$DEPOT, 456)
})

test_that("Initials update via init ",{
  
  mod1 <- mod %>% init(CENT=1123, DEPOT=1456)
  expect_equal(init(mod1)$CENT, 1123)
  expect_equal(init(mod1)$DEPOT, 1456)
})


test_that("Initials update via init and data.frame ",{
  
  mod1 <- mod %>% init(data.frame(CENT=987,DEPOT=654))
  expect_equal(init(mod1)$CENT, 987)
  expect_equal(init(mod1)$DEPOT, 654)
})




test_that("Initial conditions update properly via update()",{
  expect_equal(init(mod6)$DEPOT,5566)
})





context("Test updates: solver settings")

test_that("Solver setting hmin updates properly", {
    expect_equal(mod7@hmin,111)
})
test_that("Solver setting hmax updates properly", {
    expect_equal(mod7@hmax,222)
})
test_that("Solver setting maxsteps updates properly", {
    expect_equal(mod7@maxsteps,333)
})
test_that("Solver setting ixpr updates properly", {
    expect_equal(mod7@ixpr,444)
})
test_that("Solver setting mxhnil updates properly", {
    expect_equal(mod7@mxhnil,555)
})
test_that("Solver setting atol updates properly", {
    expect_equal(mod7@atol,1E-99)
})
test_that("Solver setting rtol updates properly", {
    expect_equal(mod7@rtol,1E-88)
})


context("Test updates: events")
myev <- ev(amt=111, time=222, ii=333, addl=444, cmt=999)

test_that("Events update properly through %>% operator",{
   expect_equivalent(myev, events(mod %>% ev(myev)))
})
test_that("Events update properly through update()",{
   expect_equivalent(myev, events(update(mod, events=myev)))
})

ev1 <- ev(amt=555,cmt=1,rate=33)
ev2 <- ev(amt=444,cmt=5,rate=22)
ev12 <- ev1+ev2

test_that("Events update properly through %>%  and  + operator", {
    expect_equivalent(ev12, events(mod %>% ev(ev1 + ev2)))
})





context("Testing error generation ")
test_that("Error when parameters and compartments have same name", {
  expect_error(mrgmod("firstmodel", project, param=list(CENT=2)))
  expect_error(mrgmod("firstmodel", project, init=list(CL=2)))
})

test_that("Error when parameters and compartments have same name introduced via update", {
  expect_error(update(mod, param=list(CENT=2), strict=FALSE))
  expect_error(update(mod, init=list(CL=2), strict=FALSE))
})

test_that("Error when a parameter name is listed in compartments", {
  expect_error(update(mod, init=list(CL=2), strict=FALSE))
  expect_error(mrgmod("firstmodel", project, init=list(CL=2)))
})
test_that("Error  when a compartment names is listed in parameters", {
  expect_error(update(mod, param=list(CENT=2), strict=FALSE))
  expect_error(mrgmod("firstmodel", project, param=list(CENT=2)))
})

bad <- update(mod, param=list(FAKEPARAMETER=22), strict=FALSE)
test_that("Error when the number of parameters changes without recompile",{
    expect_error(mrgsim(bad))
})





context("Test utils")
test_that("Corecing simulated output to data.frame", {
  expect_is(as.data.frame(out), "data.frame")
})
test_that("Corecing simulated output to matrix", {
  expect_is(as.matrix(out), "matrix")
})
test_that("Corecing parameters to list", {
  expect_is(as.list(param(mod)), "list")
})
test_that("Corecing parameters to numeric", {
  expect_is(as.numeric(param(mod)), "numeric")
})
test_that("Corecing initials to list", {
  expect_is(as.list(init(mod)), "list")
})
test_that("Corecing initials to numeric", {
  expect_is(as.numeric(init(mod)), "numeric")
})
test_that("Corecing parameters to data.frame", {
  expect_is(as.data.frame(param(mod)), "data.frame")

})
test_that("Corecing initials to data.frame", {
  expect_is(as.data.frame(init(mod)), "data.frame")
})



test_that("stime correctly generates simulation times", {
  expect_equal(stime(mod), seq(0,mod@end, mod@delta))
  expect_equal(stime(update(mod, end=-1, add=c(1,6,9))), c(1,6,9))
  expect_error(stime(update(mod, end=-1, add=c())))
})
test_that("Negative end time gives simulations at add only", {
  expect_equal(stime(update(mod, end=-1, add=c(1,6,9))), c(1,6,9))
  expect_error(stime(update(mod, end=-1, add=c())))
})
test_that("If no simulation times can be rendered time=0 only is simulated", {
    out <- mrgsim(mod, end=-1, add=numeric(0))
    expect_equal(unique(out$time),0)
})



test_that("Get parameter names with pars()", {
  expect_equivalent(names(param(mod)), pars(mod))
})

test_that("Get compartment names with cmt()", {
  expect_equivalent(names(param(mod)), pars(mod))
})



context("Test knobs")
out <- knobs(mod %>% init(DEPOT=0), CL=c(1,2,3), foo=c(2,3,4),fooo=1, amt=c(100,200), cmt=1)
dfout <- as.data.frame(out)

test_that("knobs() returns object of class batch_mrgsims", {
   expect_is(out, "batch_mrgsims")
})

test_that("Moving knobs are correctly identified", {
   expect_identical(moving(out), c("CL", "amt"))
})

test_that("amt knob is correctly captured in output in Amt", {
   expect_true(is.element("Amt", names(out)))
})


test_that("CL knob is correctly captured in output as CL", {
   expect_true(is.element("CL", names(out)))
   expect_identical(unique(dfout$CL),c(1,2,3))
})

test_that("A false knob does not appear in simulated output", {
   expect_false(is.element("foo", names(out)))
})

test_that("Knob potential knob errors are caught", {
   expect_error(knobs(mod, CL=c(1,2,3), amt=c(100,200)))
})

context("Testing events (basic testing)")

out1 <- mrgsim(mod %>%init(CENT=0, DEPOT=0) %>% ev(amt=1000, time=0, cmt=1), carry.out="evid")
out2 <- mrgsim(mod %>% init(CENT=0, DEPOT=1000), carry.out="evid")

test_that("Events initialize properly", {
    expect_equivalent(unlist(subset(out1, evid==0 & time > 0)), unlist(subset(out2, evid==0 & time >0)))  ## different row names
})



context("Testing parameter updates via idata")
CL <- exp(rnorm(100, log(3),  sqrt(0.5)))
VC <- exp(rnorm(100, log(30), sqrt(0.5))) 
pars <- signif(data.frame(CL=CL,VC=VC),6)
pars$ID <- 1:nrow(pars)

out <- mrgsim(mod, idata=pars, end=8, carry.out=c("CL", "VC"))
out <- out %>% as.tbl %>% distinct(ID, .keep_all=TRUE)
out <- signif(as.data.frame(out[,c("CL", "VC", "ID")]),6)


test_that("We can recover CL VC and ID from simulated data when passed in as idata",{
     expect_equivalent(out,pars)
})


context("Testing parameter updates via data")

data <- expand.grid(time=seq(0,12,1), ID=1:100, cmt=1)
data <- merge(data, pars, sort=FALSE)

out <- mrgsim(mod, data=data, carry.out=c("CL", "VC"))
out <- out %>% as.tbl %>% distinct(ID, .keep_all=TRUE)
out <- signif(as.data.frame(out)[,c("CL", "VC", "ID")], 6)

test_that("We can recover CL VC and ID from simulated data when passed in as data", {
   expect_equivalent(out,pars)
})

events <- ev(time=c(0,24,48), amt=1000, rate=50, addl=c(0,0,10), ii=12,cmt=1)
out1 <- mrgsim(mod %>% ev(events), idata=data.frame(ID=1:20), end=200,
               carry.out=c("evid", "amt", "rate", "addl", "ii","cmt"), req="")
data1 <- as.data.frame(out1)

out2 <- mrgsim(mod, data=data1, carry.out=c("evid", "amt", "rate", "addl", "ii"), req="")
data2 <- as.data.frame(out2)

context("Check that events-based simulation gives same results as data set-based simulation")
test_that("CP is equal when simulating from events or data", {
    expect_identical(data1$CP, data2$CP)
})


context("Testing time-varying data items  passed in via data set")

set.seed(11111)
data1$ROW <- sample(1:nrow(data1))

out <- mrgsim(mod, data=data1, carry.out=c("ROW"))

test_that("Time-varying data items in data are properly carried into output", {
  expect_true(all(data1$ROW == out$ROW))
})

# 
# 
# context("Setup and run model with cpp_model")
# code <- "
# // @param: CL=1, VC=2
# // @init: CENT=0
# // @end: 36
# // @delta: 0.2
# 
# BEGIN_ode
#   dxdt_CENT = -(CL/VC)*CENT;
# DONE
# 
# BEGIN_table table(CP) = CENT/VC;DONE
# "
# 
# test_that("Non-existant project generates error", {
#    expect_error(cpp_model(code, project="a_non_existant_directory"))
# })
# test_that("No code generates error", {
#    expect_error(cpp_model(project="a_non_existant_directory"))
# })
# test_that("Model writes compiles and loads", {
#     mod <- cpp_model(code,model="test1", end=120)
#     expect_is(mod,"mrgmod")
#     expect_true(file.exists(cfile(mod)))
#     out <- mrgsim(mod)
#     expect_is(out,"mrgsims")
# })
# 
# 
# context("Testing that appropriate errors are generated using mrgmod")
# test_that("error generated when project doesn't exist", {
#     expect_error(mrgmod("foo", "bar"))
# })
# 
# test_that("Error generated when spec file doesn't exist", {
#     expect_error(mrgmod("foo00000"))
# })
# 
# 
# test_that("Error when compartment names overlap with parameter names", {
#    expect_error((new("mrgmod") < init(GUT=0, CENT=0)) < param(CL=2, VC=3, GUT=100))
#    expect_error((new("mrgmod") < param(CL=2, VC=3) )  < init(GUT=0, CL=0))
# })
# 
# test_that("Error when compartment or parameter doesn't have a name", {
#    expect_error(new("mrgmod") < init(0, CENT=0))
#    expect_error(new("mrgmod") < param(CL=2, 3) )
# })
# 

context("Testing that plots can be generated from output objects")

mod <- mrgsolve:::house() %>% init(CENT=1000)
out <- mrgsim(mod)


test_that("Plot from mrgsims without a formula", {
    expect_is(plot(out), "trellis")
})

test_that("Plot from mrgsims with a formula", {
    expect_is(plot(out, CP~time), "trellis")
})



out <- knobs(mod, CL=c(1,2,3), VC=c(10,20))

test_that("Plot from batch_mrgsims without a formula", {
    expect_is(plot(out), "trellis")

})

test_that("Plot from batch_mrgsims with a formula", {
    expect_is(plot(out, CP~time), "trellis")
})


context("Checking that random number generation respects R RNG via set.seed()")
mod <- mrgsolve:::house(omega=diag(c(1,1,1,1)))

out1 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
out2 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
set.seed(333)
out3 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))
set.seed(333)
out4 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20))

out5 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20),seed=555)
out6 <- mrgsim(mod %>% init(GUT=100), idata=data.frame(ID=1:20),seed=555)


out7 <- simre(out5)
out8 <- simre(out6)
out9 <- simre(out6)


ident <- function(x,y,...) {
  x@date <- y@date <- date()
  identical(x,y) 
}

test_that("Runs with different seeds give different results without call to set.seed()", {
    expect_false(ident(out1,out2))
})

test_that("Runs with different seeds give different results with different calls to set.seed()", {
    expect_false(ident(out3,out5))
})
test_that("Runs with same seeds give same results with call to set.seed()", {
    expect_true(ident(out3,out4))
})
test_that("Runs with same seeds give same results when seed passed to mrgsim()", {
    expect_true(ident(out5,out6))
})
test_that("Random effects are properly recovered from runs with same seed passed", {
    expect_true(identical(out7,out8))
    expect_true(identical(out8,out9))
})
test_that("Random effects are properly recovered by repeated calls to simre", {
    expect_true(identical(out8,out9))
})


