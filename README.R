##' ---
##' title: ""
##' date: ""
##' author: ""
##' output:
##'   md_document:
##'     variant: markdown_github
##' ---
##'

#+ echo=FALSE
library(lattice)
library(knitr)
opts_chunk$set(comment='.', fig.align="center", fig.path="img/README-")


#+
##'
##'[![Build Status master](https://travis-ci.org/metrumresearchgroup/mrgsolve.svg?branch=master)](https://travis-ci.org/metrumresearchgroup/mrgsolve.svg?branch=master)
##'
##' # Resources
##'
##'
##' * [Latest release: 0.6.0](https://github.com/metrumresearchgroup/mrgsolve/releases)
##' * [Gallery](https://github.com/metrumresearchgroup/mrgsolve/wiki/gallery)
##' * [Wiki](https://github.com/metrumresearchgroup/mrgsolve/wiki)
##'     * [Upcoming changes](https://github.com/metrumresearchgroup/mrgsolve/blob/master/NEWS.md)
##'     * [Windows issues](https://github.com/metrumresearchgroup/mrgsolve/wiki/Windows-issues#issues-with-using-mrgsolve-on-windows-platforms)
##' * [`mrgsolve` discussion](https://groups.google.com/a/metrumrg.com/forum/#!forum/mrgsolve)
##' * [Issue tracker](https://github.com/metrumresearchgroup/mrgsolve/issues)

##'
##' <hr>
##'
##'


##'
##' # Installation
##'
##'  3 primary options exist to install `mrgsolve`:
##'
##'  * Installation from source the source tar.gz file and all dependencies, [as documented in the wiki](https://github.com/metrumresearchgroup/mrgsolve/wiki/mrgsolve-Installation)
##'  * Latest stable version directly from github via:
#+ eval=FALSE
devtools::install_github("metrumresearchgroup/mrgsolve@v0.6.0", subdir="rdev")
##'
##' * The development version from github via:
##'
#+ eval=FALSE
devtools::install_github("metrumresearchgroup/mrgsolve", subdir="rdev")

##' Note that installation from github will automatically install all dependencies, such RcppArmadillo.


##'
##' # About
##' `mrgsolve` facilitates simulation in R from hierarchical,
##' ordinary differential equation (ODE) based models typically
##' employed in drug development. The modeler creates a model specification
##' file consisting of `R` and `C++` code that is parsed, compiled, and dynamically
##' loaded into the `R` session. Input data are passed in and simulated data are
##' returned as `R` objects, so disk access is never required during the simulation
##' cycle after compiling.
##'
##'
##' ## `mrgsolve` is open-source software distributed as a package for `R`
##'
##'   * Installs via `R` package compile and install system
##'   * Easy integration with any other relevant `R` functionality
##'      * Graphics: `lattice`, `ggplot2`
##'      * Model estimation: `nls`,`nlme`, `MCMCpack`, `saemix`, others
##'      * Optimal design: `PFIM`, `PopED`
##'      * Data summary: `dplyr` and many other functions and packages
##'      * Interactive model exploration with `shiny`
##'
#+ message=FALSE
library(mrgsolve)
library(dplyr)
library(ggplot2)



##' ## The model specification file is similar to other non-linear mixed effects modeling software
code <- '
$GLOBAL
#define CP (CENT/VC)
#define INH (CP/(IC50+CP))

$SET delta=0.1

$PARAM TVCL=1, TVVC=20, KA = 1.3, KIN=100, KOUT=2, IC50=10

$CMT GUT, CENT, RESP

$MAIN
  double CL = exp(log(TVCL) + ETA(1));
  double VC = exp(log(TVVC) + ETA(2));

  RESP_0 = KIN/KOUT;

$OMEGA 0 0

$ODE
  dxdt_GUT = -KA*GUT;
  dxdt_CENT = KA*GUT - (CL/VC)*CENT;
  dxdt_RESP = KIN*(1-INH) - KOUT*RESP;

$TABLE capture(CP);
'


##' The model is parsed, compiled, and dynamically loaded into the `R` session
##'
##'   * Information about the model is saved as an `R` object
##'   * Important model attributes can be updated in `R` without recompiling
##'
#+ message=FALSE
mod <- mread("demo", tempdir(), code)


##' # Use `mrgsolve` as an interactive simulation tool for model exploration and sensitivity analyses
##'   * Simulated data are returned as `R` objects
##'   * Input and output data are kept in memory in the `R` process; writing or reading to disk
##'   is never necessary (unless results are to be saved for later use).

out <- mod %>%
  ev(amt=100, ii=24, addl=2) %>%
  mrgsim(end=120)

out


#+ fig.height=3, fig.width=7
plot(out, CP+RESP~.)

out <- mod %>%
  ev(amt=100, ii=24, addl=2) %>%
  Req(CP,RESP) %>%
  knobs(TVVC=c(10,20,40), TVCL=c(0.5,1.5))

#+ fig.height=6, fig.width=9
plot(out, auto.key=list(columns=3))



##' ## Also use `mrgsolve` for efficient, large-scale population simulation

mod <- mod %>% omat(cmat(0.1, 0.67, 0.4))

##' ## Flexibility with input data sets
##' * Data set format that is likely familiar to modeling and simulation scientists
##' * No need to include observation records; `mrgsolve` will automatically insert
##'
data <- expand.ev(ID=1:10, amt=c(100,300,1000)) %>%
  mutate(dose=amt)

head(data)

##' ### Input data are passed in as `R` objects
##'   * Pass many different data sets or implement different designs in the same model code
##'   without recompiling
##'   * Control simulation output from `R` to better manage memory
out <- mod %>%
  data_set(data) %>%
  Req(RESP,CP) %>% obsonly %>%
  carry.out(evid,amt,dose) %>%
  mrgsim(end=48, seed=1010)

#+ fig.height=4, fig.width=9
plot(out, RESP~time|factor(dose), scales="same")

##' ## Pass simulated output to your favorite data summary or visualization routines

##' Summarise with `dplyr`
out %>%
  as.tbl %>%
  group_by(dose) %>%
  summarise(rmin = min(RESP), tmim=time[which.min(RESP)])

##' Plot with `ggplot2`
#+ fig.height=4, fig.width=9, fig.align="center"
out %>%
  as.tbl %>%
  ggplot(data=.) +
  geom_line(aes(x=time, y=RESP, group=ID, col=factor(dose)))



##' <hr>
##' <center><i>Metrum Research Group, LLC 2 Tunxis Rd Suite 112 Tariffville, CT 06081 </i></center>
##' <br>
##' <br>



