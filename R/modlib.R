# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.


##' Internal model library.
##' 
##' @param list list available models
##' @export
##' 
##' @details
##' See \code{\link{modlib_details}}, \code{\link{modlib_pk}}, \code{\link{modlib_pkpd}}, 
##' \code{\link{modlib_tmdd}}, \code{\link{modlib_viral}} for details.
##' 
##' Call \code{modlib(list=TRUE)} to list available models.  Once the model 
##' is loaded (see examples below), call \code{mrgsolve:::code(mod)} to see
##' model code and equations.
##' 
##' 
##' @examples
##' \dontrun{
##' mod <- mread("pk1cmt", modlib())
##' mod <- mread("pk2cmt", modlib()) 
##' mod <- mread("pk3cmt", modlib()) 
##' mod <- mread("irm1",   modlib()) 
##' mod <- mread("irm2",   modlib()) 
##' mod <- mread("irm3",   modlib()) 
##' mod <- mread("irm4",   modlib())
##' mod <- mread("emax",   modlib())
##' mod <- mread("effect", modlib())
##' mod <- mread("tmdd",   modlib())
##' mod <- mread("viral1", modlib())
##' mod <- mread("viral2", modlib())
##' 
##' mrgsolve:::code(mod)
##' }
##' 
modlib <- function(list=FALSE)  {
  if(list) return(modlib_list())
  return(object_dir() )
}

modlib_models <- c("pk1cmt", "pk2cmt", "pk3cmt","irm1", "irm2", "irm3",
           "emax", "tmdd", "viral1", "viral2", "effect")

modlib_list <- function() {
  message("mrgsolve internal library:")
  models <- readLines(pfile("mrgsolve", "models", "MODLIST"))
  cat(paste0("  ",models),sep="\n")
  return(invisible(NULL))
}

##' modlib: PK/PD Model parameters, compartments, and output variables.
##' 
##' @name modlib_details
##'
##' @section Compartments:
##' \itemize{
##' \item{\code{EV1}, \code{EV2}}: extravasular dosing compartments
##' \item{\code{CENT}}: central PK compartment
##' \item{\code{PERIPH}}: peripheral PK compartment
##' \item{\code{PERIPH2}}: peripheral PK compartment 2
##' \item{\code{RESP}}: response PD compartment (irm models)
##' }
##'
##' @section Output variables:
##' \itemize{
##' \item{\code{CP}}: concentration in the central compartment (\code{CENT/VC})
##' \item{\code{RESP}}: response (emax model)
##' }
##'
##' @section PK parameters:
##' \itemize{
##' \item{\code{KA1}, \code{KA2}}:  first order absorption rate constants from first and second extravascular compartment (1/time)
##' \item{\code{CL}}: clearance (volume/time)
##' \item{\code{VC}}: volume of distribution, central compartment (volume)
##' \item{\code{VP}}:  volume of distribution, peripheral compartment (volume)
##' \item{\code{VP2}}: volume of distribution, peripheral compartment 2 (volume)
##' \item{\code{Q}}: intercompartmental clearance (volume/time)
##' \item{\code{Q2}}: intercompartmental clearance 2 (volume/time)
##' \item{\code{VMAX}}:  maximum rate, nonlinear process (mass/time)
##' \item{\code{KM}}: Michaelis constant (mass/volume)
##' \item{\code{K10}}: elimination rate constant (1/time); \code{CL/VC}
##' \item{\code{K12}}: rate constant for transfer to peripheral compartment from central (1/time); \code{Q/VC}
##' \item{\code{K21}}: rate constant for transfer to central compartment from peripheral (1/time); \code{Q/VP}
##' }
##'
##' @section PD parameters:
##' \itemize{
##' \item{\code{E0}}: baseline effect (emax model)
##' \item{\code{EMAX}, \code{IMAX}}: maximum effect (response)
##' \item{\code{EC50}, \code{IC50}}: concentration producing 50 percent of effect (mass/volume)
##' \item{\code{KIN}}: zero-order response production rate (irm models) (response/time)
##' \item{\code{KOUT}}: first-order response elimination rate (irm models) (1/time)
##' \item{\code{n}}: sigmoidicity factor
##' \item{\code{KEO}}: rate constant for transfer to effect compartment (1/time)
##' }
NULL


cfile_dir <- function() {
  file.path(path.package("mrgsolve"), "models")
}

object_dir <- function() {
  file.path(path.package("mrgsolve"), "models")
}

mintern <- function(model,tryload=TRUE,...) {
  
  if(!is.element(model,modlib_models)) {
    stop(model, " not found in the library. Use modlib(list=TRUE) to list models.",call.=FALSE)
  }
  
  message("Compiling model: ", model)
  
  code <- scan(filename(object_dir(), model, ".cpp"),
               what=character(0),sep="\n",quiet=TRUE)
  if(length(code)>1) code <- paste(code,collapse="\n")
  newmodel <- paste0("mintern_", model)
  x <- mrgsolve::mcode(newmodel,code,quiet=TRUE)
  x <- update(x,...)
  a <- try(mrgsolve::touch_funs(x),silent=TRUE)
  if(inherits(a,"try-error")) {
    stop("There was an error in this model.",call.=FALSE)
  }
  return(x)
}


##' modlib: Pharmacokinetic models.
##' 
##' @name modlib_pk
##' @param ... passed to update
##'
##' @section Model description:
##' All pk models have two extravascular dosing compartments and potential for linear and nonlinear clearance.
##' \itemize{
##'  \item{\code{pk1cmt}}: one compartment pk model
##'  \item{\code{pk2cmt}}: two compartment pk  model
##'  \item{\code{pk3cmt}}: three compartment pk model
##' }
##'
##' @details
##'
##' See \code{\link{modlib_details}} for more detailed descriptions of parameters and compartments.
##'
##' The \code{pk1cmt} model is parameterized in terms of \code{CL}, \code{VC}, \code{KA1} and \code{KA2} and uses compartments \code{EV1},
##' \code{EV2}, and \code{CENT}.  The \code{pk2cmt} model adds a \code{PERIPH} compartment and parameters \code{Q} and \code{VP} to that of the
##' one-compartment model.  Likewise, the three-compartment model (\code{pk3cmt}) adds \code{PERIPH2} and parameters \code{Q2} and \code{VP2} to
##' that of the two-compartment models.  All pk models also have parameters \code{VMAX} (defaulting to zero, no non-linear clearance) and \code{KM}.
##'
##' @return an object of class \code{packmod}
##'
NULL



pk1cmt <- function(...) {mintern("pk1cmt",...)}
pk2cmt <- function(...) return(mintern("pk2cmt",...))
pk3cmt <- function(...) return(mintern("pk3cmt", ...))
pk1cmt_pop <- function(...) return(mintern("pk1cmt_pop", ...))
irm1 <- function(...) return(mintern("irm1",...))
irm2 <- function(...) return(mintern("irm2",...))
irm3 <- function(...) return(mintern("irm3",...))
irm4 <- function(...) return(mintern("irm4",...))
emax <- function(...) return(mintern("emax",...))
tmdd <- function(...) return(mintern("tmdd",...))
viral1 <- function(...) return(mintern("viral1",...))
viral2 <- function(...) return(mintern("viral2",...))
pd_effect <- function(...) mintern("effect",...) 


##' modlib: Pharmacokinetic / pharmacodynamic models. 
##' 
##' @name modlib_pkpd
##' @details
##'
##' See \code{\link{modlib_details}} for more detailed descriptions of parameters and compartments.
##'
##' All PK/PD models include 2-compartment PK model with absorption from 2 extravasular compartments and linear + nonlinear clearance.  The PK models are parameterized with \code{CL}, \code{VC}, \code{Q}, \code{VMAX}, \code{KM}, \code{KA1} and \code{KA2} and implement compartments \code{EV1}, \code{EV2}, \code{CENT}, \code{PERIPH} .  The indirect response models have compartment \code{RESP} and the emax model has output variable \code{RESP}.  PD parameters include \code{KIN}, \code{KOUT}, \code{IC50}, \code{EC50}, \code{IMAX}, \code{EMAX}, \code{E0}, and \code{n}.
##'
##' Also, once the model is loaded, use \code{\link{see}} method for \code{mrgmod} to view the model code.
##'
##' @section Model description:
##' \itemize{
##'  \item{\code{irm1}} inhibition of response production
##'  \item{\code{irm2}} inhibition of response loss
##'  \item{\code{irm3}} stimulation of response production
##'  \item{\code{irm4}} stimulation of response loss
##'  \item{\code{pd_effect}} effect compartment model
##'  \item{\code{emax}} sigmoid emax model
##' }
##' 
##'
##'
NULL



##' modlib: Target mediated disposition model.
##' @name modlib_tmdd
##' @param ... passed to update
##' 
##'
##' @section Parameters:
##' \itemize{
##' \item{\code{KEL}}: elimination rate constant
##' \item{\code{KTP}}: tissue to plasma rate constant
##' \item{\code{KPT}}: plasma to tissue rate constant
##' \item{\code{VC}}: volume of distribution
##' \item{\code{KA1}, \code{KA2}}: absorption rate constants
##' \item{\code{KINT}}: internalization rate constant
##' \item{\code{KON}}: association rate constant
##' \item{\code{KOFF}}: dissociation rate constant
##' \item{\code{KSYN}}: target systhesis rate
##' \item{\code{KDEG}}: target degredation rate constant
##' }
##'
##' @section Compartments:
##' \itemize{
##' \item{\code{CENT}}: unbound drug in central compartment
##' \item{\code{TISS}}: unbound drug in tissue compartment
##' \item{\code{REC}}: concentration of target
##' \item{\code{RC}}: concentration of drug-target complex
##' \item{\code{EV1}, \code{EV2}}: extravascular dosing compartments
##' }
##'
##' @section Output variables:
##' \itemize{
##' \item{\code{CP}}: unbound drug in the central compartment
##' \item{\code{TOTAL}}: total concentration of target (complexed and uncomplexed)
##' }
##'
NULL



##' modlib: HCV viral dynamics models.
##' 
##' @name modlib_viral
##' 
##' @section Models:
##' \itemize{
##'  \item{\code{viral1}}: viral dynamics model with single HCV species
##'  \item{\code{viral2}}: viral dynamics model with wild-type and mutant HCV species
##' }
##'
##' @section Parameters:
##' \itemize{
##' \item{\code{s}}: new hepatocyte synthesis rate (cells/ml/day)
##' \item{\code{d}}: hepatocyte death rate constant (1/day)
##' \item{\code{p}}: viral production rate constant (copies/cell/day)
##' \item{\code{beta}}: new infection rate constant (ml/copy/day)
##' \item{\code{delta}}: infected cell death rate constant (1/day)
##' \item{\code{c}}: viral clearance rate constant (1/day)
##' \item{\code{fit}}: mutant virus fitness
##' \item{\code{N}}: non-target hepatocytes
##' \item{\code{mu}}: forward mutation rate
##' \item{\code{Tmax}}: maximum number of target hepatocytes (cells/ml)
##' \item{\code{rho}}: maximum hepatocyte regeneration rate (1/day)
##' }
##'
##' @section Compartments:
##' \itemize{
##' \item{\code{T}}: uninfected target hepatocytes (cells/ml)
##' \item{\code{I}}: productively infected hepatocytes (cells/ml)
##' \item{\code{V}}: hepatitis C virus (copies/ml)
##' \item{\code{IM}}: mutant infected hepatocytes (cells/ml)
##' \item{\code{VM}}: mutant hepatitis C virus (copies/ml)
##' \item{\code{expos}}: exposure metric to drive pharmacodynamic model
##' }
##'
##' 
##'
##'
NULL


##' Extract the code from a model.
##' 
##' @param x an mrgsolve model object
##' @return a character vector of model code
##' 
code <- function(x) {
  stopifnot(is.mrgmod(x))
  what <- try(x@code, silent=TRUE)
  if(inherits("try-error",what)) {
    message("Could not find model code.")
    return(invisible(NULL))
  }
  return(what)
}

