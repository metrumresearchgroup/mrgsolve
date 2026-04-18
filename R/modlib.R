# Copyright (C) 2013 - 2026  Metrum Research Group
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


#' Internal model library
#' 
#' Pre-coded models are included in the mrgsolve installation; these can be 
#' compiled and loaded with `modlib()`. These models are usually most useful 
#' for exploratory simulation or learning mrgsolve. Production simulation work 
#' is typically accomplished by a custom-coded model.
#' 
#' @param model `character` name of a model in the library.
#' @param ... passed to [mread_cache()].
#' @param list logical; if `TRUE`, a list of available models is returned.
#' 
#' @details
#' See [modlib_details], [modlib_pk], [modlib_pkpd], [modlib_tmdd], 
#' [modlib_viral] for details.
#' 
#' Call `modlib("<modelname>")` to compile and load a mode from the 
#' library.
#' 
#' Call `modlib(list=TRUE)` to list available models.  Once the model 
#' is loaded (see examples below), call `as.list(mod)$code` to extract
#' model code and equations.
#' 
#' @examples
#' \dontrun{
#' mod <- mread("pk1cmt", modlib())
#' mod <- mread("pk2cmt", modlib()) 
#' mod <- mread("pk3cmt", modlib()) 
#' mod <- mread("pk1",    modlib())
#' mod <- mread("pk2",    modlib())
#' mod <- mread("pk3",    modlib())
#' mod <- mread("popex",  modlib())
#' mod <- mread("irm1",   modlib()) 
#' mod <- mread("irm2",   modlib()) 
#' mod <- mread("irm3",   modlib()) 
#' mod <- mread("irm4",   modlib())
#' mod <- mread("emax",   modlib())
#' mod <- mread("effect", modlib())
#' mod <- mread("tmdd",   modlib())
#' mod <- mread("viral1", modlib())
#' mod <- mread("viral2", modlib())
#' mod <- mread("pred1",  modlib())
#' mod <- mread("pbpk",   modlib())
#' mod <- mread("1005",   modlib())  # embedded NONMEM result
#' mod <- mread("nm-like", modlib()) # model with nonmem-like syntax
#' mod <- mread("evtools", modlib())
#' 
#' as.list(mod)$code
#' }
#' @md
#' @export
modlib <- function(model = NULL,...,list=FALSE)  {
  if(list) {
    return(modlib_list())
  }
  if(is.character(model)) {
    return(mread_cache(model, project = modlib(), ...))
  }
  return(object_dir())
}

#nocov start
modlib_models <- c(
  "pk1cmt", "pk2cmt", "pk3cmt", "pk", "pk1", "pk2", "pk3", "pk3iv", "popex",
  "irm1", "irm2", "irm3", "pred1", "emax", "tmdd", "viral1", 
  "viral2", "effect", "1005", "nm-like", "evtools"
)
#nocov end

modlib_list <- function() {
  message("mrgsolve internal library:")
  models <- readLines(pfile("mrgsolve", "models", "MODLIST"))
  message(paste0("  ",models),sep="\n")
  return(invisible(NULL))
}

#' modlib: PK/PD Model parameters, compartments, and output variables
#'
#' @name modlib_details
#'
#' @section Compartments:
#' - `EV`, `EV2`: extravascular dosing compartments
#' - `CENT`: central PK compartment
#' - `PERIPH`: peripheral PK compartment
#' - `PERIPH2`: peripheral PK compartment 2
#' - `RESP`: response PD compartment (irm models)
#'
#' @section Output variables:
#' - `CP`: concentration in the central compartment
#' - `RESP`: response (emax model)
#'
#' @section PK parameters:
#' - `KA`, `KA2`: first order absorption rate constants from first and
#'   second extravascular compartment (1/time)
#' - `CL`: clearance (volume/time)
#' - `V`: volume of distribution (volume)
#' - `V2`: volume of distribution, central compartment (volume)
#' - `V3`: volume of distribution, peripheral compartment (volume)
#' - `V4`: volume of distribution, peripheral compartment 2 (volume)
#' - `Q`: intercompartmental clearance (volume/time)
#' - `Q3`: intercompartmental clearance (volume/time)
#' - `Q4`: intercompartmental clearance 2 (volume/time)
#' - `VMAX`: maximum rate, nonlinear process (mass/time)
#' - `KM`: Michaelis constant (mass/volume)
#' - `K10`: elimination rate constant (1/time)
#' - `K12`: rate constant for transfer to peripheral compartment from
#'   central (1/time)
#' - `K21`: rate constant for transfer to central compartment from
#'   peripheral (1/time)
#'
#' @section PD parameters:
#' - `E0`: baseline effect (emax model)
#' - `EMAX`, `IMAX`: maximum effect (response)
#' - `EC50`, `IC50`: concentration producing 50 percent of effect
#'   (mass/volume)
#' - `KIN`: zero-order response production rate (irm models) (response/time)
#' - `KOUT`: first-order response elimination rate (irm models) (1/time)
#' - `n`: sigmoidicity factor
#' - `KEO`: rate constant for transfer to effect compartment (1/time)
#'
#' @md
NULL

#nocov start
# TODO: clean
cfile_dir <- function() {
  file.path(path.package("mrgsolve"), "models")
}

object_dir <- function() {
  file.path(path.package("mrgsolve"), "models")
}
#nocov end

#' modlib: Pharmacokinetic models
#'
#' @name modlib_pk
#' @param ... passed to update.
#'
#' @section Model description:
#' All ODE-based pk models have two extravascular dosing compartments and 
#' potential for linear and nonlinear clearance.
#' - `pk1cmt`: one compartment pk model using ODEs
#' - `pk2cmt`: two compartment pk model using ODEs
#' - `pk3cmt`: three compartment pk model using ODEs
#' - `pk1`: one compartment pk model in closed-form
#' - `pk2`: two compartment pk model in closed-form
#' - `pk3`: three compartment pk model in closed-form
#' - `popex`: a simple population pk model
#'
#' @details
#'
#' See [modlib_details] for more detailed descriptions of parameters and
#' compartments.
#'
#' The `pk1cmt` model is parameterized in terms of `CL`, `V`, `KA` and
#' `KA2` and uses compartments `EV`, `EV2`, and `CENT`. The `pk2cmt` model
#' adds a `PERIPH` compartment and parameters `Q` and `V3` to that of the
#' one-compartment model. Likewise, the three-compartment model (`pk3cmt`)
#' adds `PERIPH2` and parameters `Q4` and `V4` to that of the
#' two-compartment models. All pk models also have parameters `VMAX`
#' (defaulting to zero, no non-linear clearance) and `KM`.
#'
#' @md
NULL


#' modlib: Pharmacokinetic / pharmacodynamic models
#'
#' @name modlib_pkpd
#' @details
#'
#' See [modlib_details] for more detailed descriptions of parameters and
#' compartments.
#'
#' All PK/PD models include 2-compartment PK model with absorption from
#' 2 extravascular compartments and linear + nonlinear clearance. The
#' PK models are parameterized with `CL`, `V2`, `Q`, `V3, `VMAX`, `KM`, `KA`
#' and `KA2` and implement compartments `EV`, `EV2`, `CENT`, `PERIPH`.
#' The indirect response models have compartment `RESP` and the emax
#' model has output variable `RESP`. PD parameters include `KIN`, `KOUT`,
#' `IC50`, `EC50`, `IMAX`, `EMAX`, `E0`, and `n`.
#'
#' Also, once the model is loaded, use the [see()] method for `mrgmod` to
#' view the model code.
#'
#' @section Model description:
#' - `irm1`: inhibition of response production
#' - `irm2`: inhibition of response loss
#' - `irm3`: stimulation of response production
#' - `irm4`: stimulation of response loss
#' - `pd_effect`: effect compartment model
#' - `emax`: sigmoid emax model
#'
#' @md
NULL


#' modlib: Target mediated disposition model
#'
#' @name modlib_tmdd
#' @param ... passed to update.
#'
#' @section Parameters:
#' - `KEL`: elimination rate constant
#' - `KTP`: tissue to plasma rate constant
#' - `KPT`: plasma to tissue rate constant
#' - `V2`: volume of distribution
#' - `KA`, `KA2`: absorption rate constants
#' - `KINT`: internalization rate constant
#' - `KON`: association rate constant
#' - `KOFF`: dissociation rate constant
#' - `KSYN`: target synthesis rate
#' - `KDEG`: target degradation rate constant
#'
#' @section Compartments:
#' - `CENT`: unbound drug in central compartment
#' - `TISS`: unbound drug in tissue compartment
#' - `REC`: concentration of target
#' - `RC`: concentration of drug-target complex
#' - `EV`, `EV2`: extravascular dosing compartments
#'
#' @section Output variables:
#' - `CP`: unbound drug in the central compartment
#' - `TOTAL`: total concentration of target (complexed and uncomplexed)
#'
#' @md
NULL



#' modlib: HCV viral dynamics models
#'
#' @name modlib_viral
#'
#' @section Models:
#' - `viral1`: viral dynamics model with single HCV species
#' - `viral2`: viral dynamics model with wild-type and mutant HCV species
#'
#' @section Parameters:
#' - `s`: new hepatocyte synthesis rate (cells/ml/day)
#' - `d`: hepatocyte death rate constant (1/day)
#' - `p`: viral production rate constant (copies/cell/day)
#' - `beta`: new infection rate constant (ml/copy/day)
#' - `delta`: infected cell death rate constant (1/day)
#' - `c`: viral clearance rate constant (1/day)
#' - `fit`: mutant virus fitness
#' - `N`: non-target hepatocytes
#' - `mu`: forward mutation rate
#' - `Tmax`: maximum number of target hepatocytes (cells/ml)
#' - `rho`: maximum hepatocyte regeneration rate (1/day)
#'
#' @section Compartments:
#' - `T`: uninfected target hepatocytes (cells/ml)
#' - `I`: productively infected hepatocytes (cells/ml)
#' - `V`: hepatitis C virus (copies/ml)
#' - `IM`: mutant infected hepatocytes (cells/ml)
#' - `VM`: mutant hepatitis C virus (copies/ml)
#' - `expos`: exposure metric to drive pharmacodynamic model
#'
#' @md
NULL


#' Extract the code from a model
#' 
#' This function is currently not exported, so be sure to call it with 
#' `mrgsolve:::code(...)`.
#' 
#' @param x a model object.
#' 
#' @examples
#' mod <- mrgsolve::house()
#' mrgsolve:::code(mod)
#' 
#' # Alternative
#' as.list(mod)$code
#' 
#' @return 
#' A character vector of model code.
#' 
#' @md
code <- function(x) {
  stopifnot(is.mrgmod(x))
  what <- try(x@code, silent=TRUE)
  if(inherits("try-error",what)) {
    message("Could not find model code.")
    return(invisible(NULL))
  }
  return(what)
}
