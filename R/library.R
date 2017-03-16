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

modpk1 <- '
$PARAM CL = 1, V = 20
$CMT CENT
$PKMODEL ncmt=1, depot = FALSE,trans=1
$TABLE double CP = CENT/pred_V;
$CAPTURE CP
$OMEGA 0 0
labels=s(ECL,EV)
$MAIN
pred_CL = CL*exp(ETA(1));
pred_V  = V *exp(ETA(2));
'
modpk1po <- '
$PARAM CL = 1, V = 20, KA=1.1
$CMT DEPOT CENT
$PKMODEL ncmt=1, depot = TRUE,trans=1
$OMEGA 0 0 0
labels=s(ECL,EV,EKA)
$TABLE double CP = CENT/pred_V;
$CAPTURE CP
$MAIN
pred_CL   = CL*exp(ETA(1));
pred_V    = V *exp(ETA(2));
pred_KA   = KA*exp(ETA(3));
'
modpk2 <- '
$PARAM CL = 1, V1 = 20, Q = 20, V2 = 100
$CMT CENT PERIPH
$PKMODEL ncmt=2, depot = FALSE,trans=1
$OMEGA 0 0 0 0
labels=s(ECL,EV2,EQ,EV3)
$TABLE double CP = CENT/pred_V2;
$CAPTURE CP
$MAIN
pred_CL = CL*exp(ETA(1));
pred_V2 = V1*exp(ETA(2));
pred_Q  = Q *exp(ETA(3));
pred_V3 = V2*exp(ETA(4));
'
modpk2po <- '
$PARAM CL = 1, V2 = 20, Q = 20, V3 = 100, KA=1.1
$CMT DEPOT CENT PERIPH
$PKMODEL ncmt=2, depot = TRUE,trans=1
$OMEGA 0 0 0 0 0
labels=s(ECL,EV2,EQ,EV3,EKA)
$TABLE double CP = CENT/pred_V2;
$CAPTURE CP
$MAIN
pred_CL = CL*exp(ETA(1));
pred_V2 = V2*exp(ETA(2));
pred_Q =  Q *exp(ETA(3));
pred_V3 = V3*exp(ETA(4));
pred_KA = KA*exp(ETA(5));
'

##'
##' Simulate from 1- or 2-compartment PK model.
##'
##' This is an R function that returns model objects based on \code{$PKMODEL}.
##'
##' @param ncmt passed to \code{\link{PKMODEL}}
##' @param depot passed to \code{\link{PKMODEL}}
##' @param ... passed to \code{\link{update}}
##' @return An object of class \code{\link{mrgmod-class}}
##'
##'
##' @details Once the model object is generated, use \code{\link{param}} to check names of the parameters in the model and \code{\link{init}} to check
##' the names of the compartments in the model.  Calculations for the amounts in each compartment are done via analytical solutions, not differential equations.
##' A subject-level random effect is also provided for each PK parameter; use \code{\link{omat}} to see the names of those random effects.  All random effect
##' variances have initial value of zero and may be updated via \code{\link{omat}}.
##'
##' @examples
##'
##' \dontrun{
##' mod <- pkmodel(1)
##'
##' mod %>% ev(amt=1000, ii=24, addl=3) %>% mrgsim(end=120)
##' mod <- pkmodel(1,TRUE)
##' mod <- pkmodel(2)
##' mod <- pkmodel(2,TRUE)
##' 
##' }
##'
##'
##'
##'
##' @export
##' @rdname pk_model
pkmodel <- function(ncmt=1,depot=FALSE,...) {
    stopifnot(ncmt %in% c(1,2))
    depot <- as.logical(depot)
    code <- paste0("modpk", ncmt, ifelse(depot, "po", ""))
    mod <- suppressMessages(mcode(code,get(code))) %>% update(...)
    return(mod)
}












