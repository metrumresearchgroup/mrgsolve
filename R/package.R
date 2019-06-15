# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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


##' @title mrgsolve
##' @name mrgsolve
##' 
##'
##' @section Resources:
##' \itemize{
##' \item Main mrgsolve resource page: \href{https://mrgsolve.github.io}{https://mrgsolve.github.io}
##' \item User guide: \href{https://mrgsolve.github.io/user_guide}{https://mrgsolve.github.io/user_guide}
##' \item Vignettes: \href{https://mrgsolve.github.io/vignettes}{https://mrgsolve/github.io/vignettes}
##' }
##' 
##' @description
##' mrgsolve is an R package maintained under the auspices of 
##' Metrum Research Group that facilitates simulation from 
##' models based on systems of ordinary differential equations (ODE) 
##' that are typically employed for understanding pharmacokinetics, 
##' pharmacodynamics, and systems biology and pharmacology. mrgsolve 
##' consists of computer code written in the R and C++ languages, 
##' providing an interface to the DLSODA differential equation solver 
##' (written in FORTRAN) provided through ODEPACK -
##' A Systematized Collection of ODE Solvers.
##' 
##' @rdname mrgsolve_package
##' @docType package
##' @useDynLib mrgsolve, .registration=TRUE
##' @aliases mrgsolve
##' @examples
##'
##' ## example("mrgsolve")
##'
##' mod <- mrgsolve:::house(delta=0.1)  %>% param(CL=0.5)
##'
##' events <-  ev(amt=1000, cmt=1, addl=5, ii=24)
##' 
##' events
##' 
##' mod
##'
##' see(mod)
##' 
##' stime(mod)
##'
##' param(mod)
##' init(mod)
##'
##' out <- mod %>% ev(events) %>% mrgsim(end=168)
##'
##' out
##' 
##' head(out)
##' tail(out)
##' dim(out)
##'
##' plot(out, GUT+CP~.)
##'
##' sims <- as.data.frame(out)
##'
##' t72 <- dplyr::filter(sims, time==72)
##' str(t72)
##' 
##' idata <- data.frame(ID=c(1,2,3), CL=c(0.5,1,2),VC=12)
##' out <- mod %>% ev(events) %>% mrgsim(end=168, idata=idata, req="")
##' plot(out)
##'
##' out <- mod %>% ev(events) %>% mrgsim(carry.out="amt,evid,cmt,CL")
##' head(out)
##'
##' out <- 
##'   mod %>% 
##'   ev() %>% 
##'   knobs(CL=c(0.5, 1,2), amt=c(100,300,1000), cmt=1,end=48)
##' 
##' plot(out, CP~., scales="same")
##' plot(out, RESP+CP~time|amt,groups=CL)
##'
##'
##' ev1 <- ev(amt=500, cmt=2,rate=10)
##' ev2 <- ev(amt=100, cmt=1, time=54, ii=8, addl=10)
##' events <- ev1+ev2
##' events
##'
##' out <- mod %>% ev(ev1+ev2) %>% mrgsim(end=180, req="")
##' plot(out)
##'
##'
##'
##' ## "Condensed" data set
##' data(extran1)
##' extran1
##'
##' out <- mod %>% data_set(extran1) %>% mrgsim(end=200)
##'
##' plot(out,CP~time|factor(ID))
##'
##'
##' ## idata
##' data(exidata)
##' exidata
##'
##' out <- 
##'   mod %>% 
##'   ev(amt=1000, cmt=1) %>% 
##'   idata_set(exidata) %>%  
##'   mrgsim(end=72)
##'
##' plot(out, CP~., as="log10")
##'
##'
##' # Internal model library
##' \dontrun{
##' mod <- mread("irm1", modlib())
##' 
##' mod
##' 
##' mod %>% ev(amt=300, ii=12, addl=3) %>% mrgsim
##' 
##' }
##'
NULL

##' About the ODEPACK differential equation solver used by mrgsolve
##'
##' @name aboutsolver
##' @rdname aboutsolver
##'
##' @section DLSODA:
##' \preformatted{
##'C----------------------------------------------------------------------
##'C This is the 12 November 2003 version of
##'C DLSODA: Livermore Solver for Ordinary Differential Equations, with
##'C         Automatic method switching for stiff and nonstiff problems.
##'C
##'C This version is in double precision.
##'C
##'C DLSODA solves the initial value problem for stiff or nonstiff
##'C systems of first order ODEs,
##'C     dy/dt = f(t,y) ,  or, in component form,
##'C     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) (i = 1,...,NEQ).
##'C
##'C This a variant version of the DLSODE package.
##'C It switches automatically between stiff and nonstiff methods.
##'C This means that the user does not have to determine whether the
##'C problem is stiff or not, and the solver will automatically choose the
##'C appropriate method.  It always starts with the nonstiff method.
##'C
##'C Authors:       Alan C. Hindmarsh
##'C                Center for Applied Scientific Computing, L-561
##'C                Lawrence Livermore National Laboratory
##'C                Livermore, CA 94551
##'C and
##'C                Linda R. Petzold
##'C                Univ. of California at Santa Barbara
##'C                Dept. of Computer Science
##'C                Santa Barbara, CA 93106
##'C
##'C References:
##'C 1.  Alan C. Hindmarsh,  ODEPACK, A Systematized Collection of ODE
##'C     Solvers, in Scientific Computing, R. S. Stepleman et al. (Eds.),
##'C     North-Holland, Amsterdam, 1983, pp. 55-64.
##'C 2.  Linda R. Petzold, Automatic Selection of Methods for Solving
##'C     Stiff and Nonstiff Systems of Ordinary Differential Equations,
##'C     Siam J. Sci. Stat. Comput. 4 (1983), pp. 136-148.
##'C----------------------------------------------------------------------
##' }
NULL

##' Optional inputs for DLSODA
##' 
##' These are settings for the differential equation 
##' solver (\code{DLSODA}) that can be accessed via
##' the R interface.  The code listing below is taken directly
##' from the \code{DLSODA} source code.  
##'
##' @name solversettings
##' @rdname solversettings
##' @seealso \code{\link{aboutsolver}}, \code{\link[mrgsolve]{update}}
##' 
##' @details
##' 
##' The following items can be set
##' 
##' \itemize{
##' \item \code{hmax} (\code{HMAX} below); decrease \code{hmax} when 
##' you want to limit how big of a step the solver can take when 
##' integrating from one time to the next time. However be aware
##' that smaller \code{hmax} will result in longer run times.
##' \item \code{hmin} (\code{HMIN} below); don't fiddle with this
##' unless you know what you're doing.  
##' \item \code{ixpr} (\code{IXPR} below)
##' \item \code{maxsteps} (\code{MXSTEP} below); increase this 
##' number when the solver has a long interval between 
##' two integration times (e.g. when observation records are 
##' far apart). 
##' \item \code{mxhnil} (\code{MXHNIL below}); don't usually 
##' modify this one
##' \item \code{atol} - the absolute solver tolerance; decrease
##' this number (e.g. to 1E-10 or 1E-20 or 1E-50) when the 
##' value in a compartment can get extremely small; without this 
##' extra (lower) tolerance, the value can get so low that the number
##' can randomly become negative.  However be aware that more precision
##' here will result in longer run times. 
##' \item \code{rtol} - the relative solver tolerances; decrease this 
##' number when you want a more precise solution.  However be aware 
##' that more precision here will result in longer run times.
##' }
##' 
##' 
##' 
##'
##' @section Solver Settings:
##' \preformatted{
##'C-----------------------------------------------------------------------
##'C Optional Inputs.
##'C
##'C The following is a list of the optional inputs provided for in the
##'C call sequence.  (See also Part 2.)  For each such input variable,
##'C this table lists its name as used in this documentation, its
##'C location in the call sequence, its meaning, and the default value.
##'C The use of any of these inputs requires IOPT = 1, and in that
##'C case all of these inputs are examined.  A value of zero for any
##'C of these optional inputs will cause the default value to be used.
##'C Thus to use a subset of the optional inputs, simply preload
##'C locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, and
##'C then set those of interest to nonzero values.
##'C
##'C Name    Location      Meaning and Default Value
##'C
##'C
##'C HMAX    RWORK(6)  the maximum absolute step size allowed.
##'C                   The default value is infinite.
##'C
##'C HMIN    RWORK(7)  the minimum absolute step size allowed.
##'C                   The default value is 0.  (This lower bound is not
##'C                   enforced on the final step before reaching TCRIT
##'C                   when ITASK = 4 or 5.)
##'C
##'C IXPR    IWORK(5)  flag to generate extra printing at method switches.
##'C                   IXPR = 0 means no extra printing (the default).
##'C                   IXPR = 1 means print data on each switch.
##'C                   T, H, and NST will be printed on the same logical
##'C                   unit as used for error messages.
##'C
##'C MXSTEP  IWORK(6)  maximum number of (internally defined) steps
##'C                   allowed during one call to the solver.
##'C                   The default value is 500.
##'C
##'C MXHNIL  IWORK(7)  maximum number of messages printed (per problem)
##'C                   warning that T + H = T on a step (H = step size).
##'C                   This must be positive to result in a non-default
##'C                   value.  The default value is 10.
##'C
##'C-----------------------------------------------------------------------
##' }
NULL

##' Reserved words
##'
##' @name reserved
##' @details
##' Note: this function is not exported; you must go into the 
##' \code{mrgsolve} namespace by using the \code{mrgsolve:::} prefix.
##' @examples
##' mrgsolve:::reserved()
##'
reserved <- function() {
  cat(paste(" ", Reserved), sep="\n")
}

examples <- function(...) {
  example("mrgsolve", package="mrgsolve",...)
  example("knobs", package="mrgsolve",...)
  example("param", package="mrgsolve",...)
  example("init", package="mrgsolve",...)
  example("mrgsim", package="mrgsolve",...)
  example("mrgsims", package="mrgsolve",...)
  example("mrgsolve_example", package="mrgsolve",...)
  example("mrgsolve_utils", package="mrgsolve",...)
  example("plot_mrgsims", package="mrgsolve",...)
}

tests <- function() {
  if(!requireNamespace("testthat", quietly=TRUE)) stop("testthat not available")
  testthat::test_package("mrgsolve")
}

models <- function() {
  file.path(path.package("mrgsolve"), "models")
}

.onLoad <- function(libname, pkgname) {
  GLOBALS[["version"]] <- utils::packageVersion("mrgsolve")
}

# .onAttach <- function(libname,pkgname) {
#   base::packageStartupMessage("mrgsolve: Community Edition")
# }


