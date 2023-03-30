# Copyright (C) 2013 - 2022  Metrum Research Group
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


#' @title mrgsolve
#' @name mrgsolve
#' 
#'
#' @section Resources:
#' 
#' - Main mrgsolve resource page: [https://mrgsolve.org](https://mrgsolve.org)
#' - User guide: [https://mrgsolve.org/user_guide/](https://mrgsolve.org/user_guide/)
#' - Package documentation and vignettes: [https://mrgsolve.org/docs/](https://mrgsolve.org/docs/)
#' 
#' @section Package-wide options:
#' 
#' - `mrgolve.project`: sets the default project director ([mread()])
#' - `mrgsolve.soloc`: sets the default package build directory ([mread()])
#' - `mrgsolve_mread_quiet`: don't print messages during [mread()]
#' - `mrgsolve.update.strict`: this option has been deprecated; use the `strict`
#'   argument to [mrgsolve::update()] instead
#' 
#' 
#' @description
#' mrgsolve is an R package maintained under the auspices of 
#' Metrum Research Group that facilitates simulation from 
#' models based on systems of ordinary differential equations (ODE) 
#' that are typically employed for understanding pharmacokinetics, 
#' pharmacodynamics, and systems biology and pharmacology. mrgsolve 
#' consists of computer code written in the R and C++ languages, 
#' providing an interface to a C++ translation of the lsoda differential 
#' equation solver. See [aboutsolver] for more information.
#' 
#' @examples
#'
#' ## example("mrgsolve")
#'
#' mod <- mrgsolve::house(delta=0.1)  %>% param(CL=0.5)
#'
#' events <-  ev(amt=1000, cmt=1, addl=5, ii=24)
#' 
#' events
#' 
#' mod
#'
#' see(mod)
#' 
#' \dontrun{
#' stime(mod)
#' }
#' param(mod)
#' 
#' init(mod)
#'
#' out <- mod %>% ev(events) %>% mrgsim(end=168)
#' 
#' head(out)
#' tail(out)
#' dim(out)
#'
#' plot(out, GUT+CP~.)
#'
#' sims <- as.data.frame(out)
#'
#' t72 <- dplyr::filter(sims, time==72)
#' 
#' str(t72)
#' 
#' idata <- data.frame(ID=c(1,2,3), CL=c(0.5,1,2),VC=12)
#' out <- mod %>% ev(events) %>% mrgsim(end=168, idata=idata, req="")
#' plot(out)
#'
#' out <- mod %>% ev(events) %>% mrgsim(carry_out="amt,evid,cmt,CL")
#' head(out)
#' 
#' ev1 <- ev(amt=500, cmt=2,rate=10)
#' ev2 <- ev(amt=100, cmt=1, time=54, ii=8, addl=10)
#' events <- c(ev1+ev2)
#' events
#'
#' out <- mod %>% ev(events) %>% mrgsim(end=180, req="")
#' plot(out)
#'
#'
#' ## "Condensed" data set
#' data(extran1)
#' extran1
#'
#' out <- mod %>% data_set(extran1) %>% mrgsim(end=200)
#'
#' plot(out,CP~time|factor(ID))
#'
#'
#' ## idata
#' data(exidata)
#'
#' out <- 
#'   mod %>% 
#'   ev(amt=1000, cmt=1) %>% 
#'   idata_set(exidata) %>%  
#'   mrgsim(end=72)
#'
#' plot(out, CP~., as="log10")
#'
#'
#' # Internal model library
#' \dontrun{
#' mod <- mread("irm1", modlib())
#' 
#' mod
#' 
#' x <- mod %>% ev(amt=300, ii=12, addl=3) %>% mrgsim
#' 
#' }
#' 
#' @rdname mrgsolve_package
#' @docType package
#' @useDynLib mrgsolve, .registration=TRUE
#' @aliases mrgsolve
#' @md
#' 
NULL

#' About the lsoda differential equation solver used by mrgsolve
#'
#' The differential equation solver is a C++ translation of DLSODA from 
#' ODEPACK.  The C++ translation was created by Dilawar Singh and hosted 
#' here [https://github.com/dilawar/libsoda-cxx/](https://github.com/dilawar/libsoda-cxx/). 
#' As we understand the history of the code, Heng Li was also involved in early 
#' versions of the code written in C.  There was a potentially-related 
#' project hosted here [https://github.com/sdwfrost/liblsoda/](https://github.com/sdwfrost/liblsoda/).
#' 
#' The C++ translation by Dilawar Singh contains functions that appear to be 
#' based on BLAS and LAPACK routines.  These functions have been renamed to be
#' distinct from the respective BLAS and LAPACK function names.  References
#' are given in the section below.   
#' 
#' @section History: 
#' The following history was recorded in the source code published by 
#' Dilawar Singh:
#' 
#' \preformatted{
#' /*
#' * HISTORY:
#' * This is a CPP version of the LSODA library for integration into MOOSE
#' somulator.
#' * The original was aquired from
#' * http://www.ccl.net/cca/software/SOURCES/C/kinetics2/index.shtml and modified
#' by
#' * Heng Li <lh3lh3@gmail.com>. Heng merged several C files into one and added a
#' * simpler interface. [Available
#' here](http://lh3lh3.users.sourceforge.net/download/lsoda.c)
#' 
#' * The original source code came with no license or copyright
#' * information. Heng Li released his modification under the MIT/X11 license. I
#' * maintain the same license. I have removed quite a lot of text/comments from
#' * this library. Please refer to the standard documentation.
#' *
#' * Contact: Dilawar Singh <dilawars@ncbs.res.in>
#' */
#' }
#' 
#' @references
#' 
#' 1. LAPACK: [https://netlib.org/lapack/](https://netlib.org/lapack/)
#' 1. BLAS: [https://netlib.org/blas/](https://netlib.org/blas/)
#' 
#' 
#' @name aboutsolver
#' @rdname aboutsolver
#' @md
#' 
NULL

#' Optional inputs for lsoda
#' 
#' These are settings for the differential equation 
#' solver (\code{lsoda}) that can be accessed via
#' the R interface.  The code listing below is taken directly
#' from the \code{lsoda} source code.  
#'
#' @details
#' 
#' The following items can be set
#' 
#' \itemize{
#' \item \code{hmax} (\code{HMAX} below); decrease \code{hmax} when 
#' you want to limit how big of a step the solver can take when 
#' integrating from one time to the next time. However be aware
#' that smaller \code{hmax} will result in longer run times.
#' \item \code{hmin} (\code{HMIN} below); don't fiddle with this
#' unless you know what you're doing.  
#' \item \code{ixpr} (\code{IXPR} below)
#' \item \code{maxsteps} (\code{MXSTEP} below); increase this 
#' number when the solver has a long interval between 
#' two integration times (e.g. when observation records are 
#' far apart). 
#' \item \code{mxhnil} (\code{MXHNIL below}); don't usually 
#' modify this one
#' \item \code{atol} - the absolute solver tolerance; decrease
#' this number (e.g. to 1E-10 or 1E-20 or 1E-50) when the 
#' value in a compartment can get extremely small; without this 
#' extra (lower) tolerance, the value can get so low that the number
#' can randomly become negative.  However be aware that more precision
#' here will result in longer run times. 
#' \item \code{rtol} - the relative solver tolerances; decrease this 
#' number when you want a more precise solution.  However be aware 
#' that more precision here will result in longer run times.
#' }
#' 
#' @name solversettings
#' @rdname solversettings
#' @seealso \code{\link{aboutsolver}}, \code{\link[mrgsolve]{update}}
#' 
NULL

#' Reserved words
#'
#' @name reserved
#' @details
#' Note: this function is not exported; you must go into the 
#' \code{mrgsolve} namespace by using the \code{mrgsolve:::} prefix.
#' @examples
#' mrgsolve:::reserved()
#'
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
