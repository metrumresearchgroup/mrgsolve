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



##' Simulate from a model object with quicker turnaround
##' 
##' Use the function when you would usually use \code{\link{mrgsim_d}}, 
##' but you need a quicker turnaround time.  The timing differences 
##' might be difficult to detect for a single simulation run
##' but could become appreciable with repeated simulation.  See
##' \code{details} for important differences in how \code{\link{mrgsim_q}}
##' is invoked compared to \code{\link{mrgsim}} and \code{\link{mrgsim_d}}.  
##' This function should always be used for benchmarking simulation time with
##' mrgsolve.
##' 
##' @param x a model object
##' @param data a simulation data set
##' @param recsort record sorting flag
##' @param stime a numeric vector of observation times; these observation
##' times will only be added to the output if there are no observation
##' records in \code{data}
##' @param skip_init_calc don't use \code{$MAIN} to calculate initial conditions
##' @param output output data type; if \code{NULL}, then an \code{mrgsims}
##' object is returned; if \code{"df"} then a data frame is returned
##' 
##' @details
##' 
##' This function does not support the piped simulation workflow.  All
##' arguments must be passed into the function except for \code{x}.  
##' 
##' A data set is required for this simulation workflow.  The 
##' data set can have only dosing records or doses with observations.
##' When the data set only includes doses, a single numeric vector of 
##' observation times should be passed in.  
##' 
##' This simulation workflow does not support \code{Req} (request) 
##' functionality.  All compartments and captured variables will 
##' always be returned in the simulation output.
##' 
##' This simulation workflow does not support carry-out functionality.
##' 
##' This simulation workflow does not accept arguments to be passed
##' to \code{\link[mrgsolve]{update}}. This must be done by a separate
##' call to \code{\link[mrgsolve]{update}}.
##' 
##' This simulation workflow does not support use of event objects.  If 
##' an event object is needed, it should be converted to a data set 
##' prior to the simulation run (see \code{as_data_set} or 
##' \code{\link{as.data.frame.ev}}.
##' 
##' This simulation workflow does not support idata sets or any 
##' feature enabled by idata set use.  Individual level parameters
##' should be joined onto the data set prior to simulation.  Otherwise
##' \code{\link{mrgsim_i}} or \code{\link{mrgsim_ei}} should be used.
##' 
##' By default, a mrgsims object is returned (as with \code{\link{mrgsim}}). 
##' Use the \code{matrix_return} argument to request a plain 
##' matrix of simulated data on return.
##' 
##' 
##' @examples
##' 
##' mod <- mrgsolve:::house()
##' 
##' data <- expand.ev(amt = c(100,300,1000))
##' 
##' out <- mrgsim_q(mod,data)
##' 
##' out
##' 
##' @seealso \code{\link{mrgsim}}, \code{\link{mrgsim_variants}}
##' @export
mrgsim_q <- function(x,
                     data,
                     recsort = 1,
                     stime = numeric(0),
                     output = NULL,
                     skip_init_calc = FALSE) {
  
  ## data
  if(!is.valid_data_set(data)) {
    data <- valid_data_set(data,x,x@verbose)
  } 
  
  tcol <- timename(data)
  tcol <- if_else(is.na(tcol), "time", tcol)
  
  param <- as.numeric(Param(x))
  init <-  as.numeric(Init(x))
  
  compartments <- Cmt(x)
  
  capt <- unname(x@capture)
  
  # Non-compartment names in capture
  if(any(is.element(capt,compartments))) {
    stop("Compartment names should not be used in $CAPTURE.", call.=FALSE)  
  }

  # First spot is the number of capture.items, followed by integer positions
  # Important to use the total length of x@capture
  capt_pos <- c(length(x@capture),(match(capt,x@capture)-1))
  
  # Big list of stuff to pass to DEVTRAN
  parin <- parin(x)
  parin$recsort <- recsort
  parin$stime <- stime
  parin$do_init_calc <- !skip_init_calc
  
  # already took intersect
  parin$request <- as.integer(seq_along(compartments)-1);
  
  out <- .Call(
    `_mrgsolve_MRGSIMQ`,
    parin,
    param,
    Pars(x),
    init,
    compartments,
    capt_pos,
    pointers(x),
    data,
    as.matrix(omat(x)),
    as.matrix(smat(x)),
    x@envir
  )
  
  cnames <- c("ID", tcol, compartments, capt)
  
  dimnames(out) <- list(NULL, cnames)
  
  if(!is.null(output)) {
    if(output=="df") {
      return(as.data.frame(out))  
    }
    if(output=="matrix") {
      return(out)  
    }
  }
  
  new("mrgsims",
      request=compartments,
      data=as.data.frame(out),
      outnames=capt,
      mod=x)
}
