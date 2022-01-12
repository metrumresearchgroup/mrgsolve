# Copyright (C) 2013 - 2020  Metrum Research Group
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
##' @param output output data type; if \code{mrgsims}, then the default output
##' object is returned; if \code{"df"} then a data frame is returned
##' @param simcall not used; only the default value of 0 is allowed
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
##' Use the \code{output="df"} argument to request a plain 
##' data.frame of simulated data on return.
##' 
##' @return
##' By default, an object of class `mrgsims`. Use `output = "df"` to return 
##' a data frame.
##' 
##' @examples
##' 
##' mod <- mrgsolve::house()
##' 
##' data <- expand.ev(amt = c(100, 300, 1000))
##' 
##' out <- mrgsim_q(mod, data)
##' 
##' out
##' 
##' @seealso \code{\link{mrgsim}}, \code{\link{mrgsim_variants}}, \code{\link{qsim}}
##' @export
mrgsim_q <- function(x,
                     data,
                     recsort = 1,
                     stime = numeric(0),
                     output = "mrgsims",
                     skip_init_calc = FALSE, 
                     simcall = 0) {
  
  if(!is.mrgmod(x)) mod_first()
  
  ## data
  if(is.ev(data)) {
    data <- as.data.frame.ev(data, add_ID = 1)  
  }
  
  if(!is.valid_data_set(data)) {
    data <- valid_data_set(data,x,x@verbose)
  } 
  
  tcol <- timename(data)
  if(is.na(tcol)) tcol <- "time"

  # Big list of stuff to pass to DEVTRAN
  parin <- parin(x)
  parin$recsort <- recsort
  parin$stime <- stime
  parin$do_init_calc <- !skip_init_calc
  parin$request <- Cmti(x)-1L

  if(simcall!=0) {
    if(simcall==1) {
      wstop("the interface with simcall=1 is no longer available; please use simcall=0 instead.")
    }
    wstop("simcall values other than 0 are prohibited.")
  }
  
  if(length(stime) == 0) {
    parin[["tgridmatrix"]] <- matrix(stime(x),ncol=1)
  } else {
    parin[["tgridmatrix"]] <- matrix(stime,ncol=1)
  }
  
  parin[["whichtg"]] <- integer(0)
  parin[["carry_data"]] <- character(0)
  parin[["carry_idata"]] <- character(0)
  parin[["carry_tran"]] <- character(0)
  parin[["obsonly"]] <- FALSE
  parin[["filbak"]] <- TRUE
  parin[["tad"]] <- FALSE
  parin[["nocb"]] <- TRUE
  parin[["obsaug"]] <- FALSE
    
  out <- .Call(
    `_mrgsolve_DEVTRAN`,
    parin,
    as.numeric(Param(x)),
    Pars(x),
    as.numeric(Init(x)),
    Cmt(x),
    CAPTUREI(x),
    pointers(x),
    data,null_idata,
    as.matrix(omat(x)),
    as.matrix(smat(x)),
    x@envir, 
    PACKAGE = "mrgsolve"
  )[["data"]]
  
  names(out) <- c("ID", tcol, x@cmtL, x@capL)
  
  if(output=="df") {
    return(out)  
  }
  
  if(output=="matrix") {
    return(as.matrix(out))  
  }
  
  new(
    "mrgsims",
    request = x@cmtL,
    data = out,
    outnames = x@capL,
    mod = x
  )
}
