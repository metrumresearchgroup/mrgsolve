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



##' Select and modify a idata set for simulation.
##'
##' @param x model object
##' @param data a data set coercable to data.frame
##' @param object character name of an object existing in \code{$ENV} to use for the data set
##' @param subset passed to \code{dplyr::filter_}
##' @param select passed to \code{dplyr::select_}
##' @param ... passed along
##' 
##' @details
##' The \code{idata_set} is a data.frame that specifies individual-level 
##' data for the problem.  An  \code{ID} column is required and there 
##' can be no more than one row in the data frame for each individual.  
##' 
##' In most cases, the columns in the `idata_set` have the same names
##' as parameters in the \code{\link{param}} list.  When this is the case, 
##' the parameter set is updated as the simulation proceeds once at the 
##' start of each individual.  The `idata_set` can also be used to 
##' set initial conditions for each individual: for a compartment called
##' \code{CMT}, make a column in \code{idata_set} called \code{CMT_0} and 
##' make the value the desired initial value for that compartment.  Note that 
##' this initial condition will be over-ridden if you also set the \code{CMT_0} 
##' in \code{$MAIN}.
##' 
##' The most common application of \code{idata_set} is to specify a population
##' or bach of simulations to do.  We commonly use \code{idata_set} with an 
##' event object (see \code{\link{ev}}).  In that case, the event gets applied
##' to each individual in the \code{\link{idata_set}}.  
##' 
##' It is also possible to provide both a \code{data_set} and a \code{idata_set}.  
##' In this case, the \code{idata_set} is used as a parameter lookup for \code{IDs}
##' found in the \code{data_set}.  Remember in this case, it is the \code{data_set}
##' (not the \code{idata_set}) that determines the number of individuals in the
##' simulation.
##' 
##' @examples
##' 
##' mod <- mrgsolve:::house()
##' 
##' data(exidata)
##' 
##' exidata
##' 
##' mod %>% idata_set(exidata, ID <= 2) %>% mrgsim %>% plot
##' 
##' mod %>% idata_set(exidata) %>% mrgsim
##' 
##' mod %>% mrgsim(idata=exidata) 
##' 
##' @seealso \code{\link{data_set}}, \code{\link{ev}}
##' 
##' @export
setGeneric("idata_set", function(x,data,...) standardGeneric("idata_set"))

##' @rdname idata_set
##' @export
setMethod("idata_set", c("mrgmod", "data.frame"), 
          function(x,data,subset=TRUE,select=TRUE,object=NULL,...) {
            
  if(exists("idata", x@args)) stop("idata has already been set.")
  if(!missing(subset)) data <- filter_(data,.dots=lazy(subset))
  if(!missing(select)) data <- select_(data,.dots=lazy(select))
  if(nrow(data) ==0) {
    stop("Zero rows in idata after filtering.", call.=FALSE)
  }
  if(is.character(object)) {
    data <- data_hooks(data,object,x@envir,param(x),...) 
  }
  x@args <- merge(x@args,list(idata=as.data.frame(data)), open=TRUE)
  return(x)
  
})

##' @rdname idata_set
##' @export
setMethod("idata_set",c("mrgmod", "ANY"), function(x,data,...) {
  return(idata_set(x,as.data.frame(data),...))
})

##' @rdname idata_set
##' @export
setMethod("idata_set",c("mrgmod", "missing"), function(x,object,...) {
  object <- data_hooks(object=object,envir=x@envir,param=param(x),...)
  return(idata_set(x,object,...))
})

