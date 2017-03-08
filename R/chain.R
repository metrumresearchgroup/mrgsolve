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


##' Functions for chaining commands together.
##'
##' Use these functions with chaining commands togehter with the %>%
##' operator.
##'
##' @name chain
##' 
##' @details
##' Other functions that may be used in the chain of commands include: 
##' \code{\link{param}}, \code{\link{init}}, \code{\link[mrgsolve]{update}},
##' \code{\link{ev}}.
##' or any other function that will take the output of the preceeding command as it's first argument.
##'
##' @examples
##'
##' mod <- mrgsolve:::house()
##'
##' data(exidata)
##' data(exTheoph)
##'
##' out <- mod %>% data_set(exTheoph) %>% mrgsim()
##' out <- mod %>% carry_out(evid) %>% ev(amt=100, cmt=1) %>% mrgsim()
##' out <- mod %>% Req(CP,RESP) %>% mrgsim()
NULL

# SEE ALSO: data_set and idata_set

##' Request simulated output. 
##' 
##' Use this function to select, by name, either compartments or derived variables 
##' that have been captured (see \code{\link{CAPTURE}}).
##'
##' @param x model object
##' @param ... unquoted names of compartments or tabled items
##' 
##' 
##' There is also a \code{Req} argument to \code{\link{mrgsim}} that can 
##' be set to accomplish the same thing as a call to \code{Req} in 
##' the pipeline.
##' 
##' Note the difference between \code{req} and \code{Req}: the former only 
##' selects compartments to appear in output while the latter selects both 
##' compartments and captured items.  Also, when there are items are explicitly
##' listed in \code{Req}, all other compartments or captured items not listed
##' there are ignored.  But when compartments are selected with \code{req}
##' all of the captured items are returned.  Remember that \code{req} is 
##' strictly for compartments.
##' 
##' @examples
##' mod <- mrgsolve:::house()
##'
##' mod %>% Req(CP,RESP) %>% ev(amt=1000) %>%  mrgsim
##'
##' @export
setGeneric("Req", function(x,...) standardGeneric("Req"))

##' @export
##' @rdname Req
setMethod("Req", "mrgmod", function(x,...) {
  
  x@args <- merge(x@args, list(Request=as_character_args(match.call()[-1])), open=TRUE)
  return(x)
})

##' @rdname Req
##' @export
setGeneric("req", function(x,...) standardGeneric("req"))

##' @export
##' @rdname Req
setMethod("req", "mrgmod", function(x,...) {
  x@args <- merge(x@args, list(request=as.character(match.call()[-1])), open=TRUE)
  return(x)
})

##' Select items to carry into simulated output. 
##' 
##' When items named in this function are found in the input data set (either 
##' \code{\link{data_set}} or \code{\link{idata_set}}), they are copied
##' into the simulated output.  Special items like \code{evid} or \code{amt} or
##' the like are not copied from the data set per se, but they are copied from
##' \code{datarecord} objects that are created during the simulation.
##'
##' @param x model object
##' @param ... passed along
##' 
##' @details
##' There is also a \code{carry.out} argument to \code{\link{mrgsim}} that can 
##' be set to accomplish the same thing as a call to \code{carry_out} in 
##' the pipeline.
##' 
##' \code{carry.out} and \code{carry_out}.  Using the underscore version is now preferred.
##' @export
carry_out <- function(x,...) {
  x@args <- merge(x@args, list(carry.out=as_character_args(match.call()[-1])), open=TRUE)
  return(x)
}

##' @export
##' @rdname carry_out
carry.out <- function(x,...) {
  x@args <- merge(x@args, list(carry.out=as_character_args(match.call()[-1])), open=TRUE)
  return(x)
}

##' Rescale time in the simulated output.
##'
##' @param x model object
##' @param value value by which time will be scaled
##' @param ... passed along
##' 
##' @details
##' There is also a \code{tscale} argument to \code{\link{mrgsim}} that can 
##' be set to accomplish the same thing as a call to \code{tscale} in 
##' the pipeline.
##' 
##' @examples
##' # The model is in hours:
##' mod <- mrgsolve:::house()
##' 
##' # The output is in days:
##' mod %>% tscale(1/24) %>% mrgsim
##' 
##' @export
tscale <- function(x,value=1,...) {
  x@args <- merge(x@args, list(tscale=value), open=TRUE)
  return(x)
}



##' Collect only observations in the simulated output.
##'
##' @param x model object
##' @param value the value for \code{obsonly}
##' @param ... passed along
##' 
##' @details
##' There is also a \code{obsonly} argument to \code{\link{mrgsim}} that can 
##' be set to accomplish the same thing as a call to \code{obsonly} in 
##' the pipeline.
##' 
##' @export
obsonly <- function(x,value=TRUE,...) {
  x@args <- merge(x@args, list(obsonly=value), open=TRUE)
  return(x)
}
##' Augment observations in the simulated output.
##'
##' @param x model object
##' @param value the value for \code{obsaug}
##' @param ... passed along
##' There is also a \code{obsaug} argument to \code{\link{mrgsim}} that can 
##' be set to accomplish the same thing as a call to \code{obsaug} in 
##' the pipeline.
##' @export
obsaug <- function(x,value=TRUE,...) {
  x@args <- merge(x@args, list(obsaug=value), open=TRUE)
  x
}


##' Set observation designs for the simulation.
##'
##' This function also allows you to assign different designs to different
##' groups or individuals in a population.
##'
##' @param x model object
##' @param descol the \code{idata} column name (\code{character}) for design assignment
##' @param deslist a list of \code{tgrid} or \code{tgrids} objects or \code{numeric} vector to be used in place of ...
##' @param ... not used
##' 
##' @details
##' This setup requires the use of an \code{idata_set}, with individual-level data
##' passed in one \code{ID} per row.  For each \code{ID}, specify a grouping variable
##' in \code{idata} (\code{descol}).  For each unique value of the grouping variable, 
##' make one \code{\link{tgrid}} object and pass them in order as \code{...} or 
##' form them into a list and pass as \code{deslist}.
##' 
##' You must assign the \code{idata_set} before assigning the designs in the 
##' command chain (see the example below).
##'
##' @export
##' 
##' @examples 
##' 
##' peak <- tgrid(0,6,0.1)
##' sparse <- tgrid(0,24,6)
##' 
##' des1 <- c(peak,sparse)
##' des2 <- tgrid(0,72,4)
##' 
##' 
##' data <- expand.ev(ID = 1:10, amt=c(100,300))
##' data$GRP <- data$amt/100
##' 
##' idata <- data[,c("ID", "amt")]
##' 
##' mod <- mrgsolve:::house()
##' 
##' mod %>%
##'   omat(dmat(1,1,1,1)) %>%
##'   carry_out(GRP) %>%
##'   idata_set(idata) %>%
##'   design(list(des1, des2),"amt") %>%
##'   data_set(data) %>%
##'   mrgsim %>% 
##'   plot(RESP~time|GRP)
design <- function(x, deslist=list(), descol = character(0),...) {
  
  if(missing(descol)) {
    descol <- attr(deslist,"descol") 
    if(is.null(descol)) {
      stop("please provide a value for descol",call.=FALSE)
    }
  }
  
  descol <- as.character(substitute(descol))
  
  stopifnot(length(descol) <= 1)
  
  deslist <- deslist[unlist(lapply(deslist,inherits,c("tgrid", "tgrids", "numeric")),use.names=FALSE)]
  
  if(length(deslist) ==0) stop("No valid tgrid objects found.")

  if(length(descol) ==1) {
    if(!exists("idata", x@args)) {
      stop("Please set idata before specifying designs.")
    }
    if(!exists(descol, x@args$idata)) {
      stop(paste0("Column ", descol, " does not exist in idata."))
    }
    x@args$idata[,descol] <- as.integer(as.factor(x@args$idata[,descol]))
  } else {
    if(length(deslist) > 1) {
      warning("Multiple designs specified but no idata key; only the first design will be used.",call.=FALSE)
    }
  }
  
  x@args <- merge(x@args, list(descol=descol, deslist=deslist),open=TRUE)
  
  x

}







