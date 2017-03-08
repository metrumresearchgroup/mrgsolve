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

##' Get the compartment number from a compartment name.
##'
##' @param x model object
##' @param ... passed along
##'
##' @examples
##' mod <- mrgsolve:::house()
##' mod %>% cmtn("CENT")
##' @export
setGeneric("cmtn", function(x,...) standardGeneric("cmtn"))

##' Print model code to the console.
##'
##' @param x model object
##' @param raw return the raw code
##' @param ... passed along
##' @return invisible NULL
##' @export
setGeneric("see", function(x,...) standardGeneric("see"))

##' Load the model shared object.
##'
##' @param x the model object
##' @param ... passed along
##' @export
setGeneric("loadso", function(x,...) standardGeneric("loadso"))
setGeneric("unloadso", function(x,...) standardGeneric("unloadso"))

##' Get the times at which the model will be evaluated.
##'
##' @name stime
##'
##' @param x object of class mrgmod
##' @param ... passed on
##' @return a sorted vector of unique times
##' @details
##' Simulation times include the sequence of times created from \code{start}, \code{end}, and \code{delta} and the vector of times
##' found in \code{add}.  Making \code{end} negative will omit any \code{start} / \code{end} / \code{delta} sequence.  Negative values are discarded from the result.
##' @export
##' @examples
##'
##' ## example("stime", package="mrgsolve")
##'
##' mod <- mrgsolve:::house(end=12, delta=2, add=c(11,13,15))
##'
##' stime(mod)
##' 
##' 
setGeneric("stime", function(x,...) standardGeneric("stime"))

##' Get model random effect variances and covariances.
##'
##' @param x model object
##' @param ... passed along
##'
##' @export
##' @rdname revar
setGeneric("revar", function(x,...) standardGeneric("revar"))

##' Return the code blocks from a model specification file.
##'
##' @param x model object or path to model specification file
##' @param ... passed along
##'
##' @examples
##' mod <- mrgsolve:::house()
##' mod %>% blocks
##' mod %>% blocks(PARAM,TABLE)
##'
##' @export
setGeneric("blocks", function(x,...) standardGeneric("blocks"))
setGeneric("project", function(x,...) standardGeneric("project"))
setGeneric("sodll", function(x,...) standardGeneric("sodll"))
setGeneric("ex", function(x,...) standardGeneric("ex"))


##' Event objects for simulating PK and other interventions.
##'
##' Events can either be specified when the model object is created (with
##' \code{mrgmod}) or by updating an existing model object (with \code{update}).
##'
##' @return Returns a user-defined data frame of events that should be suitable
##' for passing into \code{lsoda}.  If events are stored as a data frame,
##' \code{events} returns the data frame.  If events are stored as a function
##' that generates the data frame, \code{events} calls the function and passes
##' return back to the user.
##'
##' @param x mrgmodel object
##' @param ... passed on
##' @export events
setGeneric("events", function(x,...) standardGeneric("events"))


##' @details
##' \itemize{
##' \item Required input for creating events objects include \code{time} and \code{cmt}
##' \item If not supplied, \code{evid} is assumed to be 1.
##' \item If not supplied, \code{cmt}  is assumed to be 1.
##' \item If not supplied, \code{time} is assumed to be 0.
##' \item \code{ID} may be specified as a vector.
##' \item If replicate is \code{TRUE} (default), thenthe events regimen is replicated for each \code{ID}; otherwise, the number of
##' event rows must match the number of \code{ID}s entered
##' }
##' @return events object
##' @export
##' @rdname events
##' @examples
##' mod <- mrgsolve:::house()
##' mod <- mod %>% ev(amt=1000, time=0, cmt=1)
##' events(mod)
##'
##' loading <- ev(time=0, cmt=1, amt=1000)
##' maint <- ev(time=12, cmt=1, amt=500, ii=12, addl=10)
##' loading + maint
##'
##'
##' ev(ID=1:10, cmt=1, time=0, amt=100)
##'
##'
setGeneric("ev", function(x,...) standardGeneric("ev"))

##' @rdname events
##' @export
setGeneric("as.ev", function(x,...) standardGeneric("as.ev"))


##' Return the model object.
##'
##' @param x mrgsims object
##' @param ... passed along
##' @rdname mod
setGeneric("mod", function(x,...) standardGeneric("mod"))
