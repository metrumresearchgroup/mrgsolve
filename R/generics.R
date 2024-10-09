# Copyright (C) 2013 - 2024  Metrum Research Group
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

#' Get the compartment number from a compartment name
#'
#' @param x model object.
#' @param ... not used.
#'
#' @examples
#' mod <- mrgsolve::house()
#' cmtn(mod, "CENT")
#' 
#' @md
#' @export
setGeneric("cmtn", function(x,...) standardGeneric("cmtn"))

#' Print model code to the console
#' 
#' This is a simple way to display the model code on the R console using 
#' the model object. The `raw` argument will return the model code as a 
#' character vector.
#' 
#' @param x model object. 
#' @param raw return the raw code.
#' @param ... not used.
#' @return `NULL` is returned invisibly when `raw` is `FALSE`; when `raw` is 
#' set to `TRUE`, the model code is returned as a character vector.
#' @md
#' @export
setGeneric("see", function(x,...) standardGeneric("see"))

#' Load the model shared object
#' 
#' Once the model is compiled, the model object can be used to re-load
#' the model shared object (the compiled code underlying the mode) when 
#' the simulation is to be done in a different R process. 
#'
#' @param x a model object.
#' @param ... not used.
#' 
#' @return The model object (invisibly).
#' 
#' 
#' @details
#' The `loadso` function most frequently needs to be used when parallelizing
#' simulations across worker nodes.  The model can be run after calling 
#' `loadso`, without requiring that it is re-compiled on worker nodes. It is 
#' likely required that the model is built (and the shared object stored) in 
#' a local directory off of the working R directory (see the second example).
#' 
#' @examples
#' \dontrun{ 
#'   mod <- mread("pk1", modlib())
#'   loadso(mod)
#'   
#'   mod2 <- mread("pk2", modlib(), soloc = "build")
#'   loadso(mod2)
#' }
#' 
#' @md
#' @export
loadso <- function(x,...) UseMethod("loadso")
unloadso <- function(x,...) UseMethod("unloadso")

#' Get the times at which the model will be evaluated
#'
#' @name stime
#'
#' @param x a model object or a mrgsims object.
#' @param ... not used.
#' 
#' @return A sorted vector of unique simulation times from the time grid in 
#' the model object.
#' 
#' @details
#' Simulation times include the sequence of times created from 
#' `start`, `end`, and `delta` and the vector of times found in `add`.  
#' 
#' Making `end` negative (e.g. -1) will omit the `start-end-delta`
#' sequence.  Negative values are discarded from the result.
#' 
#' @examples
#' ## example("stime", package="mrgsolve")
#'
#' mod <- mrgsolve::house(end = 12, delta = 2, add = c(11,13,15))
#'
#' stime(mod)
#' 
#' update(mod, end = -1) %>% stime()
#' 
#' @md
#' @export
setGeneric("stime", function(x,...) standardGeneric("stime"))

#' Get model random effect variances and covariances
#' 
#' Use this function to extract both OMEGA and SIGMA matrices from a 
#' model object. Typical use is for display on the R console.
#'
#' @param x model object.
#' @param ... passed along.
#' 
#' @return A named list containing `omega` and `sigma` matrices.
#' 
#' @examples
#' mod <- mrgsolve::house()
#' revar(mod)
#' 
#' @rdname revar
#' @md
#' @export
setGeneric("revar", function(x,...) standardGeneric("revar"))

##' Return the code blocks from a model specification file
##'
##' @param x model object or path to model specification file
##' @param ... passed along
##'
##' @examples
##' mod <- mrgsolve::house()
##' mod %>% blocks
##' mod %>% blocks(PARAM,TABLE)
##'
##' @export
setGeneric("blocks", function(x,...) standardGeneric("blocks"))
setGeneric("project", function(x,...) standardGeneric("project"))
setGeneric("sodll", function(x,...) standardGeneric("sodll"))
#setGeneric("ex", function(x,...) standardGeneric("ex"))


##' Return the model object
##'
##' @param x mrgsims object
##' @param ... passed along
##' 
##' 
##' @rdname mod
##' @keywords internal
setGeneric("mod", function(x,...) standardGeneric("mod"))

setGeneric("nrow")
