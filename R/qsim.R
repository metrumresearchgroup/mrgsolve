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

##' DEPRECATED: A quick simulation function
##' 
##' @param x model object
##' @param e event object
##' @param idata individual data set
##' @param req compartments to request
##' @param tgrid \code{tgrid} object; used if \code{e} is an \code{ev} object
##' @param skip_init_calc not used
##' @param ... passed to \code{\link{qsim}}
##' 
##' 
##' @export
##' @keywords internal
qsim <- function(x,e=NULL,idata=NULL,req=NULL,tgrid=NULL,
                 skip_init_calc = FALSE) {
  stop(shQuote("mrgsim"), " is deprecated.")
}

##' @rdname qsim
##' @export
##' @keywords internal
qsim_df <- function(...) {
  .Deprecated("mrgsim")
}

##' DEPRECATED: Create a matrix of events for simulation
##' 
##' This function is for use with \code{\link{qsim}} only.
##'
##' @param x an events object
##' @param times object that can be coerced to numeric with \code{\link{stime}}
##' @param c_indexing if \code{TRUE}, compartment numbers will be decremented by 1
##' @export
##' @keywords internal
recmatrix <- function(x, times, c_indexing=TRUE) {
  stop(shQuote("recmatrix"), " is deprecated.")
}

##' DEPRECATED: Create a matrix of events and observations for simulation
##' 
##' This function is to be used with \code{\link{qsim}} only.
##' 
##' @param e an event object
##' @param times numeric vector of observation times or a 
##' \code{tgrid} object
##' 
##' 
##' @export
##' @keywords internal
data_qsim <- function(e, times) {
  stop(shQuote("data_qsim"), " is deprecated.")
}

