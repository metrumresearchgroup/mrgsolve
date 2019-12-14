# Copyright (C) 2013 - 2019  Metrum Research Group
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

##' S4 class for mrgsolve simulation output
##'
##' @slot request character vector of compartments requested in simulated 
##' output
##' @slot outnames character vector of column names in simulated output 
##' coming from table step
##' @slot data data.frame of simulated data
##' @slot mod the mrgmod model object
##' @keywords internal
setClass("mrgsims",
         slots=c(
           request="character",
           outnames="character",
           data="data.frame",
           mod="mrgmod"
         )
)

setClass("batch_mrgsims",contains="mrgsims",
         slots=c(
           knobs="character",
           batch="data.frame",
           request="character",
           moving="character",
           input="list"
         )
)


##' Check if an object is mrgsim output
##' 
##' @param x any object
##' 
##' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
##' 
##' @export
is.mrgsims <- function(x) inherits(x,"mrgsims")


##' Coerce an mrgsims object to list
##' 
##' @param x an mrgsims object
##' @param ... not used
##' @export
setMethod("as.list", "mrgsims", function(x, ...) {
  to_get <- slotNames("mrgsims") 
  out <- vector("list",length(to_get))
  for(.i in seq_along(to_get)) {
    out[[.i]] <- slot(x,to_get[.i]) 
  }
  out <- setNames(out, to_get)
  structure(out, class = "mrgsims_list")
})

#' @export
.DollarNames.mrgsims <- function(x, pattern){
  grep(pattern, names(x), value=TRUE)
}
