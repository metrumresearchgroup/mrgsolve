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

##' S4 class for mrgsolve simulation output.
##'
##' @slot request character vector of compartments requested in simulated output
##' @slot outnames character vector of column names in simulated output coming from table step
##' @slot data matrix of simulated data
##' @slot mod the mrgmod model object
setClass("mrgsims",
         slots=c(
           request="character",
           outnames="character",
           data="data.frame",
           mod="mrgmod",
           seed="integer",
           date="character"
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


##' Check if an object is mrgsim output.
##' 
##' @param x any object
##' 
##' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
##' 
##' @export
is.mrgsims <- function(x) inherits(x,"mrgsims")
