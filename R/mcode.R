# Copyright (C) 2013 - 2018  Metrum Research Group, LLC
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


##' Write, compile, and load model code
##'
##' This is a convenience function that ultimately calls \code{\link{mread}}.
##'
##' @param model model name
##' @param project project name
##' @param code character string specifying a \code{mrgsolve} model
##' @param ... passed to \code{\link{mread}}
##' @details
##' Note that the arguments are in slightly different order than 
##' \code{\link{mread}}.  The default \code{project} is \code{tempdir()}.
##'
##' @examples
##'
##' \dontrun{ 
##' code <- '
##' $CMT DEPOT CENT
##' $PKMODEL ncmt=1, depot=TRUE
##' $MAIN
##' double CL = 1;
##' double V = 20;
##' double KA = 1;
##' '
##'
##' mod <- mcode("example",code)
##' }
##' 
##' @seealso \code{\link{mread}}, \code{\link{mread_cache}}
##' 
##' @export
mcode <- function(model, code, project=tempdir(), ...) {
  mread(model=model, project=project, code=code,...)
}

##' @rdname mcode
##' @export
mcode_cache <- function(model, code, project=tempdir(),  ...) {
  mread_cache(model, project, code = code, ...)
}

