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

##' S4 events class
##' @slot data a data frame of events
##' @export
##' @keywords internal
setClass("ev", slots=c(data="data.frame"))

is.ev <- function(x) {
  inherits(x,"ev")  
}

##' dplyr verbs for event objects
##' 
##' @param .data the event object
##' @param ... passed to the \code{dplyr} function
##' @rdname ev_dplyr
##' @export
mutate.ev <- function(.data, ...) {
  .data@data <- as.data.frame(mutate(.data@data, ...))
  .data
}

##' @rdname ev_dplyr
##' @export
select.ev <- function(.data, ...) {
  .data@data <- as.data.frame(dplyr::select(.data@data,...))
  .data 
}

##' @rdname ev_dplyr
##' @export
filter_.ev <-  function(.data, ...) {
  .data@data <- as.data.frame(dplyr::filter_(.data@data,...))
  .data
}

##' Various methods for event objects
##' 
##' 
##' @param x an events object
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
##' @param add_ID numeric ID of length 1 used to add \code{ID} column only if 
##' one doesn't already exist
##' @param object used for \code{show}
##' @param ... passed to various methods
##' 
##' 
##' @examples
##' 
##' e <- ev(amt = 100)
##' 
##' names(e)
##' 
##' as.data.frame(e)
##' 
##' dim(e)
##' 
##' nrow(e)
##' 
##' @rdname ev_methods
##' @name ev_methods
NULL

##' @rdname ev_methods
##' @export
names.ev <- function(x) {
  names(x@data)  
}

##' @rdname ev_methods
##' @export
dim.ev <- function(x) {
  dim(x@data)  
}

##' @method as.matrix ev
##' @rdname ev_methods
##' @export
as.matrix.ev <- function(x,...) {
  as.matrix(x@data,...) 
}

##' @method as.data.frame ev
##' @rdname ev_methods
##' @export
as.data.frame.ev <- function(x, row.names = NULL, optional = FALSE, 
                             add_ID = NULL, ...) {
  ans <- x@data
  if(is.numeric(add_ID) & !has_ID(ans) & nrow(ans) > 0) {
    ans[["ID"]] <- add_ID[1]
  } 
  return(ans)
}

##' @rdname ev_methods
##' @export
##' @keywords internal
setMethod("show", "ev", function(object) {
  cat("Events:\n")
  print(as.data.frame(object))
  return(invisible(NULL))
})

As_data_set <- function(x) {
  if(!is.data.frame(x)) {
    if(is.ev(x)) {
      x <- x@data
    } else {
      x <- as.data.frame(x) 
    } 
  }
  if(nrow(x)==0) return(x)
  if(!has_ID(x)) x[["ID"]] <- 1
  return(x)
}




