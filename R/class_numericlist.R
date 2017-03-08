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

null_list <- setNames(list(), character(0))

valid.numericlist <- function(object) {
  x1 <- all(sapply(object@data,single.number))
  x2 <- all(names(object@data) !="")
  x3 <- !any(grepl("=|\\.",names(object),perl=TRUE))
  
  x <- x1 & x2 & x3
  if(all(x)) return(TRUE)

  out <- c()
  
  if(!x3) {
    message("Problem with names:")
    cat(paste(names(object), collapse=","))
    out <- c(out, "Invalid names")
  }
  
  if(!x2) {
    d <- object@data
    d <- d[nchar(names(d))==0]
    message("Parameter values without names:")
    print(d)
    out <- c(out, "All parameters require names")
  }
  
  if(!x1) {
    out <- c(out, "All parameters must be single numbers") 
  }

  return(out)

}

create_numeric_list <- function(x,class,...) {
  if(length(x) ==0) return(new(class))
  new(class, data=x)
}



##' S4 class numeric list.
##'
##' @name numericlist-class
##' @param data list of data
##' @param pattern character of length 1 containing regular expression to be used as a filter when printing data to the console
setClass("numericlist", 
         slots=c(
           data="list", 
           pattern="character"
         ),
         validity=valid.numericlist, 
         prototype=list(data=null_list, pattern="*")
)

##' Methods for numericlist.
##' 
##' These methods can be used to corece \code{param} and \code{init} objects into common \code{R} data structures,
##' extract elements from \code{numericlist}s, or get attributes from \code{numericlist}s.
##' 
##' @name numericlist
##' @rdname numericlist
NULL



##' @rdname numericlist
##' @param x object
##' @param ... passed along to other methods
##' @export
setMethod("as.list", "numericlist", function(x,...) as.list(x@data))

##' @rdname numericlist
##' @export
setMethod("as.numeric", "numericlist", function(x) {
  ans <- unlist(x@data)
  if(is.null(ans)) return(numeric(0))
  return(ans)
})

##' @rdname numericlist
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
##' @export
setMethod("as.data.frame", "numericlist", function(x,row.names=NULL, optional=FALSE,...) {
  as.data.frame(x@data,row.names,optional,...)
})

##' @rdname numericlist
##' @export
setMethod("length", "numericlist", function(x) length(x@data))

##' @rdname numericlist
##' @export
setMethod("names", "numericlist", function(x) as.character(names(x@data)))

##' @rdname numericlist
##' @param name column to take
##' @export
setMethod("$", "numericlist", function(x,name){unlist(x@data[name],use.names=FALSE)})

##' @export
##' @rdname numericlist
##' @param i elements to keep
##' @param j not used
##' @param drop not used
##' @aliases [,numericlist-method
setMethod("[", "numericlist", function(x,i,j,...){x@data[i,...]})
