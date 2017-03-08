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

valid.matlist <- function(object) {
  
  labels <- names(object@data)[names(object@data) != "..."]
  
  x1 <- all(sapply(object@data, is.matrix))
  x2 <- all(sapply(object@data, is.numeric))
  
  x3 <- (!any(duplicated(labels))) | length(labels)==0
  
  x4 <- all(sapply(object@data, det)>=0)
  
  x5 <- mapply(object@data, object@labels, FUN=function(x,y) {
    nrow(x) == length(y)
  }) %>% all
  
  
  x <- x1 & x2 & x3 & x4 & x5
  
  if(all(x)) return(TRUE)
  out <- c()
  if(!x1) out <- c(out, "Found objects that are not matrix.")
  if(!x2) out <- c(out, "Found matrices that are not numeric.")
  if(!x3) {
    y <- labels[duplicated(labels)]
    message("Problem with this/these name(s):")
    cat(paste(y, collapse=","))
    out <- c(out, "Found duplicate names.")
  }
  
  if(!x4) {
    y <- which(sapply(object@data, det) < 0)
    message("Problem with this matrix:")
    print(object@data[y])
    out <- c(out, "Invalid matrix: determinant is less than 0.")
  }
  if(!x5) {
    
    n1 <- paste(sapply(object@data,   nrow),collapse=",")
    n2 <- paste(sapply(object@labels, length),collapse=',')
    out <- c(out, paste0("Length of labels (", n2, ") does not match the matrix rows (", n1, ")."))
  }
  return(out)
}

dim_matlist <- function(x) {
  if(length(x@data)==0) return(0)
  unname(sapply(x@data,nrow))
}

create_matlist <- function(x=list(),class,labels=list(),signature=NULL,...) {
  x <- x[!sapply(x,nrow)==0]
  if(is.null(names(x))) names(x) <- rep("...", length(x))
  names(x)[nchar(names(x))==0] <- "..."
  if(is.null(unlist(labels))) labels <- lapply(x, function(y) rep('.',nrow(y)))
  x <- new(class, data=x, labels=labels)
  x@n <- dim_matlist(x)
  return(x)
}


##' S4 class matlist.
##'
##' @rdname matlist-class
setClass("matlist", 
         slots=c(
           data="list",
           n="numeric", 
           labels="list"
         ),
         prototype=list(data=list(), labels=list()),
         validity=valid.matlist
)

is.matlist <- function(x) inherits(x,"matlist")