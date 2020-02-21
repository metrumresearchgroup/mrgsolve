# Copyright (C) 2013 - 2020  Metrum Research Group
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

##' @export
##' @rdname stime
##' @keywords internal
setClass("tgrid", slots=c(start  = "numeric", 
                          end    = "numeric", 
                          delta  = "numeric", 
                          add    = "numeric", 
                          offset = "numeric", 
                          scale  = "numeric"),
         prototype=list(start  = 0, 
                        end    = 24, 
                        delta  = 1, 
                        offset = 0,
                        scale  = 1))

##' @export
##' @rdname stime
setClass("tgrids", slots=c(data="list"))


##' Create a list of designs from a data frame
##' 
##' @param data input data set; see details
##' @param descol character column name to be used for design groups
##' 
##' @details
##' The input data set must have a column with the same name as the value 
##' of \code{descol}.  Other column names should be \code{start} (the time 
##' of the first observation), \code{end} (the time of the last observation), 
##' \code{delta} (the time steps to take between \code{start} and \code{end}), 
##' and \code{add} (other, ad-hoc times).  Note that \code{add} might be 
##' a \code{list-column} to get a vector of times for each time grid object.
##' 
##' @return The function returns a list of \code{tgrid} objects, 
##' one for each unique value found in \code{descol}.
##' 
##' @examples
##' idata <- tibble::tibble(ID=1:4, end=seq(24,96,24), delta=6,
##' add=list(c(122,124,135),c(111), c(99),c(88)))
##' 
##' idata <- dplyr::mutate(idata, GRP = ID %%2)
##' 
##' idata
##' 
##' l <- as_deslist(idata,"GRP")
##' 
##' l
##' 
##' lapply(l,stime)
##' 
##' lapply(as_deslist(idata, "ID"),stime)
##' 
##' @export
as_deslist <- function(data, descol="ID") {
  
  if(!is.data.frame(data)) {
    stop("data must be a data frame", call.=FALSE) 
  }
  if(!is.element("end", names(data))) {
    stop("end is a required column for input data", call.=FALSE) 
  }
  if(!is.element("delta", names(data))) {
    data[["delta"]] <- 1
  }
  if(!is.element("start", names(data))) {
    data[["start"]] <- 0
  }
  if(!is.element("add", names(data))) {
    data[["add"]] <- 0
  }

  designs <- distinct__(data, .dots=descol, .keep_all=TRUE)
  
  designs <- as.data.frame(designs)
  
  data <- as.data.frame(data)
  
  deslevels <- sort(designs[,descol])
  
  fact <- match(designs[,descol], deslevels)
  
  sp <- split(designs, fact)
  
  sp <- setNames(sp, paste0(descol,"_",deslevels))
  
  out <- lapply(sp, function(x) {
    tgrid(start=x$start[1],end=x$end[1],delta=x$delta[1],add=unlist(x$add))
  })
  
  structure(out, descol=descol)
}

##' Create a simtime object
##'
##' simtime objects allow the user to specify simulation start and end times, 
##' along with the simulation time step.
##'
##' @rdname tgrid
##' @name tgrid
##'
##' @param x tgrid object
##' @param start simulation start time
##' @param end simulation end time
##' @param delta simulation time step
##' @param add addition simulation times
##' @param .offset the resulting set of times will be adjusted by this amount
##' @param .scale the resulting set of times will be scaled by this factor
##' @param ... passed on to other methods
##' @export
##' @examples
##'
##' peak <- tgrid(0,6,0.2)
##' sparse <- tgrid(0,24,4)
##'
##' day1 <- c(peak,sparse)
##'
##' design <- c(day1, day1+72, day1+240)
##' 
##' \dontrun{
##' mod <- mrgsolve::house()
##'
##' out <- mod %>% ev(amt=1000, ii=24, addl=10) %>% mrgsim(tgrid=design)
##'
##' plot(out,CP~., type='b')
##' }
tgrid <-  function(start=0,end=24,delta=1,add=numeric(0),
                   .offset=0, .scale=1,...) {
  new("tgrid", start=start, end=end, delta=delta, 
      add=add, offset=.offset, scale=.scale)
}

tgrids <- function(...) {
  new("tgrids", data=list(...))
}

#' @rdname tgrid
#' @export
setMethod("stime", "tgrid", function(x,...) {
  (render_time(x) + x@offset) * x@scale
})

#' @rdname tgrid
#' @export
setMethod("stime", "tgrids", function(x,...) {
  sort(unique(unlist(lapply(x@data,stime), use.names=FALSE)))
})

render_time <- function(x) {
  add <- times <- numeric(0)
  #if(!is.mt(x@add)){add <- x@add}
  if(x@end >= 0){times <-seq(x@start,x@end,x@delta)}
  times <- invisible(as.numeric(unique(c(times,x@add))))
  if(is.mt(times)) {return(0)}
  sort(times[times>=0])
}

##' @rdname tgrid
##' @export
setMethod("stime", "numeric", function(x,...) {
  sort(unique(x))
})

##' Operations with tgrid objects
##' 
##' @param x mrgmod object
##' @param recursive not used
##' @param ... passed along to other methods
##' @rdname tgrid_ops
##' @export
setMethod("c", "tgrid", function(x,..., recursive=FALSE) {
  
  x <- c(list(x), list(...))
  
  singles <- sapply(x, inherits, what="tgrid")
  
  multis <- sapply(x, inherits, what="tgrids")
  
  x <- c(x[singles], unlist(lapply(x[multis], function(y) y@data)))
  
  do.call("tgrids", x)
})


##' @rdname tgrid_ops
##' @export
setMethod("c", "tgrids", function(x,...,recursive=FALSE) {
  do.call("c",c(x@data, list(...)))
})


##' @param e1 tgrid or tgrids object
##' @param e2 numeric value
##' 
##' @name tgrid_+_numeric
##' @docType methods
##' @aliases +,tgrid,numeric-method
##' @rdname tgrid_ops
setMethod("+", c("tgrid","numeric"), function(e1,e2) {
  e1@offset <- e1@offset + e2
  e1
})

##' @name tgrid_*_numeric
##' @docType methods
##' @aliases *,tgrid,numeric-method
##' @rdname tgrid_ops
setMethod("*", c("tgrid", "numeric"), function(e1,e2) {
  e1@scale <- e2
  e1
})


##' @rdname tgrid_ops
##' @name tgrids_+_numeric
##' @docType methods
##' @aliases +,tgrids,numeric-method
setMethod("+", c("tgrids","numeric"), function(e1,e2) {
  e1@data <- lapply(e1@data, function(x) {
    x@offset <- x@offset + e2
    x
  })
  e1
})

##' @rdname tgrid_ops
##' @name tgrids_*_numeric
##' @docType methods
##' @aliases *,tgrids,numeric-method
setMethod("*", c("tgrids","numeric"), function(e1,e2) {
  e1@data <- lapply(e1@data, function(x) {
    x@scale <- e2
    x
  })
  e1
})

##' @rdname tgrid
##' @param object passed to show
##' @export
##' @keywords internal
setMethod("show", "tgrid", function(object) {
  x <- stime(object)
  min <- min(x)
  max <- max(x)
  cat("start: ", object@start, " ")
  cat("end:   ", object@end, " ")
  cat("delta: ", object@delta, " ")
  cat("offset:", object@offset, " ")
  cat("min:   ", min, "  ")
  cat("max:   ", max, "\n")
})


##' @export
##' @rdname tgrid
setMethod("show", "tgrids", function(object) {
  lapply(object@data, function(x) {
    show(x)
    cat("--------\n")
  })
})







