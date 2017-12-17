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


.ren.make_pairs <- function(from,to = NULL) {
  if(is.null(to)) return(paste(from,from,sep = "="))
  if(length(to) != length(from)) {
    stop("cannot make relabel pairs; different lengths", call. = FALSE)
  }
  paste(to,from,sep="=")
}


.ren.parse <- function(x,y = NULL,context=NULL,unique=TRUE) {
  if(length(x)==0) list(Old = character(0), New = character(0))
  if(!is.null(y)) x <- .ren.make_pairs(x,y)
  x <- gsub("=", " = ", x, fixed = TRUE)
  x <- cvec_cs(x)
  weq <- which(x=="=")
  to <- weq-1
  from <- weq+1
  named <- c(to,from)
  if(any(duplicated(named), named > length(x), named < 0)) {
    msg <- "error parsing input"
    if(is.character(context)) {
      paste0(msg, ": ", msg)
    }
    stop(msg)
  }
  alone <- setdiff(seq_along(x), c(to,from,weq))
  from <- x[sort(c(from,alone))]
  to <- x[sort(c(to,alone))]
  if(unique) {
    drop <- duplicated(to, fromLast = TRUE)
    from <- from[!drop]
    to <- to[!drop]
  }
  list(Old = from, New = to)
}

# relabel_initialize <- function(x,y = NULL) {
#   x <- relabel_parse(x,y) 
#   self$Old <- x$Old
#   self$New <- x$New
#   if(all(x$Old==x$New)) {
#     self$identical <- TRUE 
#   }
# }
# 
# 
# relabel_relabel <- function(y) {
#   if(self$identical) return(y)
#   old <- match(y,self$Old)
#   old <- sort(old[!is.na(old)])
#   nw <- match(self$Old,y)
#   nw <- nw[!is.na(nw)]
#   y[nw] <- self$New[old]
#   return(y)
# }
# 
# relabel <- R6::R6Class("relabel" , class = TRUE,
#                        public = list(initialize = relabel_initialize, 
#                                      Old = character(0), 
#                                      New = character(0),
#                                      identical = FALSE,
#                                      as_vector = function(named = TRUE) {
#                                        if(named) return(setNames(self$New,self$Old))
#                                        return(self$New)
#                                      },
#                                      old  = function() {
#                                        self$Old
#                                      }, 
#                                      new = function() {
#                                        self$New 
#                                      }, 
#                                      relabel = relabel_relabel)
# )

.ren.create <- function(x,y = NULL) {
  x <- .ren.parse(x,y) 
  self <- list(identical = FALSE, old = character(0), new = character(0))
  self$old <- x$Old
  self$new <- x$New
  self$identical <- FALSE
  if(all(self$old==self$new)) {
    self$identical <- TRUE 
  }
  return(self)
}

.ren.rename <- function(self,y) {
  if(self$identical) return(y)
  old <- match(y,self$old)
  old <- sort(old[!is.na(old)])
  nw <- match(self$old,y)
  nw <- nw[!is.na(nw)]
  y[nw] <- self$new[old]
  return(y)
}

.ren.old <- function(self) return(self$old)

.ren.new <- function(self) return(self$new)

.ren.chr = function(self,named = TRUE) {
  if(named) return(setNames(self$new,self$old))
  return(self$new)
}




