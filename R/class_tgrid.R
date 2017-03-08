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

##' @export
##' @rdname stime
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


##' Create a list of designs from a data frame.
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
##' idata <- dplyr::data_frame(ID=1:4, end=seq(24,96,24), delta=6,
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
as_deslist <- function(data,descol="ID") {
  
  if(!is.data.frame(data)) {
    stop("data must be a data frame", call.=FALSE) 
  }
  if(!is.element("end", names(data))) {
    stop("end is a required column for input data", call.=FALSE) 
  }
  if(!is.element("delta", names(data))) {
    data <- mutate(data,delta = 1) 
  }
  if(!is.element("start", names(data))) {
    data <- mutate(data,start=0) 
  }
  if(!is.element("add", names(data))) {
    data <- mutate(data,add=0)
  }
  
  designs <- as.data.frame(distinct_(data,.dots=descol,.keep_all=TRUE))
  
  sp <- setNames(split(designs,designs[,descol]), paste0(descol,"_",designs[,descol]))
  
  out <- lapply(sp, function(x) {
    tgrid(start=x$start[1],end=x$end[1],delta=x$delta[1],add=unlist(x$add))
  })
  
  structure(out, descol=descol)
}


