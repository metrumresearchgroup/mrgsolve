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

##' Select and modify a data set for simulation.
##'
##'
##' @export
##' @param x model object
##' @param data data set
##' @param subset passed to \code{dplyr::filter_}; retain only certain 
##' rows in the data set
##' @param select passed to \code{dplyr::select_}; retain only certain 
##' columns in the data set
##' @param object character name of an object existing in \code{$ENV} 
##' to use for the data set
##' @param ... passed along
setGeneric("data_set", function(x,data,...) standardGeneric("data_set"))

##' @rdname data_set
##'
##' @details
##' Input data sets are \code{R} data frames that can include columns 
##' with any valid name, however columns with selected names are 
##' treated specially by \code{mrgsolve} and incorporated into the 
##' simulation.
##'
##' \code{ID} specifies the subject ID and is required for every 
##' input data set.
##'
##' When columns have the same name as parameters (\code{$PARAM} in 
##' the model specification file), the values in those columns will 
##' be used to update the corresponding parameter as the simulation 
##' progresses.
##'
##' Input data set may include the following columns related to 
##' PK dosing events: \code{time}, \code{cmt}, \code{amt}, \code{rate},
##' \code{ii}, \code{addl}, \code{ss}.  \code{time} and \code{cmt} 
##' (and \code{ID}) are required columns in the input data set. 
##' \code{time} is the observation or event time, \code{cmt} 
##' is the compartment number (see \code{\link{init}}), \code{amt} 
##' is the dosing amount, \code{rate} is the infusion rate, 
##' \code{ii} is the dosing interval, \code{addl} specifies 
##' additional doses to administer, and \code{ss} is a flag 
##' for steady state dosing.  These column names operate 
##' similarly to other non-linear mixed effects modeling 
##' software.  Upper case PK dosing column names including
##' \code{TIME}, \code{CMT}, \code{AMT}, \code{RATE}, \code{II},
##' \code{ADDL}, \code{SS} are also recognized.  However, an 
##' error will be generated if a mix of upper case and lower
##' case columns are found.
##'
##' Only numeric data can be brought in to the problem.  
##' Any non-numeric data columns will be dropped with warning.
##'
##' See \code{\link{exdatasets}} for different example data sets.
##' 
##' @seealso \code{\link{idata_set}}, \code{\link{ev}}, 
##' \code{\link{valid_data_set}}, \code{\link{valid_idata_set}}
##'
##' @examples
##'
##' mod <- mrgsolve:::house()
##' 
##' data <- expand.ev(ID=1:3, amt=c(10,20))
##'
##' mod %>% data_set(data, ID > 1) %>% mrgsim
##' 
##' data(extran1)
##' head(extran1)
##' 
##' mod %>% data_set(extran1) %>% mrgsim
##' mod %>% mrgsim(data=extran1)
##' 
##' @export
setMethod("data_set", c("mrgmod", "data.frame"), 
          function(x,data,subset=TRUE,select=TRUE,object=NULL,...) {
            
  if(exists("data", x@args)) stop("data already has been set.")
  if(!missing(subset)) data <- dplyr::filter_(data,.dots=lazy(subset))
  if(!missing(select)) data <- dplyr::select_(data,.dots=lazy(select))
  if(nrow(data) ==0) {
    stop("Zero rows in data after filtering.", call.=FALSE)
  }
  if(is.character(object)) {
    data <- data_hooks(data,object,x@envir,param(x),...) 
  }
  data <- valid_data_set(m=x,x=as.data.frame(data),...)
  x@args <- merge(x@args,list(data=data), open=TRUE)
  return(x)
})

##' @export
##' @rdname data_set
setMethod("data_set",c("mrgmod", "ANY"), function(x,data,...) {
  return(data_set(x,as.data.frame(data),...))
})

##' @export
##' @rdname data_set
setMethod("data_set", c("mrgmod", "missing"), function(x,object,...) {
  object <- data_hooks(object=object,envir=x@envir,param=param(x),...)
  return(data_set(x,as.data.frame(object),...))
})


##' Convert select upper case column names to lower case to conform to mrgsolve data expectations.
##'
##' @param data an nmtran-like data frame
##' @return A data.frame with renamed columns.
##'
##' @details
##' Columns that will be renamed with lower case versions: \code{AMT}, \code{II}, \code{SS}, \code{CMT}, \code{ADDL}, \code{RATE}, \code{EVID}, \code{TIME}.  If a lower case version
##' of these names exist in the data set, the column will not be renamed.
##' @export
lctran <- function(data) {
  n <- names(data)
  infrom <- is.element(n,tran_upper)
  haslower <- is.element(tolower(n),n)
  change <- infrom & !haslower
  if(sum(change) > 0) names(data)[change] <- tolower(n[change])
  data
}


data_hooks <- function(data,object,envir,param=list(),...) {
  param <- as.list(param)
  envir <- merge(as.list(param),as.list(envir),open=TRUE)
  objects <- cvec_cs(object)
  args <- list(...)
  if(missing(data)) {
    data <- eval(tparse(objects[1]),envir=envir)
    if(is.function(data)) {
      data <- do.call(data,args,envir=as.environment(envir))
    } 
    objects <- objects[-1]
  } 
  for(f in objects) {
    args$data <- data
    data <- do.call(f,args,envir=as.environment(envir))
  }
  return(data)
}
