# Copyright (C) 2013 - 2024  Metrum Research Group
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



#' Select and modify a idata set for simulation
#' 
#' The individual data set (`idata_set`) is a data frame with one 
#' row for each individual in a population, specifying parameters and 
#' other individual-level data.
#'
#' @param x model object.
#' @param data a data set that can be coerced to data.frame.
#' @param object character name of an object existing in `$ENV` 
#' to use for the data set.
#' @param .subset an unquoted expression passed to [dplyr::filter()]; retain 
#' only certain rows in the data set.
#' @param .select passed to [dplyr::select()]; retain only certain columns in 
#' the data set; this should be the result of a call to [dplyr::vars()].
#' @param need passed to [inventory()].
#' @param ... other arguments passed along when `object` is a function.
#' 
#' @details
#' The `idata_set` is a data frame that specifies individual-level 
#' data for the problem. An `ID` column is required and there 
#' can be no more than one row in the data frame for each individual.  
#' 
#' In most cases, the columns in the `idata_set` have the same names
#' as parameters in the [param()] list.  When this is the case, 
#' the parameter set is updated as the simulation proceeds once at the 
#' start of each individual.  The `idata_set` can also be used to 
#' set initial conditions for each individual: for a compartment called
#' `CMT`, make a column in `idata_set` called `CMT_0` and 
#' make the value the desired initial value for that compartment.  Note that 
#' this initial condition will be overridden if you also set the `CMT_0` 
#' in `$MAIN` (`$PK`).
#' 
#' The most common application of `idata_set` is to specify a population
#' or batch of simulations to do.  We commonly use `idata_set` with an 
#' event object (see [ev()]).  In that case, the event gets applied
#' to each individual in the `idata_set`.  
#' 
#' It is also possible to provide both a `data_set` and a `idata_set`.  
#' In this case, the `idata_set` is used as a parameter lookup for `IDs`
#' found in the `data_set`.  Remember in this case, it is the `data_set`
#' (not the `idata_set`) that determines the number of individuals in the
#' simulation.
#' 
#' An error will be generated if any parameter columns in the 
#' input idata set contain `NA`.  
#'  
#' @examples
#' mod <- mrgsolve::house()
#' 
#' data(exidata)
#' 
#' exidata
#' 
#' mod %>% 
#'   idata_set(exidata, ID <= 2) %>% 
#'   ev(amt = 100) %>%
#'   mrgsim() %>% 
#'   plot()
#' 
#' mod %>% 
#'   idata_set(exidata) %>% 
#'   ev(amt = 100) %>%
#'   mrgsim()
#' 
#' mod %>% ev(amt = 100) %>% mrgsim(idata=exidata) 
#' 
#' @seealso [data_set()], [ev()]
#' 
#' @md
#' @export
setGeneric("idata_set", function(x,data,...) {
  standardGeneric("idata_set")
})

##' @rdname idata_set
##' @export
setMethod("idata_set", c("mrgmod", "data.frame"), function(x,data,.subset=TRUE,.select=TRUE,object=NULL,need=NULL,...) {
  
  if(is.character(need)) suppressMessages(inventory(x,data,need))
  
  if(!missing(.subset)) {
    data <- filter(data,`!!`(enquo(.subset)))
  }
  if(!missing(.select)) {
    data <- select(data,`!!!`(.select))
  }
  if(nrow(data)==0) {
    stop("zero rows in idata after filtering.", call.=FALSE)
  }
  if(is.character(object)) {
    data <- data_hooks(data,object,x@envir,param(x),...) 
  }
  x@args[["idata"]] <- as.data.frame(data)
  return(x)
  
})

##' @rdname idata_set
##' @export
setMethod("idata_set",c("mrgmod", "ANY"), function(x,data,...) {
  return(idata_set(x,as.data.frame(data),...))
})

##' @rdname idata_set
##' @export
setMethod("idata_set",c("mrgmod", "missing"), function(x,object,...) {
  object <- data_hooks(object=object,envir=x@envir,param=param(x),...)
  return(idata_set(x,object,...))
})

