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

#' Check whether all required parameters needed in a model are present in an object
#' @param x model object
#' @param obj data.frame to pass to \code{\link{idata_set}} or \code{\link{data_set}}
#' @param ... capture dplyr-style parameter requirements
#' @param .strict whether to stop execution if all requirements are present
#'  (\code{TRUE}) 
#' or just warn (\code{FALSE}); see details
#' @examples \dontrun{
#' inventory(mod, idata, CL:V) # parameters defined, inclusively, CL through Volume 
#' inventory(mod, idata, everything()) # all parameters
#' inventory(mod, idata, contains("OCC")) # all parameters containing OCC
#' inventory(mod, idata, -F) # all parameters except F
#' }
#' @return original mrgmod
#' @details 
#' If parameter requirements are not explicitly stated, the requirement defaults to 
#' all parameter names in \code{x}.  Note that, by default,
#' the inventory is not \code{.strict} unless the user explicitly
#' states the parameter requirement. That is, if parameter requirements are explicitly 
#' stated, \code{.strict} will be set to \code{TRUE} if a value \code{.strict} was not
#' passed in the call.
#' @export
inventory <- function(x, obj, ..., .strict = FALSE) {
  
  oname <- as.character(as.list(match.call())$obj)
  
  need <- vars_select(Pars(x),...)
  
  if(!length(need)) {
    need <- Pars(x)
  } else {
    if(missing(.strict)) .strict <- TRUE 
  }
  
  missing <- setdiff(need,names(obj))
  miss <- length(missing) 
  
  if(!miss) {
    message("Found all required parameters in ", sQuote(oname),".")
    return(invisible(x))
  }
  
  if(.strict) {
    stop("missing parameters in ", sQuote(oname), "\n", 
         paste(paste0(" - ", missing, collapse="\n")), call.=FALSE)
  } else {
    warning("Missing parameters in ", shQuote(oname), "\n", 
            paste(paste0(" - ", missing, collapse="\n")), call.=FALSE)
  }
  
  return(invisible(x))
}

#' Check inputs against model parameters
#' 
#' @param obj a data frame or other object with names to check.
#' @param x a model object.
#' 
#' @md
#' @export
check_inputs <- function(obj, x, covariates = TRUE, strict = FALSE, 
                         silent = FALSE) {
  
  need <- x@shlib$input
  need_type <- rep("input", length(need))
  
  if(isTRUE(covariates)) {
    need <- c(need, x@shlib$covariates)  
    n_cov <- length(x@shlib$covariates)
    need_type <- c(need_type, rep("covariates", n_cov))
  }

  n_need <- length(need)
  
  if(n_need==0) {
    warn("didn't find any covariates or inputs to check.")
    return(invisible(NULL))
  }
  
  found <- need %in% names(obj)
  
  if(!(status <- all(found))) {
    miss <- need[!found]
    miss <- formatC(miss, width = max(nchar(miss)), flag = "-")
    miss <- paste0(miss, " (", need_type[!found], ")")
    names(miss) <- rep("*", length(miss))
    msg <- c(
      "Could not find the following input items", 
      miss
    )
    if(isTRUE(strict)) {
      abort(msg)  
    } else {
      if(isFALSE(silent)) warn(msg) 
    }
  }
  
  return(invisible(status))
}
