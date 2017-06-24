##' Inventory parameters in R objects.
##' 
##' @param x model object
##' @param obj the object to inventory
##' @param need required parameters
##' @param crump logical; if \code{TRUE} an error is generated when any parameter
##' in \code{need} is not found in the object
##' 
##' @export
inven <- function(x,obj,need=NULL,crump=TRUE) {
  # if(substr(need,1,1)=="@") {
  #   if(need=="@all") {
  #     need <- names(param(x))
  #   } else {
  #     need <- get(substr(need,2,nchar(need)),env_get(x,tolist=FALSE))  
  #   }
  # }
  
  if(is.null(need)) {
    # return a consistent type of empty character vector 
    # if nothing missing
    return(vector(mode = "character"))
  }
  
  need <- cvec_cs(need)
  
  if(!all(need %in% names(param(x)))) {
    stop("need can only be parameter names", call.=FALSE) 
  }
  
  if(crump) {
    inven_stop(obj,need)
  }
  
  return(inven_report(obj, need))
}

#' Check whether all required parameters needed in a model are present in an object
#' @param x model object
#' @param obj dataframe to pass to \code{\link{idata_set}} or \code{\link{data_set}}
#' @param ... capture dplyr-style parameter requirements
#' @param .strict whether to stop execution if all requirements are present (\code{TRUE}) 
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
#' the inventory is not \code{.strict} by default unless the user explicitly
#' states the parameter requirement. That is, if parameter requirements are explicitly 
#' stated, \code{.strict} will be set to \code{TRUE} if a value \code{.strict} was not
#' passed in the call.
#' @export
inventory <- function(x,obj,..., .strict = FALSE) {
  
  oname <- as.character(as.list(match.call())$obj)

  need <- select_vars(names(param(x)),...)
  
  if(!length(need)) {
    need <- names(param(x))
  } else {
    if(missing(.strict)) .strict <- TRUE 
  }
  
  if (.strict) {
    inven_stop(obj, need, oname)
  }
  
  found <- inven_report(obj, need, oname)
  if (!length(found)) {
    message("Found all required parameters in ", sQuote(oname),".")
  }
  
  return(invisible(x))
}

inven_stop <- function(obj,need,oname) {
  miss <- setdiff(need,names(obj))
  if (length(miss)) {
    stop("missing parameters in ", sQuote(oname), "\n", 
         paste(paste0(" - ",miss,collapse="\n")),call.=FALSE)
  }
  return(TRUE)
}

inven_report <- function(obj,need,oname) {
  miss <- setdiff(need,names(obj))
  if (length(miss)) {
    warning("Missing parameters in ", shQuote(oname), "\n", 
            paste(paste0(" - ",miss,collapse="\n")), call.=FALSE)
  }
  return(miss)
}

