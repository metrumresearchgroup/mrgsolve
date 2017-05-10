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
#' @param x mrgsolve model
#' @param obj dataframe to pass to idata_set or data_set
#' @param ... capture dplyr-style parameter requirements
#' @param .strict whether to stop execution if all requirements are present (TRUE) or just warn (FALSE)
#' @examples \dontrun{
#' inventory(mod, idata, CL:V) # parameters defined, inclusively, CL through Volume 
#' inventory(mod, idata, everything()) # all parameters
#' inventory(mod, idata, contains("OCC")) # all parameters containing OCC
#' inventory(mod, idata, -F) # all parameters except F
#' }
#' @return original mrgmod
#' @export
inventory <- function(x,obj,..., .strict = TRUE) {
  dots <- lazyeval::lazy_dots(...)
  need <- select_vars_(names(param(x)),dots)
  
  if(!length(need)) {
    return(x)
  }
  
  if (.strict) {
    inven_stop(obj, need)
  }
  
  found <- inven_report(obj, need)
  if (!length(found)) {
    message("Found all required parameters.")
  }
  
  return(x)
}

inven_stop <- function(obj,need) {
  miss <- setdiff(need,names(obj))
  if (length(miss)) {
    stop("The object is missing required parameters:\n", 
         paste(paste0("- ",miss,collapse="\n")),call.=FALSE)
  }
  return(TRUE)
}

inven_report <- function(obj,need) {
  miss <- setdiff(need,names(obj))
  if (length(miss)) {
    warning("The object is missing these parameters:\n", 
       paste(paste0(" - ",miss,collapse="\n")))
  }
  return(miss)
}

