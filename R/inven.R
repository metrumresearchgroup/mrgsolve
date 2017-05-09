##' Inventory parameters in R objects.
##' 
##' @param x model object
##' @param obj the object to inventory
##' @param need required parameters
##' @param crump logical; if \code{TRUE} an error is generated when any parameter
##' in \code{need} is not found in the object
##' @param ... passed along
##' 
##' @export
##' 
##' 
##' 
inven <- function(x,obj,need="@all",crump=TRUE,...) {
  # if(substr(need,1,1)=="@") {
  #   if(need=="@all") {
  #     need <- names(param(x))
  #   } else {
  #     need <- get(substr(need,2,nchar(need)),env_get(x,tolist=FALSE))  
  #   }
  # }

  if(is.null(need)) {
    if(crump) inven_stop(x,obj,names(param(x)))
    return(inven_report(x,obj,names(param(x))))
  }
  
  need <- cvec_cs(need)
  
  if(!all(need %in% names(param(x)))) {
    stop("need can only be parameter names", call.=FALSE) 
  }
  
  if(!all(need %in% names(obj))) {
    if(crump) inven_stop(x,obj,need)
    return(inven_report(x,obj,need))
  }
  
  return(invisible(TRUE))
}

##' @rdname inven
##' @export
inventory <- function(x,obj,...) {
  dots <- lazyeval::lazy_dots(...)
  need <- select_vars_(names(param(x)),dots)
  if(length(need)==0) need <- names(param(x))
  inven(x,obj,need,crump=FALSE) 
  message("Found all required parameters.")
  return(x)
}

inven_stop <- function(x,obj,need) {
  miss <- setdiff(need,names(obj))
  stop("The object is missing required parameters:\n", 
       paste(paste0("- ",miss,collapse="\n")),call.=FALSE)
}

inven_report <- function(x,obj,need) {
  miss <- setdiff(need,names(obj))
  message("The object is missing these parameters:\n", 
       paste(paste0(" - ",miss,collapse="\n")))
  return(invisible(FALSE))
}

