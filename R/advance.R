

#' Advance an ODE system
#' 
#' @param x mrgsolve model object.
#' @param tspan vector of output times.
#' 
#' @export
advance <- function(x, tspan = NULL) {
  
  parin <- parin(x)
  parin$do_init_calc <- TRUE
  
  if(is.null(tspan)) tspan <- stime(x)
  
  out <- .Call(
    `_mrgsolve_ADVANCE`,
    parin,
    pointers(x),
    as.double(tspan),
    x,
    PACKAGE = "mrgsolve"
  )$data
  names(out) <- c("time", x@cmtL, x@capL) 
  out
}
