##' Do a simplified, quicker simulation run 
##' 
##' @param x a model object
##' @param data a simulation data set
##' @param recsort record sorting flag
##' @param stime a numeric vector of observation times; these observation
##' times will only be added to the output if there are no observation
##' records in \code{data}
##' @param skip_init_calc don't use \code{$MAIN} to 
##' calculate initial conditions
##' @param ... passed to update
##' 
##' 
##' @export
mrgsim_dq <- function(x,
                      data,
                      recsort = 1,
                      stime = numeric(0),
                      skip_init_calc = FALSE, ...) {
  
  ## data
  if(!is.valid_data_set(data)) {
    data <- valid_data_set(data,x,x@verbose)
  } 
  
  tcol <- timename(data)
  tcol <- if_else(is.na(tcol), "time", tcol)
  
  param <- as.numeric(param(x))
  init <-  as.numeric(Init(x))
  
  compartments <- Cmt(x)
  
  capt <- x@capture
  
  # Non-compartment names in capture
  capt <- unique(setdiff(capt,compartments))
  
  # First spot is the number of capture.items, followed by integer positions
  # Important to use the total length of x@capture
  capt_pos <- c(length(x@capture),(match(capt,x@capture)-1))
  
  # Big list of stuff to pass to DEVTRAN
  parin <- parin(x)
  parin$recsort <- recsort
  parin$stime <- stime
  parin$do_init_calc <- !skip_init_calc
  
  # already took intersect
  parin$request <- as.integer(seq_along(compartments)-1);
  
  out <- .Call(
    `_mrgsolve_SIMDATA`,
    parin,
    param,
    Pars(x),
    init,
    compartments,
    capt_pos,
    pointers(x),
    data,
    as.matrix(omat(x)),
    as.matrix(smat(x)),
    x@envir
  )
  
  cnames <- c("ID", tcol, compartments, capt)
  
  dimnames(out[["data"]]) <- list(NULL, cnames)
  
  new("mrgsims",
      request=compartments,
      data=as.data.frame(out[["data"]]),
      outnames=capt,
      mod=x)
}
