
##' A quick simulation function.
##' 
##' @param x model object
##' @param data simulation skeleton
##' 
##' @export
##' 
qsim <- function(x,data) {
  
  parin <- parin(x)
  
 stopifnot(ncol(data)==5)
 
 data <- data.matrix(data)
  
  out <- .Call('mrgsolve_QUICKSIM', PACKAGE = 'mrgsolve',
               parin,
               as.numeric(param(x)),
               as.numeric(init(x)),
               0,
               pointers(x),
               data)
  
  dimnames(out) <- list(NULL, c("time", cmt(x)))
  as.data.frame(out)
  
}