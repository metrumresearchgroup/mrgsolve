
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
  
  cap <- c(length(x@capture),seq_along(x@capture)-1)
  
  out <- .Call('mrgsolve_QUICKSIM', 
               PACKAGE = 'mrgsolve',
               parin,
               as.numeric(param(x)),
               as.numeric(init(x)),
               cap,
               pointers(x),
               data)
  
  dimnames(out) <- list(NULL, c("time", cmt(x),x@capture))
  as.data.frame(out)
}
