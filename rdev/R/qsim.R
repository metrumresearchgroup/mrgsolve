
##' A quick simulation function.
##' 
##' @param x model object
##' @param e event object
##' @param idata individual data set
##' @param stime a \code{\link{tgrid}} object or any object with \code{\link{stime}} method
##' 
##' 
##' @export
##' 
qsim <- function(x,e,idata,stime) {
  
  e <- qdata(e,stime)

  cap <- c(length(x@capture),seq_along(x@capture)-1)
  
  out <- .Call('mrgsolve_QUICKSIM', 
               PACKAGE = 'mrgsolve',
               parin(x),
               as.numeric(param(x)),
               as.numeric(init(x)),
               pars(x),
               cmt(x),
               e,data.matrix(idata),
               cap,
               pointers(x))
  
  dimnames(out) <- list(NULL, c("ID","time", cmt(x),x@capture))
  out
  
}

qdata <- function(e,stime) {
  e <- as.data.frame(e)
  if(is.null(e$ii)) {
    e$ii <- 0
    e$addl <- 0
  }
  doses <- seq(0,by=e$ii,length.out=e$addl+1)
  time <- sort(unique(c(stime,doses)))
  nrow <- length(time)
  evid <- as.integer(time %in% doses)
  rate <- rep(e$rate,nrow)
  amt <- e$amt * evid
  cmt <- e$cmt
  x <- matrix(0,nrow=nrow,ncol=6)
  
  x[,1] <- 1
  x[,2] <- time
  x[,3] <- cmt
  x[,4] <- evid
  x[,5] <- amt
  x[,6] <- rate
  
  x
}

