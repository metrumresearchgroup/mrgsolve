
##' A quick simulation function.
##' 
##' @param x model object
##' @param data simulation skeleton
##' 
##' @export
##' 
qsim <- function(x,e,idata,stime) {
  
  parin <- parin(x)

  e <- qdata(e,stime)
  
  idata <- data.matrix(idata)
  
  cap <- c(length(x@capture),seq_along(x@capture)-1)
  
  out <- .Call('mrgsolve_QUICKSIM', 
               PACKAGE = 'mrgsolve',
               parin,
               as.numeric(param(x)),
               as.numeric(init(x)),
               e,idata,
               cap,
               pointers(x))
  
  dimnames(out) <- list(NULL, c("time", cmt(x),x@capture))
  as.data.frame(out)
  
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
  rate <- rep(0,nrow)
  amt <- e$amt * evid
  cmt <- e$cmt
  x <- matrix(0,nrow=nrow,ncol=5)
  
  x[,1] <- 1
  x[,2] <- time
  x[,3] <- cmt
  x[,4] <- evid
  x[,5] <- amt
  
  x
}

