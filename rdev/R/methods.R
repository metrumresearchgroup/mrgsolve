





parin <- function(x) {
  list(rtol=x@rtol,atol=x@atol, hmin=as.double(x@hmin), hmax=as.double(x@hmax),ixpr=x@ixpr,
       maxsteps=as.integer(x@maxsteps),mxhnil=x@mxhnil,verbose=as.integer(x@verbose),debug=x@debug,
       digits=x@digits, tscale=x@tscale,
       mindt=x@mindt, advan=x@advan)
}


##' Show model specification and C++ files.
##' 
##' @param x model object
##' @param spec logical; show the model specification file
##' @param source logical; show the C++ file that is actually compiled
##' @param ... not used
##' @export
##' 
file_show <- function(x,spec=TRUE,source=TRUE,...) {
  stopifnot(is.mrgmod(x))
  what <- list()
  if(spec) what$spec <- cfile(x)
  if(source) what$source <- x@shlib$source
  do.call(base::file.show,what)
}






