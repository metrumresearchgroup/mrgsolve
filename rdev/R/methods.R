

##' @export
##' @rdname tgrid
setMethod("stime", "mrgmod",  function(x,...) {
  render_time(x)
})

##' @export
##' @rdname revar
setMethod("revar", "mrgmod", function(x,...) {
  return(list(omega=x@omega,sigma=x@sigma))
})

##' @export
##' @rdname blocks
setMethod("blocks", "mrgmod", function(x,...) {
  what <- as.character(match.call()[-1])[-1]
  blocks_(cfile(x),what)
})

##' @export
##' @rdname blocks
setMethod("blocks", "character", function(x,...) {
  what <- as.character(match.call()[-1])[-1]
  blocks_(x,what)
})

blocks_ <- function(file,what) {
  if(length(what)==0) what <- c("PARAM","MAIN", "ODE","DES", "TABLE")
  if(!file.exists(file)) stop("Can't find model file", call.=FALSE)
  bl <- modelparse(readLines(file, warn=FALSE))
  if(!any(what == "all")) bl <- bl[names(bl) %in% what]
  if(length(bl)==0) {
    message("No blocks found.")
    return(invisible(NULL))
  }
  
  bl <- lapply(bl, paste, collapse="\n")
  x1 <- paste0("$", names(bl), "\n")
  cat("\nModel file:",basename(file), "\n\n")
  cat(paste0(x1,unlist(bl)), sep="\n\n")
}





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






