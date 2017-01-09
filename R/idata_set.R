##' Set the \code{idata} argument for \code{mrgsim}.
##'
##'
##' @param x model object
##' @param data a data set coercable to data.frame
##' @param ... passed along
##' @export
setGeneric("idata_set", function(x,data,...) standardGeneric("idata_set"))
##' @export
##' @param subset passed to \code{dplyr::filter_}
##' @param select passed to \code{dplyr::select_}
##' @param covset the name of a \code{\link{covset}} object in `$ENV`
##' @rdname idata_set
setMethod("idata_set",c("mrgmod", "data.frame"), function(x,data,subset=TRUE,select=TRUE,covset=NULL,...) {
  if(exists("idata", x@args)) stop("idata has already been set.")
  if(!missing(subset)) data <- filter_(data,.dots=lazy(subset))
  if(!missing(select)) data <- select_(data,.dots=lazy(select))
  if(nrow(data) ==0) stop("Zero rows in idata after filtering.", call.=FALSE)
  if(!is.null(covset)) {
    covset <- get(covset,x@envir)
    envir <- merge(as.list(param(x)),as.list(x@envir),open=TRUE)
    data <- mutate_random(data,covset,envir=envir) 
  }
  x@args <- merge(x@args,list(idata=as.data.frame(data)), open=TRUE)
  return(x)
})
##' @export
##' @rdname idata_set
setMethod("idata_set",c("mrgmod", "ANY"), function(x,data,...) {
  return(idata_set(x,as.data.frame(data),...))
})
##' @export
##' @rdname idata_set
##' @param object character name of an object existing in \code{$ENV} to use for the data set
##' 
setMethod("idata_set",c("mrgmod", "missing"), function(x,object,...) {

  object <- eval(parse(text=object),envir=x@envir)
  
  if(is.function(object)) {
    object <- do.call(object,args=list(...),envir=x@envir)
  }
  object <- as.data.frame(object)

  return(idata_set(x,object,...))
})

