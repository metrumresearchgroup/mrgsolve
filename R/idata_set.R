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
##' @rdname idata_set
setMethod("idata_set",c("mrgmod", "data.frame"), function(x,data,subset=TRUE,select=TRUE,call=NULL,...) {
  if(exists("idata", x@args)) stop("idata has already been set.")
  if(!missing(subset)) data <- filter_(data,.dots=lazy(subset))
  if(!missing(select)) data <- select_(data,.dots=lazy(select))
  if(nrow(data) ==0) stop("Zero rows in idata after filtering.", call.=FALSE)
  if(is.character(call)) {
    data <- do.call(call,envir=x@envir,args=c(list(data),list(...)))
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
setMethod("idata_set",c("mrgmod", "missing"), function(x,object,form=NULL,...) {
  object <- eval(parse(text=as.character(object)),envir=x@envir)
  if(is.function(object)) {
    object <- do.call(object,args=list(...))
  }
  object <- as.data.frame(object)
  if(is.character(form)) {
    form <- cvec_cs(form)
    form <- mget(form,mode="language",envir=x@envir)
    for(f in form) {
      object <- dmutate(object,f) 
    }
  }

  return(idata_set(x,object,...))
})

