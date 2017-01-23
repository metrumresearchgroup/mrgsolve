##' Select and modify a idata set for simulation.
##'
##' @param x model object
##' @param data a data set coercable to data.frame
##' @param object character name of an object existing in \code{$ENV} to use for the data set
##' @param subset passed to \code{dplyr::filter_}
##' @param select passed to \code{dplyr::select_}
##' @param covset coming in the future
##' @param ... passed along
##' @seealso \code{\link{data_set}}, \code{\link{ev}}
##' 
##' @details
##' The \code{idata_set} is a data.frame that specifies individual-level 
##' data for the problem.  An  \code{ID} column is required and there 
##' needs to be one row in the data frame for each individual.  
##' 
##' In most cases, the columns in the `idata_set` have the same names
##' as parameters in the \code{\link{param}} list.  When this is the case, 
##' the parameter set is updated as the simulation proceeds once at the 
##' start of each individual.  The `idata_set` can also be used to 
##' set initial conditions for each individual: for a compartment called
##' \code{CMT}, make a column in \code{idata_set} called \code{CMT_0} and 
##' make the value the desired initial value for that compartment.  Note that 
##' this initial condition will be over-ridden if you also set the \code{CMT_0} 
##' in \code{$MAIN}.
##' 
##' The most common application of \code{idata_set} is to specify a population
##' or bach of simulations to do.  We commonly use \code{idata_set} with an 
##' event object (see \code{\link{ev}}).  In that case, the event gets applied
##' to each individual in the \code{\link{idata_set}}.  
##' 
##' It is also possible to provide both a \code{data_set} and a \code{idata_set}.  
##' In this case, the \code{idata_set} is used as a parameter lookup for \code{IDs}
##' found in the \code{data_set}.  Remember in this case, it is the \code{data_set}
##' (not the \code{idata_set}) that determines the number of individuals in the
##' simulation.
##' 
##' @export
setGeneric("idata_set", function(x,data,...) standardGeneric("idata_set"))

##' @export
##' @rdname idata_set
setMethod("idata_set",c("mrgmod", "data.frame"), function(x,data,subset=TRUE,select=TRUE,covset=NULL,...) {
  if(exists("idata", x@args)) stop("idata has already been set.")
  if(!missing(subset)) data <- filter_(data,.dots=lazy(subset))
  if(!missing(select)) data <- select_(data,.dots=lazy(select))
  if(nrow(data) ==0) stop("Zero rows in idata after filtering.", call.=FALSE)
  if(!is.null(covset)) {
    stop("covset implementation has been disabled for the time begin.")
    # require_covset()
    # covset <- get(covset,x@envir)
    # if(!is.covset(covset)) stop("Object was not a covset.",call.=FALSE)
    # covset <- as.list(covset)
    # envir <- merge(as.list(param(x)),as.list(x@envir),open=TRUE)
    # data <- dmutate::mutate_random(data,covset,envir=envir) 
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
##' 
setMethod("idata_set",c("mrgmod", "missing"), function(x,object,...) {

  object <- eval(parse(text=object),envir=x@envir)
  
  if(is.function(object)) {
    object <- do.call(object,args=list(...),envir=x@envir)
  }
  object <- as.data.frame(object)

  return(idata_set(x,object,...))
})

