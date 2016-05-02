globalVariables("ID")


## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

##' @include classes.R
NULL

##' Methods for working with \code{mrgsims} objects.
##'
##' These methods help the user view simulation output and extract simulated data to work with further.  The methods listed here for the most part have generics defined by R or other R packages.  See the \code{seealso} section for other methods defined by \code{mrgsolve} that have their own documentation pages.
##'
##' @details
##' Most methods should behave as expected according to other method commonly used in R (e.g. head, tail, as.data.frame, etc ...)
##'
##' \itemize{
##'   \item{\code{subset}} coreces simulated output to data.frame and passes to subset.data.frame
##'   \item{\code{$}} selects a column in the simulated data and returns numeric
##'   \item{\code{head}} see \code{\link{head.matrix}}; returns simulated data
##'   \item{\code{tail}} see \code{\link{tail.matrix}}; returns simulated data
##'   \item{\code{dim}, \code{nrow}, \code{ncol}} returns dimensions, number of rows, and number of columns in simulated data
##'   \item{\code{as.data.frame}} coreces simulated data to data.frame and returns the data.frame
##'   \item{\code{as.matrix}} returns matrix of simulated data
##'   \item{\code{as.tbl}} coreces simulated to \code{tbl_df}; requires \code{dplyr}
##'   \item{\code{summary}} coreces simulated data to data.frame and passes to \code{\link{summary.data.frame}}
##'   \item{\code{plot}} plots simulated data; see \code{\link{plot_mrgsims}}
##' }
##' @param x mrgsims object
##' @param ... passed to other functions
##' @aliases mrgsims
##' @seealso mod request variables label limit stime
##' @name mrgsims
##' @rdname mrgsims
##' @examples
##'
##' ## example("mrgsims")
##'
##' mod <- mrgsolve:::house() %>% init(GUT=100)
##'
##' out <- mrgsim(mod)
##' class(out)
##'
##' out
##' head(out)
##' tail(out)
##'
##' mod(out)
##'
##' dim(out)
##' names(out)
##'
##' mat <- as.matrix(out)
##' df <- as.data.frame(out)
##'
##' df <- subset(out, time < 12) ## a data frame
##' out$CP
##'
##' plot(out)
##' plot(out, CP~.)
##' plot(out, CP+RESP~time, scales="same", xlab="Time", main="Model sims")
##'
##' out <- label(out, DOSE=100)
##' head(out)
NULL

##' Label simulation output.
##'
##' Attaches a named column to the simulation output with a single numeric value.
##'
##' @param x mrgsims object
##' @param ... name=value pairs; value must be numeric.
##' @export
##' @rdname label
setGeneric("label", function(x,...) standardGeneric("label"))
##' @export
##' @rdname label
setMethod("label", "mrgsims", function(x,...) {

    args <- unlist(list(...))

    if(!all(sapply(args, is.numeric))) stop("Values must be all numeric")

    args <- sapply(args, function(x) x[1])

    if(any(names(args) %in% colnames(x@data))) stop("label name already exists in simulated data.")
    if(!all(nchar(names(args)) > 0)) stop("label name not found")
    y <- matrix(args, nrow=dim(x)[1], ncol=length(args), byrow=TRUE, dimnames=list(NULL,names(args)))
    x@data <- cbind(x@data,y)
    x
})

##' Return the model object.
##'
##' @param x mrgsims object
##' @param ... passed along
##' @export
##' @rdname mod
setGeneric("mod", function(x,...) standardGeneric("mod"))
##' @export
##' @rdname mod
setMethod("mod", "mrgsims", function(x,...) {x@mod})



##' Return the requested compartments from a simulation run.
##'
##' @param x mrgsims object
##' @param ... passed along
##' @seealso \code{\link{variables}}
##' @export
##' @rdname request
setGeneric("request", function(x,...) standardGeneric("request"))
##' @export
##' @rdname request
setMethod("request", "mrgsims", function(x,...) {return(x@request)})

##' Return the requested compartments and tabled items from a simulation run.
##'
##' @param x mrgsims object
##' @param ... passed along
##' @seealso \code{\link{request}}
setGeneric("variables", function(x,...) standardGeneric("variables"))
##' @export
##' @rdname variables
setMethod("variables", "mrgsims", function(x,...) return(c(x@request,x@outnames)))

##' @export
##' @rdname mrgsims
##' @param name name of column of simulated output to retain
setMethod("$", "mrgsims", function(x,name) {
  if(!is.element(name, colnames(x@data))) stop("Couldn't find column ", name, " in simulated data")
  return(x@data[,name])
})


##' @export
##' @rdname mrgsims
setMethod("tail", "mrgsims",function(x,...) {
  cat("Model: ", model(mod(x)), "\n")
  return(tail(x@data,...))
})

##' @export
##' @rdname mrgsims
setMethod("head", "mrgsims",function(x,...) {
  cat("Model: ", model(mod(x)), "\n")
  return(head(x@data,...))
})

##' @export
##' @rdname mrgsims
setMethod("dim", "mrgsims", function(x) {
  return(dim(x@data))
})

##' @export
##' @rdname mrgsims
setMethod("names", "mrgsims", function(x) {
  return(colnames(x@data))
})


##' @export
##' @rdname mrgsims
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
setMethod("as.data.frame", "mrgsims", function(x,row.names=NULL, optional=FALSE,...) {
  return(as.data.frame(x@data,row.names,optional,...))
})

##'
##' @param .dots passed to various \code{dplyr} functions
##' @param .data passed to various \code{dplyr} functions
##' @param add passed to \code{dplyr::group_by_}
##' @param funs passed to \code{dplyr::summarise_each}
##' @export
##' @rdname mrgsims
##' @importFrom dplyr as.tbl filter_ group_by_ mutate_ summarise_each_ do_ select_ slice_ summarise_
##' @importFrom lazyeval lazy_dots
as.tbl.mrgsims <- function(x,...) dplyr::as.tbl(as.data.frame(x))
##' @importFrom dplyr as.tbl
##' @export
##' @rdname mrgsims
filter_.mrgsims <- function(.data,...,.dots) dplyr::as.tbl(as.data.frame(.data)) %>% dplyr::filter_(...,.dots=.dots)
##' @export
##' @rdname mrgsims
group_by_.mrgsims <- function(.data,...,.dots,add=FALSE) dplyr::as.tbl(as.data.frame(.data)) %>% dplyr::group_by_(...,.dots=.dots)
##' @export
##' @rdname mrgsims
mutate_.mrgsims <- function(.data,...,.dots) dplyr::as.tbl(as.data.frame(.data)) %>% dplyr::mutate_(...,.dots=.dots)
##' @export
##' @rdname mrgsims
summarise.each <- function(.data,funs,...) dplyr::as.tbl(as.data.frame(.data)) %>% dplyr::summarise_each(funs,...)
##' @export
##' @rdname mrgsims
summarise_.mrgsims <- function(.data,...,.dots) dplyr::as.tbl(as.data.frame(.data)) %>% dplyr::summarise_(...,.dots=.dots)
##' @export
##' @rdname mrgsims
do_.mrgsims <- function(.data,...,.dots) dplyr::as.tbl(as.data.frame(.data)) %>% dplyr::do_(...,.dots=.dots)
##' @export
##' @rdname mrgsims
select_.mrgsims <- function(.data,...,.dots) dplyr::as.tbl(as.data.frame(.data)) %>% dplyr::select_(...,.dots=.dots)
##'
##' @export
##' @rdname mrgsims
slice_.mrgsims <- function(.data,...) dplyr::slice_(as.data.frame(.data),...)
##' @export
##' @rdname mrgsims
setMethod("as.matrix", "mrgsims", function(x,...) return(x@data))
##' @export
##' @rdname mrgsims
setMethod("subset", "mrgsims", function(x,...) {
  subset(as.data.frame(x@data), ...)
})

##' @param object passed to show
##'
##' @export
##' @rdname mrgsims

setMethod("summary", "mrgsims", function(object,...) {
  summary(as.data.frame(object))
})

##' @export
##' @rdname mrgsims
setMethod("show", "mrgsims", function(object) {
    digits <- 4
    top <- head(object@data, n=8)
    tcol <- match(c("time", "TIME"),colnames(object@data))[1]
    cat("Model: ", basename(cfile(mod(object))), "\n")
    cat("Dim:   ", dim(object)[1], "x", dim(object)[2], "\n")
    cat("Time:  ", paste(range(object@data[,tcol]), collapse=" to "), "\n")
    cat("ID:    ", length(unique(object@data[,"ID"])), "\n")
    print(top, digits=digits)
})


##' This function is deprecated.
##'
##'
##' @export
##' @rdname firstonly
firstonly <- function() {
    stop("This function is deprecated.")
}


cfb <- function(x) UseMethod("cfb")
cfb.data.frame <- function(x) {
  time <- x$time
  ID <- x$ID
  y <- lapply(x, function(xx) xx - xx[1])
  y <- data.frame(do.call('cbind', y))
  y$time <- time
  y$ID <- ID
  y
}






##' Generate a quick plot of simulated data.
##'
##' @name plot_mrgsims
##' @param x mrgsims object
##' @param y formula used for plotting
##' @param limit limit the the number of panels to create
##' @param show.grid logical indicating whether or not to draw panel.grid
##' @param as transformations for plotting simulated values
##' @param ylab passed to xyplot
##' @param scales passed to xyplot
##' @param type passed to xyplot
##' @param lwd passed to xyplot
##' @param outer passed to xyplot
##' @param groups passed to xyplot
##' @param ... other arguments passed to xyplot
##' @details Values for \code{as} argument: ;  \code{raw}: raw simulated output;
##' \code{frac}: each observation normalized to baseline value;
##' \code{cfb}: change (difference) from baseline;
##' \code{cfblog}: change from baseline of log10-transformed values;
##' \code{log10y}: log10 transformation; \code{lny}: natural log transformed.
##' @export
##' @rdname plot_mrgsims
##' @aliases plot,mrgsims,missing-method
##' @examples
##'
##' mod <- mrgsolve:::house(end=48, delta=0.2) %>% init(GUT=1000)
##'
##' out <- mrgsim(mod)
##'
##' plot(out)
##'
##' plot(out, subset=time <=24)
##'
##' plot(out, GUT+CP~.)
##'
##' plot(out, CP+RESP~time, col="black", scales="same", lty=2)
##'
setMethod("plot", c("mrgsims","missing"), function(x,limit=16,...) {



  ynames <- variables(x)

  if(length(ynames)==0) {
      message("No variables to plot")
      return(invisible(NULL))

  }

  if(length(ynames)>limit) {
      ynames <- ynames[1:limit]
      if(missing(limit)) warning(paste0("NOTE: show first ",
                                        limit,
                                        " variables.  Check limit argument."
                                        ), call.=FALSE)
  }

  tname <- intersect(c("time", "TIME"), colnames(x@data))[1]
  lhs <- paste(ynames, collapse="+")
  fmla <- as.formula(paste0(lhs, "~", tname))
  plot(x,fmla,limit=limit,...)
})


##' @export
##' @rdname plot_mrgsims
##' @aliases plot,mrgsims,formula-method
setMethod("plot", c("mrgsims","formula"), function(x,y,
                                                   limit=16,show.grid=TRUE,
                                                   as="raw",outer=TRUE,
                                                   type='l',lwd=2,
                                                   ylab="raw value",
                                                   groups=ID,
                                                   scales=list(y=list(relation='free')),
                                                   ...) {
  requireNamespace("lattice", quietly=TRUE)

  data <- as.data.frame(limit(x,...))

  if(y[[3]] == '.')  {

      if(exists("time", data)) y[[3]] <- quote(time)
      if(exists("TIME", data)) y[[3]] <- quote(TIME)
  }


  if(as=="cfb")  {
      data <- split(data, data$ID)
      data <- lapply(data, cfb.data.frame)
      data <- data.frame(do.call("rbind",data))

    ylab <- "Change from baseline"
  }
  if(as=="log10") {
    data[,variables(x)] <- log10(data[,variables(x)])
    ylab="log10 value"
  }
  if(as=="log")   {
    data[,variables(x)] <- log(data[,variables(x)])
    ylab="log value"
  }

 y <- structure(y, .Environment=environment())
 gr <- eval(substitute(groups),data)
  ans <- lattice::xyplot(y,data=data,
                         groups=gr,
                         ylab=ylab,
                         outer=outer,
                         type=type,
                         scales=scales,
                         lwd=lwd,
                         panel=function(...) {
                             if(show.grid) lattice::panel.grid(h=-1,v=-1)
                             lattice::panel.xyplot(...)
                         },...
                         )
  ans
})


submat <- function(x,...) UseMethod("submat")
submat.matrix <- function(x,subset,select=TRUE,...) {

    if(!is.call(subset)) subset <- substitute(subset)

    vars <- all.vars(as.call(subset))

    if(!missing(select)){
        nl <- as.list(1L:ncol(x))
        names(nl) <- colnames(x)
        select <- eval(substitute(select), nl, parent.frame())
    }

    grab <- intersect(vars,colnames(x))
    env <- as.data.frame(x[,grab,drop=FALSE])
    x[eval(subset,env,parent.frame()),select,drop=FALSE]
}


##' Limit the scope of simulated output.
##'
##' @param x mrgsims or batch mrgsims object
##' @param ... passed along
##' @export
setGeneric("limit", function(x,...) standardGeneric("limit"))
##' @param subset rows to keep
##' @param select columns to keep
##' @rdname limit
setMethod("limit", "mrgsims", function(x,subset, select=TRUE,...) {
    if(missing(subset)) return(x)
    if(is.character(select)) select <- as.cvec(select)
    x@data <- submat(x@data,substitute(subset),select)
    return(x)
})

same_data <- function(x,y) {
    identical(x@data, y@data)
}





