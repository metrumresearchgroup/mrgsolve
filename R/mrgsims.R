# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.


##' Methods for working with \code{mrgsims} objects.
##'
##' These methods help the user view simulation output and extract simulated data to work with further.  The methods listed here for the
##' most part have generics defined by R or other R packages.  See the \code{seealso} section for other methods defined by \code{mrgsolve}
##' that have their own documentation pages.
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
##' @seealso  \code{\link{stime}}
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
##' mrgsolve:::mod(out)
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
NULL


##' @rdname mod
setMethod("mod", "mrgsims", function(x,...) {x@mod})

request <- function(x) x@request

variables <- function(x) {
  stopifnot(is.mrgsims(x))
  return(c(x@request,x@outnames))
}

##' @param name name of column of simulated output to retain
##' @rdname mrgsims
##' @export
setMethod("$", "mrgsims", function(x,name) {
  if(!is.element(name, colnames(x@data))) stop("Couldn't find column ", name, " in simulated data")
  return(x@data[,name])
})

##' @rdname mrgsims
##' @export
setMethod("tail", "mrgsims",function(x,...) {
  cat("Model: ", model(mod(x)), "\n")
  return(tail(x@data,...))
})

##' @rdname mrgsims
##' @export
setMethod("head", "mrgsims",function(x,...) {
  cat("Model: ", model(mod(x)), "\n")
  return(head(x@data,...))
})

##' @rdname mrgsims
##' @export
setMethod("dim", "mrgsims", function(x) {
  return(dim(x@data))
})

##' @rdname mrgsims
##' @export
setMethod("names", "mrgsims", function(x) {
  return(colnames(x@data))
})

##' Methods for handling output with dplyr verbs.
##' 
##' @rdname mrgsims_dplyr
##' @name mrgsims_dplyr
##' 
NULL

##' @param x mrgsims object
##' @param .dots passed to various \code{dplyr} functions
##' @param .data passed to various \code{dplyr} functions
##' @param add passed to \code{dplyr::group_by_}
##' @param .keep_all passed to \code{dplyr::distinct_}
##' @param funs passed to \code{dplyr::summarise_each}
##' @param ... passed to other methods
##' @rdname mrgsims_dplyr
##' @export
as.tbl.mrgsims <- function(x,...) {
  dplyr::as.tbl(as.data.frame(x))
}

##' @rdname mrgsims_dplyr
##' @export
filter_.mrgsims <- function(.data,...,.dots) {
    dplyr::filter_(as_data_frame.mrgsims(.data),...,.dots=.dots)
}

##' @rdname mrgsims_dplyr
##' @export
group_by_.mrgsims <- function(.data,...,.dots,add=FALSE) {
    dplyr::group_by_(as_data_frame.mrgsims(.data),...,.dots=.dots)
}
##' @rdname mrgsims_dplyr
##' @export
distinct_.mrgsims <- function(.data,...,.dots,.keep_all=FALSE) {
    dplyr::distinct_(as_data_frame.mrgsims(.data),...,.dots=.dots,.keep_all=.keep_all)
}

##' @rdname mrgsims_dplyr
##' @export
mutate_.mrgsims <- function(.data,...,.dots) {
    dplyr::mutate_(as_data_frame.mrgsims(.data),...,.dots=.dots)
}
##' @rdname mrgsims_dplyr
##' @export
summarise.each <- function(.data,funs,...) {
    dplyr::summarise_each(as_data_frame.mrgsims(.data),funs,...)
}
##' @rdname mrgsims_dplyr
##' @export
summarise_.mrgsims <- function(.data,...,.dots) {
    dplyr::summarise_(as_data_frame.mrgsims(.data),...,.dots=.dots)
}
##' @rdname mrgsims_dplyr
##' @export
do_.mrgsims <- function(.data,...,.dots) {
    dplyr::do_(as_data_frame.mrgsims(.data),...,.dots=.dots)
}
##' @rdname mrgsims_dplyr
##' @export
select_.mrgsims <- function(.data,...,.dots) {
    dplyr::select_(as_data_frame.mrgsims(.data),...,.dots=.dots)
}

##' @rdname mrgsims_dplyr
##' @export
slice_.mrgsims <- function(.data,...) {
  dplyr::slice_(as_data_frame.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @param .data_ mrgsims object
##' @export
as_data_frame.mrgsims <- function(.data_,...) {
  tibble::as_data_frame(as.data.frame(.data_),...)
}

##' @rdname mrgsims
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
##' @export
setMethod("as.data.frame", "mrgsims", function(x,row.names=NULL, optional=FALSE,...) {
  return(as.data.frame(x@data,row.names,optional,...))
})

##' @export
##' @rdname mrgsims
##' @export
setMethod("as.matrix", "mrgsims", function(x,...) return(as.matrix(x@data)))

##' @rdname mrgsims
##' @export
setMethod("subset", "mrgsims", function(x,...) {
  subset(as.data.frame(x@data), ...)
})

##' @param object passed to show
##' @rdname mrgsims
##' @export
setMethod("summary", "mrgsims", function(object,...) {
  summary(as.data.frame(object))
})

##' @rdname mrgsims
##' @export
setMethod("show", "mrgsims", function(object) {
  digits <- 4
  n <- min(8,nrow(object@data))
  top <- data.matrix(object@data[seq_len(n),,drop=FALSE],rownames.force=FALSE)
  tcol <- timename(object@data)
  cat("Model: ", basename(cfile(mod(object))), "\n")
  cat("Dim:   ", dim(object)[1], "x", dim(object)[2], "\n")
  cat("Time:  ", paste(range(object@data[,tcol]), collapse=" to "), "\n")
  cat("ID:    ", length(unique(object@data[,"ID"])), "\n")
  print(top, digits=digits)
})



##' Generate a quick plot of simulated data.
##'
##' @name plot_mrgsims
##' 
##' @param x mrgsims object
##' @param y formula used for plotting
##' @param limit limit the the number of panels to create
##' @param show.grid logical indicating whether or not to draw panel.grid
##' @param ylab passed to xyplot
##' @param scales passed to xyplot
##' @param type passed to xyplot
##' @param lwd passed to xyplot
##' @param outer passed to xyplot
##' @param groups passed to xyplot
##' @param ... other arguments passed to xyplot
##'
##' @rdname plot_mrgsims
##' 
##' @aliases plot,mrgsims,missing-method
##' 
##' @details Values for \code{as} argument: ;  \code{raw}: raw simulated output;
##' 
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
##' @export
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
  
  tname <- timename(x@data)
  lhs <- paste(ynames, collapse="+")
  fmla <- as.formula(paste0(lhs, "~", tname))
  plot(x,fmla,limit=limit,...)
})

##' @rdname plot_mrgsims
##' @aliases plot,mrgsims,formula-method
##' @export
setMethod("plot", c("mrgsims","formula"), function(x,y,
                                                   limit=16,show.grid=TRUE,
                                                   outer=TRUE,
                                                   type='l',lwd=2,
                                                   ylab="value",
                                                   groups=ID,
                                                   scales=list(y=list(relation='free')),
                                                   ...) {
  requireNamespace("lattice", quietly=TRUE)
  
  data <- as.data.frame(subset(x,...))
  
  if(!has_name("time", data)) {
    if(!has_name("TIME", data)) stop("Couldn't find time or TIME column.",call.=FALSE)
    # Must mutate here; not rename
    data <- data %>% dplyr::mutate(time=TIME)
  }
  
  if(y[[3]] == '.')  y[[3]] <- quote(time)
  
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

##' @rdname events
##' @export
setMethod("events", "mrgsims", function(x,...) events(mod(x)))

