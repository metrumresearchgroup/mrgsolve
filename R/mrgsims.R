# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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


##' Methods for working with \code{mrgsims} objects
##'
##' These methods help the user view simulation output and extract 
##' simulated data to work with further.  The methods listed here 
##' for the most part have generics defined by R or other R packages.  
##' See the \code{seealso} section for other methods defined 
##' by \code{mrgsolve} that have their own documentation pages.
##'
##' @details
##' Most methods should behave as expected according to other method 
##' commonly used in R (e.g. head, tail, as.data.frame, etc ...)
##'
##' \itemize{
##'   \item{\code{$}} selects a column in the simulated data and 
##'   returns numeric
##'   \item{\code{head}} see \code{\link{head.matrix}}; returns 
##'   simulated data
##'   \item{\code{tail}} see \code{\link{tail.matrix}}; returns 
##'   simulated data
##'   \item{\code{dim}, \code{nrow}, \code{ncol}} returns dimensions, 
##'   number of rows, and number of columns in simulated data
##'   \item{\code{as.data.frame}} coerces simulated data to data.frame 
##'   and returns the data.frame
##'   \item{\code{as.matrix}} returns matrix of simulated data
##'   \item{\code{as.tbl}} coerces simulated to \code{tbl_df}; 
##'   requires \code{dplyr}
##'   \item{\code{summary}} coerces simulated data to data.frame 
##'   and passes to \code{\link{summary.data.frame}}
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
##'
##' dim(out)
##' names(out)
##'
##' mat <- as.matrix(out)
##' df <- as.data.frame(out)
##'
##' out$CP
##'
##' plot(out)
##' plot(out, CP~.)
##' plot(out, CP+RESP~time, scales="same", xlab="Time", main="Model sims")
##'
NULL


##' @rdname mod
##' @keywords internal
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
  if(!is.element(name, colnames(x@data))) {
    stop("Couldn't find column ", name, " in simulated data")
  }
  return(x@data[,name])
})

##' @rdname mrgsims
##' @export
setMethod("tail", "mrgsims", function(x,...) {
  return(tail(x@data,...))
})

##' @rdname mrgsims
##' @export
setMethod("head", "mrgsims", function(x,...) {
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

##' Methods for handling output with dplyr verbs
##' 
##' @rdname mrgsims_dplyr
##' @name mrgsims_dplyr
##' 
NULL

##' @param x mrgsims object
##' @param .dots passed to various \code{dplyr} functions
##' @param .data passed to various \code{dplyr} functions
##' @param add passed to \code{dplyr::group_by}
##' @param .keep_all passed to \code{dplyr::distinct}
##' @param funs passed to \code{dplyr::summarise_each}
##' @param ... passed to other methods
##' @rdname mrgsims_dplyr
##' @export
as.tbl.mrgsims <- function(x,...) {
  as.tbl(as.data.frame(x))
}

##' @rdname mrgsims_dplyr
##' @export
pull.mrgsims <- function(.data, ...) {
  dplyr::pull(as_tibble.mrgsims(.data), ...)
}

##' @rdname mrgsims_dplyr
##' @export
filter_.mrgsims <- function(.data,...) {
  dplyr::filter_(as_tibble.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @export
filter_sims <- function(.data, ... ) {
  .data@data <- dplyr::filter(.data@data, ...)
  .data
}

##' @rdname mrgsims_dplyr
##' @export
group_by.mrgsims <- function(.data,...,add=FALSE) {
  dplyr::group_by(as_tibble.mrgsims(.data),...,add = add)
}

##' @rdname mrgsims_dplyr
##' @export
distinct.mrgsims <- function(.data,...,.keep_all=FALSE) {
  dplyr::distinct(as_tibble.mrgsims(.data),...,
                  .keep_all=.keep_all)
}

##' @rdname mrgsims_dplyr
##' @export
mutate.mrgsims <- function(.data,...) {
  dplyr::mutate(as_tibble.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @export
mutate_sims <- function(.data, ...) {
  .data@data <- dplyr::mutate(.data@data, ...)
  .data
}

##' @rdname mrgsims_dplyr
##' @export
summarise.each <- function(.data,funs,...) {
  dplyr::summarise_each(as_tibble.mrgsims(.data),funs,...)
}

##' @rdname mrgsims_dplyr
##' @export
summarise.mrgsims <- function(.data,...) {
  dplyr::summarise(as_tibble.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @export
do.mrgsims <- function(.data,...,.dots) {
  dplyr::do(as_tibble.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @export
select.mrgsims <- function(.data,...) {
  dplyr::select(as_tibble.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @export
slice.mrgsims <- function(.data,...) {
  dplyr::slice(as_tibble.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @param .data_ mrgsims object
##' @export
as_data_frame.mrgsims <- function(.data_,...) {
  as_tibble(as.data.frame(.data_),...)
}

##' @rdname mrgsims_dplyr
##' @export
as_tibble.mrgsims <- function(.data_,...) {
  as_tibble(as.data.frame(.data_),...)  
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
##' @keywords internal
setMethod("as.matrix", "mrgsims", function(x,...) {
  return(as.matrix(x@data))
})

##' @param object passed to show
##' @rdname mrgsims
##' @export
setMethod("summary", "mrgsims", function(object,...) {
  summary(as.data.frame(object))
})

##' @rdname mrgsims
##' @export
##' @keywords internal
setMethod("show", "mrgsims", function(object) {
  digits <- 4
  n <- min(8,nrow(object@data))
  top <- object@data[seq_len(n),,drop=FALSE]
  rownames(top) <- paste0(seq_len(n), ": ")
  tcol <- timename(object@data)
  cat("Model: ", model(mod(object)), "\n")
  cat("Dim:   ", dim(object)[1], "x", dim(object)[2], "\n")
  cat("Time:  ", paste(round(range(object@data[,tcol]),2), collapse=" to "), "\n")
  cat("ID:    ", length(unique(object@data[,"ID"])), "\n")
  print(top, digits=digits)
})


##' Generate a quick plot of simulated data
##'
##' @name plot_mrgsims
##' 
##' @param x mrgsims object
##' @param y formula used for plotting
##' @param limit limit the the number of panels to create
##' @param show.grid logical indicating whether or not to draw panel.grid
##' @param ylab passed to xyplot
##' @param scales passed to xyplot
##' @param logy plot the y variables on log scale
##' @param logbr log scale breaks indicator; use `1` for breaks every log
##' unit; use `3` for breaks every half log unit; use `0` for default 
##' breaks
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
##' @md
##' @export
setMethod("plot", c("mrgsims","missing"), function(x,limit=16,...) {
  
  ynames <- variables(x)
  
  if(length(ynames)==0) {
    message("No variables to plot")
    return(invisible(NULL))
    
  }
  
  if(length(ynames)>limit) {
    ynames <- ynames[1:limit]
    if(missing(limit)) {
      warning(paste0("NOTE: show first ",
                     limit,
                     " variables.  Check limit argument."
      ), call.=FALSE)
    }
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
                                                   logy = FALSE,
                                                   logbr = 3,
                                                   ...) {
  requireNamespace("lattice", quietly=TRUE)
  
  data <- as.data.frame(subset(as.data.frame(x),...))
  
  if(length(y)==2) y[[3]] <- as.symbol(".")
  
  if(!has_name("time", data)) {
    if(!has_name("TIME", data)) {
      stop("Couldn't find time or TIME column.",call.=FALSE)
    }
    # Must mutate here; not rename
    data <- dplyr::mutate(data,time=TIME)
  }
  
  if(y[[3]] == '.')  y[[3]] <- quote(time)
  
  if(length(y[[2]])==1) ylab <- deparse(y[[2]])
  
  if(logy) {
    scales[["y"]][["log"]] <- TRUE
    if(!logbr %in% c(0,1,3)) {
      stop("'logbr' must be either 0, 1, or 3.", call.=FALSE)  
    }
    if(logbr > 0) {
      breaks <- 10^seq(-10,10)
      if(logbr==3) breaks <- sort(c(breaks,3*breaks))
      scales[["y"]][["at"]] <- breaks
    }
  }

  y <- structure(y, .Environment=environment())
  gr <- eval(substitute(groups),data)
  ans <- lattice::xyplot(
    y,
    data=data,
    groups=gr,
    ylab=ylab,
    outer=outer,
    type=type,
    scales=scales,
    lwd=lwd,
    strip = lattice::strip.custom(style=1,bg="grey85"),
    panel=function(...) {
      if(show.grid) lattice::panel.grid(h=-1,v=-1)
      lattice::panel.xyplot(...)
    },...
  )
  ans
})


##' Plot data as an mrgsims object
##' 
##' @param .data a data frame
##' @param ... unquoted column names to plot on y-axis
##' @param .f a formula to plot
##' @param .dots extra arguments passed to \code{lattice::xyplot}
##' 
##' @details 
##' This function is only intended for use with data frames that 
##' were created by modifying an \code{mrgsims} object.
##' 
##' @examples
##'
##' mod <- mrgsolve:::house() %>% ev(amt = 100)
##' 
##' out <- mrgsim(mod) 
##' out_df <- dplyr::mutate(out, time <= 72)
##' 
##' plot(out)
##' plot_sims(out, CP, RESP)
##' 
##' \dontrun{
##' plot_sims(out, .f = ~ CP + RESP)
##' plot_sims(out, .f = CP + RESP ~ time)
##' }
##' 
##' @export
plot_sims <- function(.data, ..., .f = NULL, .dots = list()) {
  .data <- as.data.frame(.data)
  vars <- quos(...)
  vars <- select_vars(names(.data), `!!!`(vars))
  .dots$x <- new("mrgsims", data = .data, outnames = vars)
  if(rlang::is_formula(.f)) {
    if(length(.f)==2) .f[[3]] <- as.symbol(".")
    .dots$y <- .f
    return(do.call(plot, .dots))
  }
  return(do.call(plot, .dots))
}
