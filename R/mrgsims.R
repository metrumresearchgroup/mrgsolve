# Copyright (C) 2013 - 2020  Metrum Research Group
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
##' mod <- mrgsolve::house() %>% init(GUT=100)
##'
##' out <- mrgsim(mod)
##' 
##' class(out)
##'
##' \dontrun{
##' out
##' }
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
##' \dontrun{
##' out$CP
##' }
##'
##' plot(out)
##' \dontrun{
##' plot(out, CP~.)
##' plot(out, CP+RESP~time, scales="same", xlab="Time", main="Model sims")
##' }
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

#' Methods for handling output with dplyr verbs
#' 
#' These methods modify the data in a mrgsims object and return a data frame.
#' Contrast with the functions in [mrgsims_modify].
#' 
#' @rdname mrgsims_dplyr
#' @name mrgsims_dplyr
#' @md
#' 
NULL

#' @param .dots passed to various `dplyr` functions
#' @param .data an mrgsims object; passed to various `dplyr` functions
#' @param x passed to [dplyr::as.tbl]
#' @param add passed to [dplyr::group_by] (for dplyr < `1.0.0`)
#' @param .add passed to [dplyr::group_by] (for dplyr >= `1.0.0`)
#' @param .keep_all passed to [dplyr::distinct]
#' @param funs passed to [dplyr::summarise_each]
#' @param ... passed to other methods
#' 
#' @details
#' 
#' For the `select_sims` function, the dots `...` must be either 
#' compartment names or variables in `$CAPTURE`.  An error will be
#' generated if no valid names are selected or the names for selection are 
#' not found in the simulated output.
#' 
#' @examples
#' 
#' out <- mrgsim(house(), events = ev(amt = 100), end = 5, delta=1)
#' 
#' dplyr::filter(out, time==2)
#' 
#' dplyr::mutate(out, label = "abc")
#' 
#' dplyr::select(out, time, RESP, CP)
#' 
#' @seealso [mrgsims_modify]
#' @md
#' @rdname mrgsims_dplyr
#' @export
pull.mrgsims <- function(.data, ...) {
  dplyr::pull(as_tibble.mrgsims(.data), ...)
}

##' @method filter mrgsims
##' @rdname mrgsims_dplyr
##' @export 
filter.mrgsims <- function(.data,...) {
  dplyr::filter(as_tibble.mrgsims(.data),...)
}

##' @rdname mrgsims_dplyr
##' @export
group_by.mrgsims <- function(.data,...,add=FALSE,.add=FALSE) {
  if(DPLYR_1_0_0) {
    return(dplyr::group_by(as_tibble.mrgsims(.data), ..., .add = .add))
  } else {
    return(dplyr::group_by(as_tibble.mrgsims(.data), ..., add = add))
  }
}

##' @rdname mrgsims_dplyr
##' @export
distinct.mrgsims <- function(.data,...,.keep_all=FALSE) {
  dplyr::distinct(as_tibble.mrgsims(.data),...,.keep_all=.keep_all)
}

##' @rdname mrgsims_dplyr
##' @export
mutate.mrgsims <- function(.data,...) {
  dplyr::mutate(as_tibble.mrgsims(.data),...)
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
##' @export
as_data_frame.mrgsims <- function(x,...) {
  as_tibble(as.data.frame(x),...)
}


#' @param x mrgsims object.
#' 
#' @rdname mrgsims_dplyr
#' @export
as_tibble.mrgsims <- function(x, ...) {
  as_tibble(as.data.frame(x), ...)  
}

#' @rdname mrgsims_dplyr
#' @export
as.tbl.mrgsims <- function(x,...) {
  as_tibble(as.data.frame(x))
}

#' Methods for modifying mrgsims objects
#' 
#' These functions modify the simulated data in an mrgsims object and return 
#' the modified object.  Contrast with the functions in [mrgsims_dplyr].
#' 
#' @param .data a mrgsims object
#' @param ... other arguments passed to the `dplyr` functions
#' 
#' @examples
#' 
#' out <- mrgsim(house(), events = ev(amt = 100))
#' 
#' filter_sims(out, time > 2)
#' 
#' mutate_sims(out, label = "abc")
#' 
#' select_sims(out, RESP, CP)
#' 
#' @rdname mrgsims_modify
#' @name   mrgsims_modify
#' @seealso [mrgsims_dplyr]
#' @md
#' @export
#' 
mutate_sims <- function(.data, ...) {
  .data@data <- dplyr::mutate(.data@data, ...)
  .data
}

#' @rdname mrgsims_modify
#' @export
select_sims <- function(.data, ...) {
  all_names <- names(.data)
  outputs <- c(.data@request,.data@outnames)
  retain <- setdiff(all_names,outputs)
  vars <- vars_select(all_names, !!!enquos(...))
  vars <- intersect(vars,outputs)
  if(length(vars)==0) {
    wstop("no output variables (compartments or captures) were selected.")  
  }
  vars <- unique(c(retain,vars))
  .data@data <- dplyr::select(.data@data,vars)
  .data@request <- intersect(.data@request,names(.data))
  .data@outnames <- intersect(.data@outnames,names(.data))
  return(.data)
}

#' @rdname mrgsims_modify
#' @export
filter_sims <- function(.data, ... ) {
  .data@data <- dplyr::filter(.data@data, ...)
  .data
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
summary.mrgsims <- function(object,...) {
  summary(as.data.frame(object))
}

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
##' mod <- mrgsolve::house(end=48, delta=0.2) %>% init(GUT=1000)
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
##' \dontrun{
##' plot(out, "CP RESP, GUT")
##' }
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
                                                   logbr = 1,
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
  
  if(length(y[[2]])==1 & missing(ylab)) ylab <- deparse(y[[2]])
  
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

#' @rdname plot_mrgsims
#' @aliases plot,mrgsims,formula-method
#' @export
setMethod("plot", c("mrgsims","character"), function(x,y,...) {
  y <- gsub("\n+", " ", y)
  y <- cvec_cs(y)
  time <- timename(x@data)
  lhs <- paste0(y,collapse="+")
  fm <- as.formula(paste0(lhs,"~",time))
  plot(x,fm,...)
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
##' mod <- mrgsolve::house() %>% ev(amt = 100)
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
  vars <- vars_select(names(.data), `!!!`(vars))
  .dots$x <- new("mrgsims", data = .data, outnames = vars)
  if(rlang::is_formula(.f)) {
    if(length(.f)==2) .f[[3]] <- as.symbol(".")
    .dots$y <- .f
    return(do.call(plot, .dots))
  }
  return(do.call(plot, .dots))
}
