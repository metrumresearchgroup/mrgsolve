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




##' Create and work with parameter objects.
##'
##' See \code{\link{numericlist}} for methods to deal with \code{parameter_list} objects.
##'
##'
##' @param .x the model object
##' @param .y list to be merged into parameter list
##' @param .pat a regular expression (character) to be applied as a filter for which parameters to show when printing
##' @param .strict if \code{TRUE}, all names to be updated must be found in the parameter list
##' @param object passed to show
##' @param ... passed along or name/value pairs to update the parameters in a model object
##' @return An object of class \code{parameter_list} (see \code{\link{numericlist}}).
##'
##' @details Can be used to either get a parameter list object from a \code{mrgmod} model object or to update
##' the parameters in a model object.  For both uses, the return value is a \code{parameter_list} object.
##' For the former use, \code{param} is usually called to print
##' the parameters to the screen, but the parameter_list object can also be coreced to a list or numeric R object.
##'
##'
##'@examples
##' ## example("param")
##' mod <- mrgsolve:::house()
##'
##' param(mod)
##' param(mod, .pat="^(C|F)") ## may be useful when large number of parameters
##'
##' class(param(mod))
##'
##' param(mod)$KA
##'
##' as.list(param(mod))
##' as.data.frame(param(mod))
##'
##' @keywords param
setGeneric("param", function(.x,...) standardGeneric("param"))

##' @export
##' @rdname param
setMethod("param", c("mrgmod"), function(.x,.y=list(),...,.pat="*",.strict=FALSE) {
  
  args <- c(as.list(.y),list(...))
  
  if(length(args)==0) {
    slot(.x@param, "pattern")  <- .pat
    return(.x@param)
  }
  
  if((length(args)-length(.y)) > 0 & missing(.strict))  {
    .strict <- TRUE
  }
  
  if(.strict) {
    if(!all(names(args) %in% pars(.x))) {
      foreign <- setdiff(names(args),pars(.x))
      foreign <- paste(foreign, collapse=", ")
      stop("\nNames not found in the parameter list:\n", "  ", 
           foreign, call.=FALSE) 
    } 
  }
  
  return(update(.x,param=args))
})

##' @rdname param
##' @export
setMethod("param", "mrgsims", function(.x,...) {
  param(mod(.x),...)
})

##' @export
##' @rdname param
setMethod("param", c("missing"), function(...,.strict=TRUE) {
  param(list(...),.strict=.strict)
})

##' @rdname param
##' @export
setMethod("param", "list", function(.x,...) {
  create_numeric_list(.x,"parameter_list",...)
})

##' @rdname param
##' @export
setMethod("param", "ANY", function(.x,...) {
  param(as.list(.x),...)
})

##' @export
##' @rdname param
setGeneric("as.param", function(.x,...) standardGeneric("as.param"))

##' @export
##' @rdname param
setMethod("as.param", "list", function(.x,...) {
  create_numeric_list(.x,"parameter_list",...)
})

##' @export
##' @rdname param
setMethod("as.param", "numeric", function(.x,...) {
  create_numeric_list(as.list(.x),"parameter_list",...)
})

##' @export
##' @rdname param
setMethod("as.param", "parameter_list", function(.x,...) .x)

##' @export
##' @rdname param
setMethod("as.param", "missing", function(.x,...) {
  create_numeric_list(list(), "parameter_list",...)
})

showparam <- function(x,right=FALSE,digits=3,ncols=NULL,...) {
  pattern <- x@pattern
  ini <- as.list(x)
  showN <- length(ini)
  
  if(length(ini)==0) {
    message("No parameters in the model.")
    return(invisible(NULL))
  }
  
  x <- names(ini)
  y <- prettyNum(unlist(ini), digits=digits)
  NPARAM <- length(x)
  n <- 1:NPARAM
  
  if(pattern !="*") {
    take <- grepl(pattern,x)
    x <- x[take]
    y <- y[take]
    n <- n[take]
  }
  ##if(length(x)==0) stop("No parameters to show with pat = ", pattern)
  if(length(x)==0) {
    message("No matching parameters were found.")
    return(invisible(NULL))
  }
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  n <- n[ord]
  
  if(missing(ncols)) ncols <- ifelse(NPARAM > 30, 3,2)
  
  N <- ceiling(length(x)/ncols)
  target <- N*ncols
  x <- c(x, rep(".", target-length(x)))
  n <- c(n, rep(".", target-length(n)))
  y <- c(y, rep(".", target-length(y)))
  x <- gsub("\\(\\.\\)", "...", x)
  grps <- ceiling(seq_along(x)/N)
  x <- split(x,grps)
  x <- data.frame(do.call('cbind',x))
  y <- split(y,grps)
  y <- data.frame(do.call('cbind',y))
  sep <- data.frame(`.` = rep("|", N))
  df <- cbind(x[,1],y[,1],sep)
  for(i in 2:ncol(x)) {
    df <- cbind(df,x[,i], y[,i],sep)
  }
  names(df) <- rep(c("name", "value", "."), ncols)
  df[,ncol(df)] <- NULL
  output <- df
  cat("\n Model parameters (N=", showN, "):\n", sep="")
  print(output, row.names=FALSE, right=right)
  return(invisible(NULL))
}

##' @export
##' @rdname param
setMethod("show", "parameter_list", function(object) showparam(object))

##' @export
##' @rdname param
allparam <- function(.x) {
  as.param(c(as.list(param(.x)), .x@fixed))
}

as.fixed <- function(x) {
  as.list(x)
}
