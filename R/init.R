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


##' Methods for working with the model compartment list.
##'
##' Calling \code{init} with the model object as the first argument will return the model initial conditions as a \code{numericlist} object. See \code{\link{numericlist}} for methods to  deal with \code{cmt_list} objects.
##'
##' @aliases init 
##' @param .x the model object
##' @param .y list to be merged into parameter list
##' @param .pat a regular expression (character) to be applied as a filter when printing compartments to the screen
##' @param ... passsed along
##' @return an object of class \code{cmt_list} (see \code{\link{numericlist}})
##' @details
##' Can be used to either get a compartment list object from a \code{mrgmod} model object or to update
##' the compartment initial conditions in a model object.  For both uses, the return value is a \code{cmt_list} object.
##' For the former use, \code{init} is usually called to print
##' the compartment initial conditions to the screen, but the \code{cmt_list} object can also be coreced to
##' a list or numeric R object.
##'
##' @examples
##' ## example("init")
##' mod <- mrgsolve:::house()
##'
##' init(mod)
##' init(mod, .pat="^C") ## may be useful for large models
##'
##' class(init(mod))
##'
##' init(mod)$CENT
##'
##' as.list(init(mod))
##' as.data.frame(init(mod))

##' @export
setGeneric("init", function(.x,...) standardGeneric("init"))


##' @export
##' @rdname init
setMethod("init", "mrgmod", function(.x,.y=list(),..., .pat="*") {

    args <- c(as.list(.y),list(...))
    
    if(length(args)>0) return(update(.x,init=args))

    .x@init@pattern <- .pat

    if(!main_loaded(.x)) {
      return(.x@init)
    }

    if(neq(.x)==0) return(as.init())

    ## Otherwise call function:
    .x@init <- as.init(touch_funs(.x)$init)
    slot(.x@init,"pattern") <- .pat
    return(.x@init)
})

##' @export
##' @rdname init
setMethod("init", "mrgsims", function(.x,...) {init(mod(.x),...)})

##' @export
##' @rdname init
setMethod("init", "missing", function(...) init(list(...)))

##' @export
##' @rdname init
setMethod("init", "list", function(.x,...) {create_numeric_list(.x,"cmt_list",...)})

##' @export
##' @rdname init
setMethod("init", "ANY", function(.x,...) init(as.list(.x),...))

Init <- function(x) x@init
Cmt <-  function(x) names(x@init@data)

##' @export
##' @rdname init
setGeneric("as.init", function(.x,...) standardGeneric("as.init"))

##' @export
##' @rdname init
setMethod("as.init", "list", function(.x,...) create_numeric_list(.x,"cmt_list",...))

##' @export
##' @rdname init
setMethod("as.init", "numeric", function(.x,...) create_numeric_list(as.list(.x),"cmt_list",...))

##' @export
##' @rdname init
setMethod("as.init", "cmt_list", function(.x,...) .x)

##' @export
##' @rdname init
setMethod("as.init", "missing", function(.x,...) create_numeric_list(list(), "cmt_list",...))

##' @export
##' @rdname init
setMethod("as.init", "NULL", function(.x,...) create_numeric_list(list(), "cmt_list",...))



showinit <-  function(x,digits=3,ncols=NULL,right=FALSE,...) {
    if(is.mt(x@data)) {
        message("No compartments in the model.")
        return(invisible(NULL))
    }

    pattern <- x@pattern

    ini <- as.list(x)

    showN <- length(ini)

    x <- names(ini)
    y <- prettyNum(unlist(ini), digits=digits)
    n <- 1:length(x)
    if(pattern !="*") {
        take <- grepl(pattern,x)
        x <- x[take]
        y <- y[take]
        n <- n[take]
    }
    ##if(length(x)==0) stop("No initial conditions to show with pat = ", pattern)
    if(length(x)==0) {message("No matching compartments were found."); return(invisible(NULL))}
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
    n <- n[ord]

    if(is.null(ncols)) {
        ncols <- ifelse(length(x) > 20, 3,2)
    }

    N <- ceiling(length(x)/ncols)
    target <- N*ncols
    x <- c(x, rep(".", target-length(x)))
    n <- c(n, rep(".", target-length(n)))
    y <- c(y, rep(".", target-length(y)))
    x <- paste(x, " (", n, ")  ", sep="")
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
    cat("\n Model initial conditions (N=", showN, "):\n", sep="")
    print(output, row.names=FALSE, right=right)
    return(invisible(NULL))

}

##' @rdname init
##' @export
##' @param object to show
setMethod("show", "cmt_list", function(object) showinit(object))

