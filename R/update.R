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



setAs("NULL", "character", function(from) character(0))

sval <- unique(c("atol","rtol",
                 "events","verbose","debug","preclean","mindt",
                 "digits", "ixpr", "mxhnil","start", "end", "add", "delta",
                 "maxsteps", "hmin", "hmax","tscale", "request"))


##' Update the model object.
##'
##' After the model object is created, update various attributes.
##'
##' @param object a model object
##' @param ... passed to other functions
##' @param merge logical indicating to merge (rather than replace) new and
##' existing attributes.
##' @param open logical; used only when merge is \code{TRUE} and parameter list or initial conditions
##' list is being updated; if \code{FALSE}, no new items will be added; if \code{TRUE}, the parameter list may
##' expand.
##' @param data a list of items to update; not used for now
##' @return The updated model object is returned.
##' @export
##' @name update
##' @aliases update,mrgmod-method
##' @examples
##' \dontrun{
##'  mod <- mrgsolve:::house()
##'
##'  mod <- update(mod, end=120, delta=4, param=list(CL=19.1))
##'  }
setMethod("update", "mrgmod", function(object,..., merge=TRUE,open=FALSE,data=list()) {

    x <- object

    args <- list(...)

    if(!is.mt(data)) args <- merge(args,data,open=TRUE)

    if(is.mt(args)) return(x)

    args <- args[!is.na(args)]

    a <- names(args)
    valid.in <- which(charmatch(a,sval, nomatch=0)>0)
    if(length(valid.in)>0) {
        valid.full <- charmatch(a[valid.in],sval, nomatch=0)
        for(i in 1:length(valid.in)) {
            slot(x, sval[valid.full[i]]) <- args[[valid.in[i]]]
        }
    }

    ## If we're not merging, just replace and return:
    if(!merge) {
        if(has_name("init",args)) {
            stop("Error... initial conditions list (init) is only updateable when merge=TRUE.")
        }
        if(has_name("param",args)) {
            x@param <- as.param(args$param)
        }
        validObject(x)
        return(x)
    }

    ## Otherwise, merge if arguments are there:
    ## Initial conditions list:
    if(has_name("init",args)) {
        i <- x@init@data
        i <- merge(i, args$init, context="init", open=open)
        slot(x, "init") <- as.init(i)
    }
    ## Parameter update:
    if(has_name("param",args)) {
        if(length(x@fixed)>0) {
            if(any(is.element(names(args$param),names(x@fixed)))) {
              warning("Attempted update of a $FIXED parameter.", call.=FALSE,immediate.=TRUE)
            }
        }
        x@param <- as.param(merge(x@param@data,args$param,open=open,context="param"))
    }
    
    if(exists("omega", args)) {
        x@omega <- update_matlist(x@omega,omat(args$omega),open=open, context="omat")
    }
    
    if(exists("sigma", args)) {
        x@sigma <- update_matlist(x@sigma,smat(args$sigma), open=open, context="smat")
    }

    validObject(x)
    return(x)
})

same_sig <- function(x,y) {
    return(identical(unname(nrow(x)), unname(nrow(y))))
}

update_matlist <-  function(x,y,open=FALSE,context="update_matlist",...) {

    n0 <- dim_matlist(x)

    if(length(x)==0) stop(paste0(context, ": there is no matrix to update"))

    anon <- all(names(y)=="...")
    ss <- same_sig(x,y)
    if(anon & !ss) stop(paste("Improper signature:", context), call.=FALSE)

    if(ss & anon) {
        ## If we match the sig and all input is unnamed
        labels <- names(x@data)
        x@data <- y@data
        names(x@data) <- labels

    } else {
        ##if(anon & all(names(x)=="...")) stop(paste("Improper signature:",context), call.=FALSE)
        x@data <- merge(x@data, y@data,open=open,context=context,...)
    }

    n <- dim_matlist(x)

    if(open) {
        x@n <- n
    } else {
        if(!identical(n0,n)) stop(paste("Improper dimension:",context), call.=FALSE)
    }

    validObject(x)

    return(x)
}


##' @export
##' @rdname update
##' @param y another object involved in update
setMethod("update", "omegalist", function(object,y,...) {
    update_matlist(object, omat(y),context="omat",...)
})
##' @export
##' @rdname update

setMethod("update", "sigmalist", function(object,y,...) {
    update_matlist(object, smat(y),context="smat",...)
})
##' @export
##' @rdname update
setMethod("update", "parameter_list", function(object,y,...) {
    as.param(merge(object@data, as.param(y)@data,context="param",...))
})

##' @export
##' @rdname update
setMethod("update", "ev", function(object,y,...) {

})




##' Update model or project in an model object.
##'
##' @param x mrgmod object
##' @param model model name
##' @param project project directory
##' @param ... passed along
##' @export
##' @return updated model object
setGeneric("relocate", function(x,...) standardGeneric("relocate"))
##' @export
##' @rdname relocate
setMethod("relocate", "mrgmod", function(x,model=NULL, project=NULL) {
    if(!missing(model)) x@model <- model
    if(!missing(project)) x@project <- normalizePath(project,winslash=.Platform$file.sep)
    validObject(x)
    return(x)
})





