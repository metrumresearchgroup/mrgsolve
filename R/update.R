# Copyright (C) 2013 - 2018  Metrum Research Group, LLC
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
                 "verbose","debug","preclean","mindt",
                 "digits", "ixpr", "mxhnil","start", "end", "add", "delta",
                 "maxsteps", "hmin", "hmax","tscale", "request"))

other_val <- c("param", "init", "omega", "sigma")

all_updatable <- c(sval,other_val)

##' Update the model object
##'
##' After the model object is created, update various attributes.
##'
##' @param object a model object
##' @param ... named items to update
##' @param merge logical indicating to merge (rather than replace) 
##' new and existing attributes
##' @param open logical; used only when merge is \code{TRUE} and 
##' parameter list or initial conditions
##' list is being updated; if \code{FALSE}, no new items will be 
##' added; if \code{TRUE}, the parameter list may expand.
##' @param data a list of items to update; this list is combined 
##' with any items passed in via \code{...}
##' 
##' @return The updated model object is returned.
##' 
##' @details
##' Slots that can be updated: 
##' 
##' \itemize{
##' \item verbose
##' \item debug
##' \item preclean
##' \item mindt
##' \item digits
##' \item atol - absolute solver tolerance; see \code{\link{solversettings}}
##' \item rtol - relative solver tolerance; see \code{\link{solversettings}}
##' \item ixpr - see \code{IXPR} in \code{\link{solversettings}}
##' \item mxhnil - see \code{MXHNIL} in \code{\link{solversettings}}
##' \item hmin - see \code{HMIN} in \code{\link{solversettings}}
##' \item hmax - see \code{HMAX} in \code{\link{solversettings}}
##' \item maxsteps - see \code{MXSTEP} in \code{\link{solversettings}}
##' \item start, end, delta, add
##' \item tscale
##' \item request
##' \item param
##' \item init
##' \item omega
##' \item sigma
##' } 
##'  
##' @name update
##' 
##' @aliases update,mrgmod-method
##' 
##' @examples
##' \dontrun{
##'  mod <- mrgsolve:::house()
##'
##'  mod <- update(mod, end=120, delta=4, param=list(CL=19.1))
##' }
##'  
##' @seealso \code{\link{update}}, \code{\link{mrgmod-class}}
##'  
##' @export
##'  
setMethod("update", "mrgmod", function(object, ..., merge=TRUE, open=FALSE, data=NULL) {
  
  args <- list(...)
  
  if(!is.null(data)) {
    args <- combine_list(args,data)
  }
  
  if(length(args)==0) return(object)
  
  args <- args[!is.na(args)]
  
  a <- names(args)
  
  valid.in <- which(charmatch(a, sval, nomatch=0) > 0)
  
  if(length(valid.in) > 0) {
    valid.full <- charmatch(a[valid.in], sval, nomatch=0)
    for(i in seq_along(valid.in)) {
      slot(object, sval[valid.full[i]]) <- args[[valid.in[i]]]
    }
  }
  
  ## Initial conditions list:
  if(is.element("init", a)) {
    i <- object@init@data
    i <- merge.list(i, args$init, context="init", open=open)
    slot(object, "init") <- as.init(i)
  }
  
  ## Parameter update:
  if(is.element("param", a)) {
    if(length(object@fixed)>0) {
      if(any(is.element(names(args$param),names(object@fixed)))) {
        warning("attempted update of a $FIXED parameter.", 
                call.=FALSE,immediate.=TRUE)
      }
    }
    object@param <- as.param(
      merge.list(
        object@param@data,args$param,open=open,context="param"
      )
    )
  }
  
  ## OMEGA
  if(is.element("omega", a)) {
    object@omega <- update_matlist(
      object@omega,omat(args$omega),open=open, context="omat"
    )
  }
  
  ## SIGMA
  if(is.element("sigma", a)) {
    object@sigma <- update_matlist(
      object@sigma,smat(args$sigma), open=open, context="smat"
    )
  }
  
  return(object)
})

same_sig <- function(x,y) {
  return(identical(unname(nrow(x)), unname(nrow(y))))
}

update_matlist <-  function(x,y,open=FALSE,context="update_matlist",...) {
  
  n0 <- dim_matlist(x)
  
  if(length(x)==0) {
    if(length(y)==0) {
      return(x)
    }
  }
  
  anon <- all(names(y)=="...")
  
  ss <- same_sig(x,y)
  
  if(anon & !ss) {
    stop(paste("improper signature:", context), call.=FALSE)
  }
  
  if(ss & anon) {
    ## If we match the sig and all input is unnamed
    labels <- names(x@data)
    x@data <- y@data
    names(x@data) <- labels
  } else {
    x@data <- merge.list(x@data, y@data,open=open,context=context,...)
  }
  
  n <- dim_matlist(x)
  
  if(open) {
    x@n <- n
  } else {
    if(!identical(n0,n)) {
      stop(paste("improper dimension:",context), call.=FALSE)
    }
  }
  
  validObject(x)
  
  return(x)
}

##' @rdname update
##' @export
##' @param y another object involved in update
setMethod("update", "omegalist", function(object,y,...) {
  update_matlist(object, omat(y),context="omat",...)
})

##' @rdname update
##' @export
setMethod("update", "sigmalist", function(object,y,...) {
  update_matlist(object, smat(y),context="smat",...)
})

##' @rdname update
##' @param .y data to update
##' @export
setMethod("update", "parameter_list", function(object,.y,...) {
  object <- as.param(
    merge.list(
      object@data, as.param(.y)@data, context="param"
    )
  )
  object
})

##' @export
##' @rdname update
setMethod("update", "ev", function(object,y,...) {
  
})


##' Update model or project in an model object
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





