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

setAs("NULL", "character", function(from) character(0))

sval <- unique(c("atol","rtol",
                 "verbose","debug","preclean","mindt",
                 "digits", "ixpr", "mxhnil","start", "end", "add", "delta",
                 "maxsteps", "hmin", "hmax","tscale", "request"))

other_val <- c("param", "init", "omega", "sigma", "outvars")

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
##' @param strict if \code{TRUE}, then an error will be generated if there is 
##' attempt to update a non-existent item
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
##' \item outvars
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
setMethod("update", "mrgmod", function(object, ..., merge=TRUE, open=FALSE, 
                                       data=NULL, strict=TRUE) {
  
  args <- list(...)
  
  if(!is.null(data)) {
    args <- combine_list(args,data)
  }
  
  if(length(args)==0) return(object)
  
  args <- args[!is.na(args)]
  
  a <- names(args)
  
  m <- charmatch(a,all_updatable)
  
  if(strict && anyNA(m)) {
    if(getOption("mrgsolve.update.strict", FALSE)) {
      bad <- a[is.na(m)]
      for(b in bad) {
        mesg <- paste0("invalid item for model object update: ", b)
        warning(mesg, call.=FALSE, immediate.=TRUE) 
      }
    }
  }
  valid <- !is.na(m)
  a[valid] <- all_updatable[m[valid]]
  names(args) <- a
  valid.in <- which(a %in% sval)
  if(length(valid.in) > 0) {
    for(i in seq_along(valid.in)) {
      slot(object, a[valid.in[i]]) <- args[[a[valid.in[i]]]]
    }
  }
  
  if("request" %in% a) {
    object <- update_request(object,cvec_cs(args[["request"]]))  
  }
  
  if("outvars" %in% a) {
    object <- update_outputs(object,cvec_cs(args[["outvars"]]))  
  }
  
  ## Initial conditions list:
  if("init" %in%  a) {
    i <- object@init@data
    i <- merge.list(i, args$init, context="init", open=open)
    slot(object, "init") <- as.init(i)
  }
  
  ## Parameter update:
  if("param" %in% a) {
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
  if("omega" %in% a) {
    object@omega <- update_matlist(
      object@omega,omat(args$omega),open=open, context="omat"
    )
  }
  
  ## SIGMA
  if("sigma" %in% a) {
    object@sigma <- update_matlist(
      object@sigma,smat(args$sigma), open=open, context="smat"
    )
  }
  
  return(object)
})

default_outputs <- function(mod) {
  mod@cmti <- setNames(seq_along(Cmt(mod)),Cmt(mod))
  mod@capturei <- setNames(seq_along(mod@capture),mod@capture)
  mod
}

update_outputs <- function(mod, outputs = character(0)) {
  if(length(outputs)==0) return(mod)
  outputs <- unique(outputs)
  if(identical(outputs,"(all)")) {
    return(default_outputs(mod))
  }
  if(identical(outputs, "(reset)")) {
    mod <- update_request(mod,mod@request)
    if(identical(mod@request,"(all)")) {
      outputs <- c(Cmt(mod),mod@capture)  
    } else {
      outputs <- c(mod@request,mod@capture)      
    }
  }
  ren <- .ren.create(outputs)
  mod@cmti <- which(Cmt(mod) %in% ren$old)
  names(mod@cmti) <- .ren.rename(ren,Cmt(mod)[mod@cmti])
  mod@capturei <- which(mod@capture %in% ren$old)
  names(mod@capturei) <- .ren.rename(ren,mod@capture[mod@capturei])
  diff <- setdiff(ren$old,c(Cmt(mod),mod@capture))
  if(length(diff) > 0) {
    diff <- paste0(diff,collapse=',')
    wstop("requested output (", diff, ") is not a compartment or captured value")
  }
  mod
}

update_request <- function(mod, request = NULL) {
  if(is.null(request)) return(mod)
  if(identical(request,"")) {
    mod@cmti <- integer(0)
    return(mod)
  }
  if(identical(request,"(all)")) {
    mod@cmti <- seq_along(Cmt(mod))
    names(mod@cmti) <- Cmt(mod)
  } else {
    ren <- .ren.create(request)
    mod@cmti <- which(Cmt(mod) %in% ren$old)
    names(mod@cmti) <- .ren.rename(ren,Cmt(mod)[mod@cmti])
  }
  return(mod)
}


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



# Update model or project in an model object
#
# @param x mrgmod object
# @param model model name
# @param project project directory
# @param ... passed along
# @return updated model object
# @export
# @keywords internal
# setGeneric("relocate", function(x,...) standardGeneric("relocate"))
# 
# setMethod("relocate", "mrgmod", function(x,model=NULL, project=NULL) {
#   if(!missing(model)) x@model <- model
#   if(!missing(project)) x@project <- normalizePath(project,winslash=.Platform$file.sep)
#   validObject(x)
#   return(x)
# })





