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

setAs("NULL", "character", function(from) character(0))

sval <- unique(c("atol","rtol","ss_rtol", "ss_atol",
                 "verbose","debug","preclean","mindt",
                 "digits", "ixpr", "mxhnil","start", "end", "add", "delta",
                 "maxsteps", "hmin", "hmax","tscale", "request"))

other_val <- c("param", "init", "omega", "sigma", "outvars")

all_updatable <- c(sval,other_val)

#' Update the model object
#'
#' After the model object is created, update various attributes.
#'
#' @param object a model object.
#' @param ... named items to update.
#' @param merge logical indicating to merge (rather than replace) 
#' new and existing attributes.
#' @param open logical; used only when merge is \code{TRUE} and 
#' parameter list or initial conditions
#' list is being updated; if \code{FALSE}, no new items will be 
#' added; if \code{TRUE}, the parameter list may expand.
#' @param data a list of items to update; this list is combined 
#' with any items passed in via \code{...}.
#' @param strict if \code{TRUE}, a warning will be issued when there is an
#' attempt to update a non-existent item.
#' 
#' @return The updated model object is returned.
#' 
#' @details
#' Slots that can be updated: 
#' 
#' \itemize{
#' \item verbose
#' \item debug
#' \item preclean
#' \item mindt
#' \item digits
#' \item atol - absolute solver tolerance; see \code{\link{solversettings}}
#' \item rtol - relative solver tolerance; see \code{\link{solversettings}}
#' \item ss_rtol - relative tolerance when finding steady state
#' \item ss_atol - absolute tolerance when finding steady state
#' \item ixpr - see \code{IXPR} in \code{\link{solversettings}}
#' \item mxhnil - see \code{MXHNIL} in \code{\link{solversettings}}
#' \item hmin - see \code{HMIN} in \code{\link{solversettings}}
#' \item hmax - see \code{HMAX} in \code{\link{solversettings}}
#' \item maxsteps - see \code{MXSTEP} in \code{\link{solversettings}}
#' \item start, end, delta, add
#' \item tscale
#' \item request
#' \item param
#' \item init
#' \item omega
#' \item sigma
#' \item outvars
#' } 
#'  
#' @name update
#' 
#' @aliases update,mrgmod-method
#' 
#' @examples
#' \dontrun{
#'  mod <- house()
##'
#'  mod <- update(mod, end = 120, delta = 4, param = list(CL = 19.1))
#' }
#'  
#' @seealso \code{\link{update}}, \code{\link{mrgmod-class}}, 
#' \code{\link{within}}
#'  
#' @export
setMethod("update", "mrgmod", function(object, ..., merge=TRUE, open=FALSE, 
                                       data=NULL, strict=TRUE) {
  # TODO: get rid of the strict argument to `update()`
  # TODO: get rid of the merge argument to `update()`
  # TODO: get rid of the open argument to `update()`
  # TODO: longer term ... error when bad arguments are passed

  args <- list(...)
  
  if(!is.null(data)) {
    args <- combine_list(args,data)
  }
  
  if(length(args)==0) return(object)
  
  args <- args[!is.na(args)]
  
  a <- names(args)
  
  m <- charmatch(a, all_updatable)
  
  if(anyNA(m) && isTRUE(strict)) {
    if(!is.null(getOption("mrgsolve.update.strict"))) {
      msg <- c("The `mrgsolve.update.strict` option has been deprecated;", 
               "please use the `strict` argument to `mrgsolve::update()`",
               "instead.")
      warn(paste0(msg, collapse = " "))  
    }
    bad <- a[is.na(m)]
    names(bad) <- rep("x", length(bad))
    if(length(bad) > 1) {
      msg <- c("The following arguments were passed to `mrgsolve::update()`,",
               "they are either invalid names (check your spelling?) or not", 
               "eligible attributes for update:")
    } else {
      msg <- c("The following argument was passed to `mrgsolve::update()`, but",
               "it is either an invalid name (check your spelling?) or not an",
               "eligible attribute for update:")
    }
    msg <- c(msg, bad)
    warn(msg, call = NULL)
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
  mod@cmtL <- Cmt(mod)
  mod@Icmt <- seq_along(mod@cmtL)
  mod@capL <- unname(mod@capture)
  mod@Icap <- seq_along(mod@capL)
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
  ren <- .ren.create(unname(outputs),unique=FALSE)
  if(any(duplicated(ren$new))) {
    dup <- ren$new[duplicated(ren$new)]
    dup <- paste0(dup,collapse=",")
    wstop("duplicate names in outvars: ",dup)
  }
  mod@Icmt <- which(Cmt(mod) %in% ren$old)
  mod@cmtL <- unname(.ren.rename(ren,Cmt(mod)[mod@Icmt]))
  mod@Icap <- which(mod@capture %in% ren$old)
  mod@capL <- unname(.ren.rename(ren,mod@capture[mod@Icap]))
  diff <- setdiff(ren$old,c(Cmt(mod),mod@capture))
  if(length(diff) > 0) {
    for(d in diff) {
      message(" ", d, " is not a compartment or captured item")  
    }
    wstop("invalid item in requested output")
  }
  mod
}

update_request <- function(mod, request = NULL) {
  if(is.null(request)) return(mod)
  if(identical(request,"")) {
    mod@Icmt <- integer(0)
    mod@cmtL <- character(0)
    return(mod)
  }
  if(identical(request,"(all)")) {
    mod@Icmt <- seq_along(Cmt(mod))
    mod@cmtL <- Cmt(mod)
  } else {
    ren <- .ren.create(unname(request),unique=FALSE)
    if(any(duplicated(ren$new))) {
      dup <- ren$new[duplicated(ren$new)]
      for(d in dup) {
        message("duplicate name: ", d)  
      }
      wstop("duplicate names found in request")
    }
    mod@Icmt <- which(Cmt(mod) %in% ren$old)
    mod@cmtL <- .ren.rename(ren,Cmt(mod)[mod@Icmt])
  }
  return(mod)
}

update_capture <- function(x, capture) {
  combined <- c(x@capture, capture)
  combined <- combined[!duplicated(names(combined))]
  x@capture <- combined
  x <- default_outputs(x)
  x <- update_request(x, x@request)
  x
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

#' @rdname update
#' @export
#' @param y another object involved in update
setMethod("update", "omegalist", function(object,y,...) {
  update_matlist(object, omat(y), context = "omat",...)
})

#' @rdname update
#' @export
setMethod("update", "sigmalist", function(object,y, ...) {
  update_matlist(object, smat(y), context = "smat", ...)
})

#' @rdname update
#' @param .y data to update
#' @export
setMethod("update", "parameter_list", function(object, .y, ...) {
  
  # parameter update: must have names
  if(!is_named(.y)) {
    wstop("[param-update] all parameters must have names.")  
  }
  
  # parameter update: must be numeric
  non_nu <- !vapply(.y, FUN = single.number, TRUE)
  if(any(non_nu)) {
    w <- names(.y)[non_nu]
    w <- w[w %in% names(object@data)]
    if(length(w) > 0) {
      wstop("[param-update] parameters must be single numeric values.")  
    }
  }
  
  object@data <- update_list(
    object@data, 
    .y, 
    context = "param-update"
  )
  object
})

#' Update parameters, initials, and settings within a model object
#' 
#' The main use case for using [within] rather than [update] or [param] or
#' [init] is when you want to update to a new value that is calculated from 
#' the existing value.  See the example in details
#' 
#' Other model object slots that can be updated: `start`, `end`, `delta`, 
#' `add`, `rtol`, `atol`, `hmax`, `maxsteps`.  These are include for convenience, 
#' but we expect that most of the time these will get updated through the 
#' update method.
#' 
#' @param data an object with class mrgmod
#' @param expr expressions evaluated in an environment containing various model
#' object components, including parameters, initial conditions, and others 
#' (see details)
#' @param ... not used
#' 
#' @examples
#' mod <- mrgsolve::house()
#' 
#' mod2 <- within(mod, {CL <- CL * 1.5})
#' 
#' mod$CL
#' mod2$CL
#' 
#' @seealso [update]
#' @name within
#' @aliases within,mrgmod-method
#' @md
#' @export
within.mrgmod <- function(data,expr,...) {
  p <- as.list(param(data))
  i <- as.list(init(data))
  up <- list(
    start = data@start, end = data@end, delta = data@delta, 
    atol = data@atol, rtol = data@rtol, hmax = data@hmax,
    maxsteps = data@maxsteps
  )
  up <- up[which(!(names(up) %in% c(names(p),names(i))))]
  parent <- parent.frame()
  env <- list2env(c(p,i,up),parent = parent)
  eval(substitute(expr),env)
  if(length(up) > 0) {
    data <- update(data,data = mget(names(up),env))  
  }
  if(length(p) > 0) {
    data <- param(data,mget(names(p),env))  
  }
  if(length(i) > 0) {
    data <- init(data,mget(names(i),env))
  }
  data
}

set_ss_cmt <- function(x,ss_cmt) {
  if(!is.character(ss_cmt)) return(x)
  ss_cmt <- cvec_cs(ss_cmt)
  if(length(ss_cmt)==0) return(x)
  ss_exclude <- grepl("^-", ss_cmt[1])
  ss_cmt[1] <- gsub("^-", "", ss_cmt[1])
  ss_cmt <- ss_cmt[nzchar(ss_cmt)]
  if(length(ss_cmt)==0) return(x)
  if(any(!is.element(ss_cmt, Cmt(x)))) {
    diff <- setdiff(ss_cmt, Cmt(x)) 
    diff <- paste0(" - ", diff, "\n")
    stop("invalid cmt names found in ss_cmt\n", diff, call.=FALSE)
  }
  if(ss_exclude) ss_cmt <- setdiff(Cmt(x),ss_cmt)  
  x@ss_cmt <- match(ss_cmt, Cmt(x)) - 1L
  ss_cmt_check <- range(x@ss_cmt) 
  stopifnot(ss_cmt_check[1] >= 0 && ss_cmt_check[2] < neq(x))
  x
}
