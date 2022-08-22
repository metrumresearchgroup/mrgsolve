# Copyright (C) 2013 - 2022  Metrum Research Group
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




#' Create and work with parameter objects
#'
#' See [numericlist] for methods to deal with `parameter_list` objects.
#'
#'
#' @param .x the model object.
#' @param .y an object to be merged into parameter list; non-`NULL` values 
#' must be named list, data.frame, or numeric vector; named items that do 
#' not exist in the parameter list are allowed and will be silently ignored; 
#' use the `.strict` argument to require that all names in `.y` exist already
#' in the parameter list.
#' @param .pat a regular expression (character) to be applied as a filter 
#' for which parameters to show when printing.
#' @param .strict if `TRUE`, all names to be updated must be found 
#' in the parameter list.
#' @param ... passed along or name/value pairs to update the parameters 
#' in a model object; when passing new values this way, all values 
#' must be numeric and all all names must exist in the parameter list
#' for `.x`.
#' 
#' @details 
#' Can be used to either get a parameter list object from a `mrgmod` 
#' model object or to update the parameters in a model object.  
#' For both uses, the return value is a `parameter_list` object. For the 
#' former use, `param` is usually called to print the parameters to the 
#' screen, but the `parameter_list` object can also be coerced to a list 
#' or numeric R object.
#' 
#' Use `allparam()` to get a `parameter_list` object including 
#' both model parameters and data items listed in `$FIXED`.
#' 
#' The update to parameters can be permissive (candidates with names that don't
#' exist in the parameter list are silently ignored) or strict (all candidates
#' must already exist in the parameter list). When passing candidate values
#' via `...`, the update is strict and an error is generated if you pass a 
#' name that isn't found in the parameter list. When candidate values 
#' are passed as a named object via `.y`, then the update is permissive. Any 
#' permissive update can be made strict (error if foreign names are found in 
#' the candidates) by passing `.strict = TRUE`. 
#' 
#' An alternative is to assess the incoming names using [inventory()]. 
#' 
#' @return 
#' An object of class `parameter_list` (see [numericlist]).
#' 
#' @seealso 
#' [inventory()]
#' 
#' @examples
#' ## example("param")
#' 
#' mod <- house()
#'
#' param(mod)
#' 
#' param(mod, .pat="^(C|F)") ## may be useful when large number of parameters
#'
#' class(param(mod))
#'
#' param(mod)$KA
#' 
#' param(mod)[["KA"]]
#'
#' as.list(param(mod))
#' 
#' as.data.frame(param(mod))
#' 
#' param(mod, CL = 1.2)
#' 
#' new_values <- list(CL = 1.3, VC = 20.5)
#' 
#' param(mod, new_values)
#' 
#' @md
#' @keywords param
setGeneric("param", function(.x, ...) {
  standardGeneric("param")
})

#' @export
#' @rdname param
setMethod("param", "mrgmod", function(.x, .y = NULL, ..., .pat="*", .strict=FALSE) {
  
  .dots <- list(...)
  
  has_dots <- length(.dots) > 0
  
  if(is.null(.y) && !has_dots) {
    slot(.x@param, "pattern")  <- .pat
    return(.x@param)
  }
  
  if(missing(.strict) && has_dots) {
    .strict <- TRUE 
  }
  
  if(!inherits(.y, c("NULL", "list", "data.frame", "numeric"))) {
    wstop("[param-update] invalid object to update parameter list.")
  }
  
  if(is.data.frame(.y) && nrow(.y) != 1) {
    stop(
      "[param-update] data.frame input should have exactly one row.", 
      call.=FALSE
    )
  } else {
    .y <- as.list(.y)  
  }
  
  if(has_dots) {
    .y <- c(.y, .dots)  
  }
  
  if(.strict) {
    if(!all(names(.y) %in% Pars(.x))) {
      foreign <- setdiff(names(.y), Pars(.x))
      foreign <- paste("--| ", foreign, collapse="\n")
      stop(
        "[param-update] not a model parameter\n", "  ", 
        foreign, call.=FALSE
      ) 
    } 
  }
  
  .x@param <- update(.x@param, .y)
  
  return(.x)
})

#' @rdname param
#' @export
setMethod("param", "mrgsims", function(.x,...) {
  param(mod(.x),...)
})

#' @rdname param
#' @export
setMethod("param", "missing", function(..., .strict = TRUE) {
  param(list(...), .strict = .strict)
})

#' @rdname param
#' @export
setMethod("param", "list", function(.x, ...) {
  create_numeric_list(.x, "parameter_list", ...)
})

#' @rdname param
#' @export
setMethod("param", "ANY", function(.x,...) {
  param(as.list(.x), ...)
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
  n <- seq(NPARAM)
  
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

#' @export
#' @rdname param
allparam <- function(.x) {
  as.param(c(as.list(param(.x)), .x@fixed))
}

#' Show the parameter list
#' @param object the object to show
#' @export
#' @keywords internal
setMethod("show", "parameter_list", function(object) {
  showparam(object)
})

setGeneric("as.param", function(.x,...) {
  standardGeneric("as.param")
})

setMethod("as.param", "list", function(.x,...) {
  create_numeric_list(.x, "parameter_list", ...)
})

setMethod("as.param", "numeric", function(.x,...) {
  create_numeric_list(as.list(.x), "parameter_list",...)
})

setMethod("as.param", "parameter_list", function(.x,...) .x)

setMethod("as.param", "missing", function(.x,...) {
  create_numeric_list(list(), "parameter_list",...)
})

as.fixed <- function(x) {
  as.list(x)
}
