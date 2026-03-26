# Copyright (C) 2013 - 2026  Metrum Research Group
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


#' Re-evaluate the code in the ENV block
#' 
#' The `$ENV` block is a block of R code that can realize any sort of R object 
#' that might be used in running a model.
#' 
#' @seealso [env_get()], [env_ls()]
#' 
#' @param x a model object.
#' @param seed passed to [set.seed()] if a numeric value is supplied.
#' 
#' @md
#' @export
env_eval <- function(x,seed=NULL) {
  if(is.numeric(seed)) set.seed(seed)
  eval_ENV_block(x=x@envir$.code,where=project(x),envir=x@envir)
  return(invisible(x))
}

#' List objects in the model environment
#' 
#' Each model keeps an internal environment that allows the user 
#' to carry any `R` object along.  Objects are coded in `$ENV`.
#' 
#' @param x a model object.
#' @param ... passed to [ls()].
#' 
#' @md
#' @export
env_ls <- function(x,...) {
  objects <- ls(x@envir,...)
  cl <- sapply(objects,function(o) {
    paste(class(get(o,x@envir)),collapse=',')
  })
  ans <- data.frame(object=objects,class=cl)
  rownames(ans) <- NULL
  arrange__(ans, .dots=c("class"))
}

#' Return model environment or objects from the model environment
#' 
#' @param x a model object.
#' @param what an object to return. 
#' @param tolist should the environment be coerced to `list`; ignored if `what`
#' is provided.
#' @param ... passed to other methods.
#' 
#' @examples 
#' mod <- house()
#' 
#' assign("let", letters[1:3], env_get_env(mod))
#' 
#' out <- mrgsim(mod)
#' 
#' env_get(out, "let")
#' 
#' env_get(mod, "let")
#' 
#' @md
#' @export
env_get <- function(x, ...) UseMethod("env_get")
#' @rdname env_get
#' @export
env_get.mrgmod <- function(x, what = NULL, tolist = TRUE, ...) {
  if(is.character(what)) {
    return(get(what, envir = x@envir))  
  }
  env <- env_get_env(x)
  if(tolist) {
    return(as.list(env))  
  } 
  env
}
#' @rdname env_get
#' @export
env_get.mrgsims <- function(x, ...) {
  env_get(x@mod, ...)
}

#' @rdname env_get
#' @export
env_get_env <- function(x) {
  x@envir 
}

#' Update objects in model environment
#' 
#' @param .x a model object.
#' @param .dots list of objects to updated.
#' @param ... objects to update.
#' 
#' @md
#' @export
env_update <- function(.x,...,.dots=list()) {
  right <- c(list(...),.dots)
  left <- as.list(.x@envir)
  .x@envir <- as.environment(merge.list(left,right))
  return(invisible(.x))
}



#' Run the model cama function
#' 
#' @param mod a model object.
#' @param fn function name.
#' @param ... passed to update.
#' 
#' @details `sah-mah`
#' 
#' @keywords internal
#' @md
#' @export
cama <- function(mod,fn="cama",...) {
  stop("this function is deprecated.")
  # object_exists(fn, mod@envir, "function", inherits=FALSE)
  # f <- get(fn, mod@envir, inherits=FALSE)
  # mod %>% update(...) %>% f
}


# param_in_env <- function(x) {
#   p <- as.list(param(x))
#   list2env(p[setdiff(names(p),ls(x@envir))],envir=x@envir)
# }
# 
# param_out_env <- function(x) {
#   what <- intersect(names(param(x)),ls(envir=x@envir))
#   rm(list=what,envir=x@envir)
#   return(invisible(x))
# }
# 


