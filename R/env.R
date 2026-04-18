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
#' The `$ENV` block is a block of R code that can realize any sort of R 
#' object that might be used in running a model.
#' 
#' @seealso [env_get()], [env_get_env()], [env_ls()]
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
#' Call `env_get()` passing either a model object or simulated output and 
#' name an object to retrieve from the model object environment. 
#' `env_get_obj()` is an alias to `env_get()`. Call  `env_get_env()` to 
#' return the environment itself. Methods for `mrgmod` and `mrgsims` both 
#' interact with the same environment (see **Examples**).  
#' 
#' @param x a model object (class `mrgmod`) or simulated output (class 
#' `mrgsims`).
#' @param what the name of an object to return. 
#' @param ... passed [base::get()].
#' 
#' @examples 
#' mod <- house(end = 1)
#' 
#' # Just for the example
#' assign("let", letters[1:3], env_get_env(mod))
#' 
#' out <- mrgsim(mod)
#' 
#' env_get(out, "let")
#' 
#' env_get(mod, "let")
#' 
#' env_get_obj(out, "let")
#' 
#' env_get_env(mod)
#' 
#' # It's the same environment in out that is in mod
#' env_get_env(out)
#' 
#' @md
#' @export
env_get <- function(x, ...) UseMethod("env_get")
#' @rdname env_get
#' @export
env_get.mrgmod <- function(x, what, ...) {
  valid_what <- is.character(what) && length(what)==1
  if(!valid_what) {
    abort("`what` must be character with length 1.")
  }
  get(what, x@envir, ...)
}
#' @rdname env_get
#' @export
env_get.mrgsims <- function(x, ...) {
  env_get(x@mod, ...)
}
#' @rdname env_get
#' @export
env_get_obj <- function(x, ...) env_get(x, ...)
#' @rdname env_get
#' @export
env_get_env <- function(x, ...) UseMethod("env_get_env")
#' @rdname env_get
#' @export
env_get_env.mrgmod <- function(x, ...) {
  x@envir 
}
#' @rdname env_get
#' @export
env_get_env.mrgsims <- function(x, ...) {
  env_get_env(x@mod)
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
