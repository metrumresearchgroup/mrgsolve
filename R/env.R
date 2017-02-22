##' Re-evaluate the code in the ENV block.
##' 
##' The \code{$ENV} block is a block of R code that can 
##' realize any sort of R object that might be used in running 
##' a model.
##' 
##' @seealso \code{\link{env_get}}, \code{\link{env_ls}}
##' 
##' @param x model object
##' @param seed passed to \code{\link{set.seed}} if a numeric value is supplied
##' 
##' @export
env_eval <- function(x,seed=NULL) {
  if(is.numeric(seed)) set.seed(seed)
  eval_ENV_block(x=x@envir$.code,where=project(x),envir=x@envir)
  return(invisible(x))
}

##' List objects in the model environment.
##' 
##' Each model keeps an internal environment that allows the user 
##' to carry any \code{R} object along.  Objects are coded in \code{$ENV}.
##' 
##' @param x model object
##' @param ... passed to \code{\link{ls}}
##' 
##' @export
env_ls <- function(x,...) {
  objects <- ls(x@envir,...)
  cl <- sapply(objects,function(o) {
    paste(class(get(o,x@envir)),collapse=',')
  })
  ans <- data.frame(object=objects,class=cl)
  rownames(ans) <- NULL
  dplyr::arrange(ans, class)
}

##' Return model environment.
##' 
##' @param x model object
##' @param tolist should the environment be coreced to \code{list}?
##' 
##' @export
env_get <- function(x,tolist=TRUE) {
  if(tolist) {
    return(as.list(x@envir))  
  } else {
    return(x@envir) 
  }
}

##' Update objects in model environment.
##' 
##' @param .x model object
##' @param .dots list of objects to updated
##' @param ... objects to update
##' 
##' @export
env_update <- function(.x,...,.dots=list()) {
  right <- c(list(...),.dots)
  left <- as.list(.x@envir)
  .x@envir <- as.environment(merge(left,right))
  return(invisible(.x))
}

##' Run the model cama function.
##' 
##' @param mod model object
##' @param fn function name
##' @param ... passed to update
##' 
##' @details \code{sah-mah}
##' 
##' @export
cama <- function(mod,fn="cama",...) {
  object_exists(fn, mod@envir, "function", inherits=FALSE)
  f <- get(fn, mod@envir, inherits=FALSE)
  mod %>% update(...) %>% f
}

param_in_env <- function(x) {
  p <- as.list(param(x))
  list2env(p[setdiff(names(p),ls(x@envir))],envir=x@envir)
}

param_out_env <- function(x) {
  what <- intersect(names(param(x)),ls(envir=x@envir))
  rm(list=what,envir=x@envir)
  return(invisible(x))
}



