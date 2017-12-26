

opt_env <- function(values = list()) {
  valid <- names(values)
  get <- function(name = NULL) {
    if(is.null(name)) return(values)
    if(length(name==1)) return(values[[name]])
    return(values[name])
  }
  set <- function(...) {
    args <- list(...)
    values <<- merge.list(values,args)
    return(invisible(NULL))
  }
  # config <- function(file, which = "default") {
  #   if(!requireNamespace("config")) {
  #     stop("please install config package") 
  #   }
  #   file <- normalizePath(file)
  #   conf <- config::get(file, config = which)
  #   values <<- merge.list(values,conf)
  # }
  environment()
}


##' Options used by mrgsolve
##' 
##' @details
##' 
##' \itemize{
##'  \item \code{end} default simulation end time
##'  \item \code{delta} default simulation step value
##'  \item \code{model_ext} default model file extension; should 
##'  include period if applicable (e.g. \code{.cpp} or \code{.txt})
##' }
##' 
##'
mrgsolve_opts <- opt_env(
  list(end = 24, 
       delta = 1, 
       model_ext = ".cpp"
  )
)



