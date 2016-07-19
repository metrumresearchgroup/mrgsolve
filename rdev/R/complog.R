## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

SAFE_WAIT_TIME <- 2

check_and_copy <- function(from,to,preclean=FALSE) {
  
  if(!file.exists(to)) {
    file.copy(from,to)
    same <- TRUE
  } else {
    same <- tools::md5sum(from) == tools::md5sum(to)
    if((!same) | preclean) {
      file.copy(from,to,overwrite=TRUE)
    }
  }
  z <- file.remove(from)
  return(same)
}

## Wait a certain amount of time before re-compiling
## and loading a model
safe_wait <- function(x) {
  
  target <- compout(model(x),soloc(x))
  if(!file.exists(target)) return(invisible(NULL))
  mt <- file.info(target)[["mtime"]]
  age <- as.numeric(as.POSIXct(Sys.time())) - as.numeric(as.POSIXct(mt))
  if(age > SAFE_WAIT_TIME) return(invisible(NULL))
  message("(waiting) ...")
  return(Sys.sleep(SAFE_WAIT_TIME-age))
}


##' Clean up model shared objects. 
##' 
##' @param x model object
##' 
##' @details
##' \code{cleanso} removes (deletes) shared objects from the model compile directory and 
##' attempts to unload shared objects which appear to be loaded.
##' 
##'
##' 
cleanso <- function(x) {
  soloc <- soloc(x)
  so <- list.files(soloc, pattern=paste0("*\\", .Platform$dynlib.ext), full.names=TRUE)
  so <- so[so != compout(model(x),soloc)]
  lo <- sapply(getLoadedDLLs(), "[[", "path")
  y <- intersect(lo,so)
  for(w in y) foo <- try(dyn.unload(y),silent=TRUE)
  file.remove(so)
  return(invisible(NULL))
}

##' Deprecated; use cleanso instead.
##' 
##' @export
##' @param x model object
##' 
comp_forget <- function(x) {
  message("comp_forget is deprecated.  Use mrgsolve:::cleanso(mod) for equivalent.")
  return(invisible(NULL))
}





