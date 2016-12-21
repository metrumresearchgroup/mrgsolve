## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

SAFE_WAIT_TIME <- 2

update_wait_time <- function(n) {
  assignInMyNamespace("SAFE_WAIT_TIME",n)
}


check_and_copy <- function(from,to,preclean=FALSE) {
  
  if(!file_exists(to)) {
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

# Wait a certain amount of time before re-compiling
# and loading a model
safe_wait <- function(x) {
  
  target <- file.path(soloc(x),compout(model(x)))
  if(!file_exists(target)) return(invisible(NULL))
  mt <- file.info(target)[["mtime"]]
  age <- as.numeric(as.POSIXct(Sys.time())) - as.numeric(as.POSIXct(mt))
  if(age > SAFE_WAIT_TIME) return(invisible(NULL))
  message("(waiting) ...")
  return(Sys.sleep(SAFE_WAIT_TIME-age))
}


# Clean up model shared objects. 
# 
# @param x model object
# @param where directory to clean up
# 
# @details
# \code{cleanso} removes (deletes) shared objects from the model compile directory and 
# attempts to unload shared objects which appear to be loaded.
# 
#
# 
cleanso <- function(x,where=soloc(x)) {
  so <- list.files(where, pattern=paste0("*\\", .Platform$dynlib.ext), full.names=TRUE)
  so <- so[so != file.path(where,compout(model(x)))]
  lo <- sapply(getLoadedDLLs(), "[[", "path")
  y <- intersect(lo,so)
  for(w in y) foo <- try(silent=TRUE,dyn.unload(w))
  for(w in y) foo <- try(silent=TRUE,file.remove(w))
  return(invisible(NULL))
}




