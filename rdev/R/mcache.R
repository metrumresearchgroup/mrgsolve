

##' @rdname mread 
##' @export
mread_cache <- function(model,project=getwd(),
                        code=NULL, soloc=tempdir(),quiet=FALSE,...) {
  
  build <- new_build(model,project,soloc,code) 

  cache_file <- file.path(build$soloc,"mrgmod_cache.RDS")
  
  ## If the cache file doesn't exist, build and return
  te <- file.exists(cache_file)
  t0 <- t1 <- t2 <- t3 <- FALSE
  
  if(te) {
    x <- readRDS(cache_file)
    t0 <- is.mrgmod(x)
    t1 <- is.character(x@shlib$md5)
    t2 <- x@shlib$md5 == build$md5
    t3 <- file.exists(sodll(x))
  }
  
  if(all(t0,t1,t2,t3,te)) {
    if(!quiet) message("Loading model from cache.")
    loadso(x)
    return(x)
  }
  
  x <- mread( model,project,soloc=soloc,quiet=quiet,...)
  
  saveRDS(x,file=cache_file)

  return(x) 
}


##' @rdname mcode
##' @export
mcode_cache <- function(model,code,project=tempdir(),...) {
  mread_cache(model,project,code,...)
}

