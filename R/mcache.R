# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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


##' @rdname mread 
##' @export
mread_cache <- function(model,project=getwd(),
                        code=NULL, soloc=tempdir(), quiet=FALSE, preclean=FALSE,...) {
  
  build <- new_build(model,project,soloc,code,preclean) 
  
  cache_file <- file.path(build$soloc,"mrgmod_cache.RDS")
  
  ## If the cache file doesn't exist, build and return
  te <- file.exists(cache_file)
  t0 <- t1 <- t2 <- t3 <- FALSE
  
  if(te) {
    x <- readRDS(cache_file)
    t0 <- is.mrgmod(x)
    if(t1 <- is.character(x@shlib$md5)) {
      t2 <- x@shlib$md5 == build$md5
    }
    t3 <- file.exists(sodll(x))
  }
  
  if(all(t0,t1,t2,t3,te)) {
    if(!quiet) message("Loading model from cache.")
    loadso(x)
    return(x)
  }
  
  x <- mread(model,project,soloc=soloc,quiet=quiet,...)
  
  saveRDS(x,file=cache_file)
  
  return(x) 
}


##' @rdname mcode
##' @export
mcode_cache <- function(model,code,project=tempdir(),...) {
  mread_cache(model,project,code,...)
}

