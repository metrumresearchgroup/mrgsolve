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

valid.modlist <- function(object) {
  
  x1 <- all(sapply(object@data,is.mrgmod))
  
  if(all(x1)) return(TRUE)
  
  out <- c()
  
  if(!x1) out <- c(out,"All objects need to be mrgmod.")
  
  return(out)
}


##' S4 class matlist.
##'
##' @rdname modlist-class
setClass("modlist",
         slots=c(data="list",n="numeric"),
         validity=valid.modlist
)


##' @export
str.modlist <- function(object,...) {
 
  dplyr::data_frame(model=names(object@data)) 
  
}

##' @export
##' @rdname modlist-class
##' @param x modlist object
##' @param name model to take; used with \code{$}
##' 
setMethod("$", "modlist", function(x,name){x@data[[name]]})



##' Create a modlist object.
##' 
##' @param project file path to models
##' @param soloc directory where the models will be built
##' @param prefix leading tag for models to process
##' @param pattern a regular expression for models to get
##' @param index_file name of file to look for registered models
##' 
##' 
##' 
modlist <- function(project='.', soloc=tempdir(), prefix="",
                    pattern=paste0(prefix,"*\\.cpp$"),
                    index_file="MODLIST") {
  
  if(!file.exists(file.path(project,index_file))) {
    stop("Could not find index_file file.",call.=FALSE) 
  }
  
  models <- readLines(file.path(project,index_file))
  models <- models[models !=""]
  
  
  if(!all(file.exists(filename(project,models,".cpp")))) {
    stop("Could not locate all models listed in index_file.",call.=FALSE) 
  }
  
  message("Found ", length(models), " in ", index_file)
  
  mod <- lapply(models,mread,project=project,soloc=soloc)
  names(mod) <- models
  new("modlist", data=mod, n=length(mod))
}

##' Show a modlist object.
##' 
##' @export
##' @param object modlist object
##' 
setMethod("show", "modlist", function(object) {
  npar <- sapply(object@data,function(x) length(pars(x)))
  ncmt <- sapply(object@data,function(x) length(cmt(x)))
  dplyr::data_frame(model = names(object@data),
             npar = npar, ncmt=ncmt) %>% as.data.frame %>% print

})

