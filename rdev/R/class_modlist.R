
valid.modlist <- function(object) {
  
  x1 <- all(sapply(object@data,is.mrgmod))
  
  if(all(x1)) return(TRUE)
  
  out <- c()
  
  if(!x1) out <- c(out,"All objects need to be mrgmod.")
  
  return(out)
}

##' @export
str.modlist <- function(object,...) {
 
  dplyr::data_frame(model=names(object@data)) 
  
}

##' @export
##' @rdname modlist
##' @param name model to take; used with \code{$}
setMethod("$", "modlist", function(x,name){x@data[[name]]})


##' S4 class matlist.
##'
##' @rdname matlist-class
setClass("modlist",
         slots=c(data="list",n="numeric"),
         validity=valid.modlist
)

##' Create a modlist object.
##' 
##' 
##' @export
##' 
modlist <- function(project='.', soloc=tempdir(), prefix="",
                    pattern=paste0(prefix,"*\\.cpp$")) {
  files <- list.files(project, pattern=pattern)
  files <- sub("\\.*cpp$", "", files)
  mod <- lapply(files,mread,project=project,soloc=soloc)
  names(mod) <- files
  new("modlist", data=mod, n=length(mod))
}

