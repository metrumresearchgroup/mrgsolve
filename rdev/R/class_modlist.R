
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
##' 
##' 
##' 
modlist <- function(project='.', soloc=tempdir(), prefix="",
                    pattern=paste0(prefix,"*\\.cpp$")) {
  
  if(!file.exists(file.path(project,"MODLIST"))) {
    stop("Could not find MODLIST file.",call.=FALSE) 
  }
  
  models <- readLines(file.path(project,"MODLIST"))
  models <- models[models !=""]
  
  
  if(!all(file.exists(filename(project,models,".cpp")))) {
    stop("Could not locate all models listed in MODLIST.",call.=FALSE) 
  }
  
  message("Found ", length(models), " in this MODLIST.")
  
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

