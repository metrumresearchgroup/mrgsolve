##' Return information about model compilation.
##'
##' @param x model object
##' @param ... passed along
##' @export
setGeneric("shlib", function(x,...) standardGeneric("shlib"))
##' @export
##' @rdname shlib
setMethod("shlib", "mrgmod", function(x,...) {
  str(x@shlib,vec.len=10, no.list=TRUE, give.head=FALSE)
})

##' Get the names of model compartments.
##'
##' @param x model object
##' @param ... passed along
##' @examples
##' mod <- mrgsolve:::house()
##'
##' cmt(mod)
##'
##'
##' @export
setGeneric("cmt", function(x,...) standardGeneric("cmt"))
##' @export
##' @rdname cmt
setMethod("cmt", "mrgmod", function(x,...) return(names(x@init)))


##' Get the compartment number from a compartment name.
##'
##' @param x model object
##' @param ... passed along
##'
##' @examples
##' mod <- mrgsolve:::house()
##' mod %>% cmtn("CENT")
##' @export
setGeneric("cmtn", function(x,...) standardGeneric("cmtn"))
##' @export
##' @rdname cmtn
##' @param tag compartment name
setMethod("cmtn", "mrgmod", function(x,tag,...) return(which(cmt(x)==tag)))


##' Return the model name.
##'
##' @param x model object
##' @param ... passed along
##'
##' @examples
##' mod <- mrgsolve:::house()
##' dllname(mod)
##'
##' @seealso \code{\link{model}}
setGeneric("dllname", function(x,...) standardGeneric("dllname"))
##' @export
##' @rdname dllname
setMethod("dllname", "mrgmod", function(x,...) return(x@package))


##' Return the model name.
##'
##' @param x model object
##' @param ... passed along
##' @examples
##' mod <- mrgsolve:::house()
##' model(mod)
##' @export
setGeneric("model", function(x,...) standardGeneric("model"))
##' @export
##' @rdname model
setMethod("model", "mrgmod", function(x,...) return(x@model))


##' Return the location of the model shared object.
##'
##' @param x model object
##' @param short logical; if \code{TRUE}, \code{soloc} will be rendered  with a short path name
##' @export
##' @rdname soloc
##' @examples
##' mod <- mrgsolve:::house()
##' soloc(mod)
##'
soloc <- function(x,short=FALSE) {
  if(short) return(build_path(x@soloc))
  return(x@soloc)
}


##' Return the name of the model specification file.
##'
##' @param x model object
##' @param ... passed along
##' @export
setGeneric("cfile", function(x,...) standardGeneric("cfile"))
##' @export
##' @rdname cfile
setMethod("cfile", "mrgmod", function(x,...) return(pathfun(filename(project(x),model(x),".cpp",...))))
##' @export
##' @rdname cfile
setMethod("cfile", "lockedmod", function(x,...) return(pathfun(filename(x@src, x@model, ".cpp",...))))



##' Return the name of the shared object file.
##'
##' @param x model object
##' @param ... passed along
##' @export
##' @examples
##' mod <- mrgsolve:::house()
##' sodll(mod)
##'
setGeneric("sodll", function(x,...) standardGeneric("sodll"))
##' @export
##' @rdname sodll
setMethod("sodll", "mrgmod", function(x,...) {
  return(pathfun(file.path(soloc(x,...),dllfile(x))))
})
##' @export
##' @rdname sodll
setMethod("sodll", "lockedmod", function(x,...) {
  return(pathfun(filename(x@dllloc,x@dllname,.Platform$dynlib.ext,...)))
})
##' @export
##' @rdname sodll
setMethod("sodll", "packmod", function(x,...) {
  return(pathfun(getLoadedDLLs()[[x@package]][["path"]]))
})


##' Print model code to the console.
##'
##' @param x model object
##' @param raw return the raw code
##' @param ... passed along
##' @return invisible NULL
##' @export
setGeneric("see", function(x,...) standardGeneric("see"))
##' @export
##' @rdname see
setMethod("see", "mrgmod", function(x,raw=FALSE,...) {
  if(raw) return(x@code)
  what <- x@code
  if(length(what)==0) {
    if(file.exists(cfile(x))) what <- readLines(cfile(x),warn=FALSE)
  }
  if(length(what)==0) {
    warning("No code to show.")
  } else {
    cat("\nModel file: ", basename(cfile(x)), "\n")
    cat(paste0(" ", what), sep="\n")
  }
  return(invisible(NULL))
})

see_compfile <- function(x) {
  file <- file.path(soloc(x),compfile(model(x)))
  if(!file.exists(file)) {
    message("Could not find the compiled code for this model.")
  }
  cat(readLines(file),sep="\n")
}



##' Return the name of the project directory.
##'
##' @param x model object or mrgsims object
##' @param ... passed along
##' @examples
##' mod <- mrgsolve:::house()
##' project(mod)
##'
##' @export
setGeneric("project", function(x,...) standardGeneric("project"))
##' @export
##' @rdname project
setMethod("project", "mrgmod", function(x,...) return(x@project))
##' @export
##' @rdname project
setMethod("project", "packmod", function(x,...) {
  return(file.path(path.package(x@package),"project"))
})
##' @export
##' @rdname project
setMethod("project", "mrgsims", function(x,...) return(project(mod(x))))

##' Return the number of compartments / equations.
##'
##' @param x model object
##' @param ... passed along
##' @examples
##' mod <- mrgsolve:::house()
##' neq(mod)
##'
##' @export
setGeneric("neq", function(x,...) standardGeneric("neq"))
##' @export
##' @rdname neq
setMethod("neq", "mrgmod", function(x,...) return(length(cmt(x))))

##' Return the names of model parameters.
##'
##' @param x model object
##' @param ... passed along
##' @examples
##' mod <- mrgsolve:::house()
##' pars(mod)
##'
##' @export
setGeneric("pars", function(x,...) standardGeneric("pars"))
##' @export
##' @rdname pars
setMethod("pars", "mrgmod", function(x,...) names(as.list(param(x))))


##' Load the model shared object.
##'
##' @param x the model object
##' @param ... passed along
##' @export
setGeneric("loadso", function(x,...) standardGeneric("loadso"))
##' @export
##' @rdname loadso
setMethod("loadso", "mrgmod", function(x,...) {
  if(.Platform$OS.type!="unix") try(dyn.unload(sodll(x)),silent=TRUE)
  foo <- try(dyn.load(sodll(x)))
  if(class(foo)=="try-catch") {
    message(foo)
    return(invisible(FALSE))
  }
  return(invisible(x))
})
##' Unload the model shared object.
##'
##' @param x  model object
##' @param ... passed along
##' @export
setGeneric("unloadso", function(x,...) standardGeneric("unloadso"))
##' @export
##' @rdname unloadso
setMethod("unloadso", "mrgmod", function(x, ...) {
  out <- try(dyn.unload(sodll(x)), TRUE)
  if(inherits(out, "try-error")) {
    stop(out[1])
  } else {
    message("unloaded ", sodll(x))
  }
  return(invisible(NULL))
})


##' Get the times at which the model will be evaluated.
##'
##' @name stime
##'
##' @param x object of class mrgmod
##' @param ... passed on
##' @return a sorted vector of unique times
##' @details
##' Simulation times include the sequence of times created from \code{start}, \code{end}, and \code{delta} and the vector of times
##' found in \code{add}.  Making \code{end} negative will omit any \code{start} / \code{end} / \code{delta} sequence.  Negative values are discarded from the result.
##' @export
##' @examples
##'
##' ## example("stime", package="mrgsolve")
##'
##' mod <- mrgsolve:::house(end=12, delta=2, add=c(11,13,15))
##'
##' stime(mod)
##'
##' out <- mrgsim(mod, end=-1, add=c(2,4,5))
##'
##' stime(out)
##'
##' out$time
setGeneric("stime", function(x,...) standardGeneric("stime"))
##' @export
##' @rdname tgrid
setMethod("stime", "mrgmod",  function(x,...) {
  render_time(x)
})
##' @export
##' @rdname tgrid
setMethod("stime", "mrgsims", function(x,...) stime(mod(x)))


##' Get model random effect variances and covariancnes.
##'
##' @param x model object
##' @param ... passed along
##'
##' @export
##' @rdname revar
setGeneric("revar", function(x,...) standardGeneric("revar"))
##' @export
##' @rdname revar
setMethod("revar", "mrgmod", function(x,...) return(list(omega=x@omega,sigma=x@sigma)))
##' @export
##' @rdname revar
setMethod("revar", "mrgsims", function(x,...) {revar(mod(x))})

##' Simulate random effects from model.
##'
##' @param x model object
##' @param seed passed to \code{\link{set.seed}}
##' @param neta number of etas to simulate
##' @param neps number of epsilon values to simulate
##' @param ... passed along
##'
##' @export
##' @rdname simre
setGeneric("simre", function(x,...) standardGeneric("simre"))

##' @export
##' @rdname simre
setMethod("simre", "mrgmod", function(x,seed=NULL,neta=10,neps=10,...) {
  if(!is.null(seed)) set.seed(seed)
  return(simulateres(neta,omat(x,make=TRUE), neps,smat(x,make=TRUE)))
})
##' @export
##' @rdname simre
setMethod("simre", "mrgsims", function(x,seed=NULL,...) {
  NID <- length(unique(x@data[,"ID"]))
  N <- nrow(x@data)
  
  y <- mod(x)
  
  if(is.null(seed) & !is.na(x@seed)) set.seed(x@seed)
  if(!is.null(seed) & is.na(x@seed)) set.seed(seed)
  
  return(simulateres(NID,omat(y,make=TRUE), N, smat(y,make=TRUE)))
})


##' Return the code blocks from a model specification file.
##'
##' @param x model object or path to model specification file
##' @param ... passed along
##'
##' @examples
##' mod <- mrgsolve:::house()
##' mod %>% blocks
##' mod %>% blocks(PARAM,TABLE)
##'
##' @export
setGeneric("blocks", function(x,...) standardGeneric("blocks"))
##' @export
##' @rdname blocks
setMethod("blocks", "mrgmod", function(x,...) {
  what <- as.character(match.call()[-1])[-1]
  blocks_(cfile(x),what)
})
##' @export
##' @rdname blocks
setMethod("blocks", "character", function(x,...) {
  what <- as.character(match.call()[-1])[-1]
  blocks_(x,what)
})

blocks_ <- function(file,what) {
  if(length(what)==0) what <- c("PARAM","MAIN", "ODE","DES", "TABLE")
  if(!file.exists(file)) stop("Can't find model file", call.=FALSE)
  bl <- modelparse(readLines(file, warn=FALSE))
  if(!any(what == "all")) bl <- bl[names(bl) %in% what]
  if(length(bl)==0) {
    message("No blocks found.")
    return(invisible(NULL))
  }
  
  bl <- lapply(bl, paste, collapse="\n")
  x1 <- paste0("$", names(bl), "\n")
  cat("\nModel file:",basename(file), "\n\n")
  cat(paste0(x1,unlist(bl)), sep="\n\n")
}


as_pack_mod <- function(model, project, PACKAGE) {
  x <- mread(model, project,compile=FALSE,udll=FALSE,ns=FALSE)
  code <- readLines(cfile(x),warn=FALSE)
  x <- new("packmod",
           x,
           package=PACKAGE,
           model=model
  )
  soloc <- soloc(x)
  source <- file.path(soloc,compfile(model(x)))
  x@shlib$par <- pars(x)
  x@shlib$cmt <- cmt(x)
  x@shlib$source <- NULL
  x@code <- code
  x <- relocate_funs(x, PACKAGE)
  x@soloc <- ""
  
  return(list(mod=x, soloc=soloc,source=source))
}

##' Get all names from a model object.
##' 
##' @param x the model object
##' @export
##' @name update
##' @aliases names,mrgmod-method
##' @examples
##' mod <- mrgsolve:::house()
##' names(mod)
setMethod("names", "mrgmod", function(x) {
  ans <- list()
  ans$param <- pars(x)
  ans$cmt <- cmt(x)
  ans$omega <- list(names(omat(x)),unlist(labels(omat(x)),use.names=FALSE))
  ans$sigma <- list(names(smat(x)),unlist(labels(smat(x)),use.names=FALSE))
  return(ans)
})


##' Show model specification and C++ files.
##' 
##' @param x model object
##' @param spec logical; show the model specification file
##' @param source logical; show the C++ file that is actually compiled
##' @param ... not used
##' @export
##' 
file_show <- function(x,spec=TRUE,source=TRUE,...) {
  stopifnot(is.mrgmod(x))
  what <- list()
  if(spec) what$spec <- cfile(x)
  if(source) what$source <- x@shlib$source
  do.call(base::file.show,what)
}


##' Check if an object is \code{mrgmod} or \code{packmod}.
##' 
##' 
##' @param x any object
##' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
##' @export
##' 
is.mrgmod <- function(x) inherits(x,c("mrgmod","packmod"))

##' Check if an object is \code{mrgsims}.
##' 
##' 
##' @param x any object
##' @export
##' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
##' 
is.mrgsims <- function(x) inherits(x,"mrgsims")

is.matlist <- function(x) inherits(x,"matlist")
is.mrgindata <- function(x) inherits(x,"mrgindata")
is.valid_idata <- function(x) inherits(x,"valid_idata")

##' @export
setGeneric("parin", function(x) standardGeneric("parin"))
setMethod("parin", "mrgmod", function(x) {
  list(rtol=x@rtol,atol=x@atol, hmin=as.double(x@hmin), hmax=as.double(x@hmax),ixpr=x@ixpr,
       maxsteps=as.integer(x@maxsteps),mxhnil=x@mxhnil,verbose=as.integer(x@verbose),debug=x@debug,
       digits=x@digits, tscale=x@tscale,
       mindt=x@mindt, advan=x@advan)
})


