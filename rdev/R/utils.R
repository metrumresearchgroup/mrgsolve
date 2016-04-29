
## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

loaded_dll_files <- function() {
    sapply(getLoadedDLLs(), function(x) x[["path"]])
}


##' Get path to example models
##' @export
installed_models <- function() {
    file.path(system.file(package="mrgsolve"), "inst", "models")
}

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


dllfile <- function(x) paste0(dllname(x),.Platform$dynlib.ext)
pathfun <- function(...) path.expand(...) #,mustWork=FALSE,winslash=.Platform$file.sep
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
##' @param ... passed along
##' @return invisible NULL
##' @export
setGeneric("see", function(x,...) standardGeneric("see"))
##' @export
##' @rdname see
setMethod("see", "mrgmod", function(x,...) {
    if(!file.exists(cfile(x))) {
        warning("can't find source code file")
    } else {
        cat("\nModel file: ", basename(cfile(x)), "\n")
        cat(readLines(cfile(x),warn=FALSE), sep="\n")
    }
    return(invisible(NULL))
})

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


is.mt <- function(x) {return(is.null(x) | length(x)==0)}
is.string <- function(x) {return(is.character(x) & length(x)==1)}
has.names <- function(x) {return(!is.mt(names(x)))}


## NEEDED: keep
ininit.mrgmod <- function(x,thislist) {
    names(thislist) %in% names(init(x))
}

##' Merge two lists
##'
##' @param x the original list
##' @param y the new list for merging
##' @param wild wild-card name; see details
##' @param warn issue warning if nothing found to update
##' @param context description of usage context
##' @param ... not used
##' @param strict logical indicating whether or not new items should be allowed in the list upon merging.
##' @rdname merge
##' @details
##' Wild-card names (\code{wild}) are always retained in \code{x} and are brought along from \code{y} only when \code{!strict}.
##' @export
merge.list <- function(x,y,...,strict=TRUE,
                       warn=TRUE,context="object",wild="...") {

  left <- x
  right <- as.list(y)

  if(strict) {
    right <- right[names(right)!=wild | is.null(names(right))]
  }

  ## Merge two lists
  common <- intersect(names(left), names(right))
  common <- common[common != wild]

  left[common] <- right[common]

  if(!strict)  {
    new <- !is.element(names(right),names(left)) | names(right) == wild
    left <- c(left,right[new])
  } else {
    if(length(common)==0 & warn) warning(paste0("Found nothing to update: ", context), call.=FALSE)
  }
  left
}

blank_names <- function(x,y="") {
    if(is.null(names(x))) return(rep(y,length(x)))
    return(names(x))
}
fillin_names <- function(x,y="..none", z="..") {
    i <- which(names(x)=="")
    names(x)[i] <- paste0(y,i,z)
    x
}

render_time <- function(x) {
    add <- times <- numeric(0)
    if(!is.mt(x@add)){add <- x@add}
    if(x@end >=0){times <-seq(x@start,x@end,x@delta)}
    times <- invisible(as.numeric(c(times,add)))
    if(is.mt(times)) {return(0)}
    sort(unique(times[times>=0]))
}


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




##' Coerce a list to a matrix
##'
##' All elements of the list must be of length 1.
##'
##' @param x a named list, with each element of length 1
##' @param ... not used
##' @param nrow the number of rows to make the matrix
##' @return matrix with colnames set to the names of x
##' @rdname as.matrix
##' @author Kyle Baron
##' @examples
##' x <- list(a=1, b=2, c=3)
##' as.matrix(x,nrow=3)
as.matrix.list <- function(x,...,nrow=1) {

    if(!all(sapply(x,length)==1))
        stop("Cannot coerce list to matrix; some elements not length 1")

    matrix(unlist(x),
           ncol=length(x),
           nrow=nrow,
           byrow=TRUE,
           dimnames=list(NULL,names(x))
           )
}

ctack <- function(x,...) UseMethod("ctack")
ctack.matrix <- function(x,...) {
    args <- list(...)
    if(length(args)==0) stop("no arguments passed")
    cbind(x,matrix(unlist(args), ncol=length(args), nrow=nrow(x),byrow=TRUE, dimnames=list(NULL,names(args))))
}

bind_col <- function(x,y,z) {
    cbind(x,matrix(z,ncol=1, nrow=nrow(x), byrow=TRUE, dimnames=list(NULL, y)))
}


setGeneric("valid_project", function(x,...) standardGeneric("valid_project"))
setMethod("valid_project", "mrgmod", function(x) {
    dir <- project(x)
    (file.exists(dir)) & (1-file.access(dir,2))
})

setGeneric("found_model_file", function(x,...) standardGeneric("found_model_file"))
setMethod("found_model_file", "mrgmod", function(x) file.exists(cfile(x)))


##' Get the package models directory.
##'
##' @export
mrgsolve_models <- function() {
    file.path(path.package("mrgsolve"), "models")
}

setGeneric("dll_loaded", function(x,...) standardGeneric("dll_loaded"))

setMethod("dll_loaded", "mrgmod", function(x,...) {
    l <- getLoadedDLLs()
    tag <- ifelse(inherits(x,"lockedmod"),x@dllname , dllname(x))
    l <- l[is.element(names(l),tag)]
    is.element(sodll(x), sapply(l,FUN=function(x) x[["path"]]))
})
setMethod("dll_loaded", "lockedmod",  function(x,...) {
    l <- getLoadedDLLs()
    tag <- ifelse(inherits(x,"lockedmod"),x@dllname , dllname(x))
    l <- l[is.element(names(l),tag)]
    is.element(sodll(x), sapply(l,FUN=function(x) x[["path"]]))
})
setMethod("dll_loaded", "packmod",function(x,...) {
    l <- getLoadedDLLs()
    any(names(l) == x@package)
})
setMethod("dll_loaded", "character", function(x,...) {
    is.element(x,loaded_dll_files())
})


##' Simulate from a multivariate normal distribution with mean zero.
##'
##' @param mat a positive-definite matrix
##' @param n number of variates to simulate
##' @param seed if not null, passed to set.seed
##' @export
mvgauss <- function(mat, n=10, seed=NULL) {
    if(!is.null(seed)) set.seed(seed)
    .Call("mrgsolve_MVGAUSS", PACKAGE="mrgsolve", mat, n,-1)
}


simulateres <- function(n1,omega,n2,sigma) {
    .Call("mrgsolve_SIMRE", PACKAGE="mrgsolve", n1, omega,n2,sigma,-1)
}

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

pfile <- function(package,dir,file,ext=NULL) {
    ans <- file.path(path.package(package),dir,file)
    if(is.character(ext)) {
        ans <- paste0(ans, ".", ext)
    }
    return(ans)
}

check_funs <- function(x,report=FALSE) {
    return(x)
    if(!is.loaded(x@func[1],x@func[2])) {
        x@func <- c("MRGSOLVE_NO_ODE_FUN", "mrgsolve")
        if(report) message("ODE fun updated")
    }
    if(!is.loaded(x@init_fun[1],x@init_fun[2])) {
        x@init_fun <- c("MRGSOLVE_NO_INIT_FUN", "mrgsolve")
        if(report) message("INIT fun updated")
    }
    if(!is.loaded(x@table_fun[1],x@table_fun[2])) {
        x@table_fun <- c("MRGSOLVE_NO_TABLE_FUN", "mrgsolve")
        if(report) message("TABLE fun updated")
    }
}


writeable <- function(x) file.access(x,mode=2)==0


nofun <- function(x) {
   mod <- new("mrgmod")
   x@func <-  mod@func
   x@init_fun <-  mod@init_fun
   x@table_fun <- mod@table_fun
   x
}

cropstr <- function(string, prefix, suffix, bump= "...") {
    nc <- nchar(string)
    total <- prefix+suffix
    if(all(nc <= total)) return(string)
    paste0(substr(string,1,prefix) , bump, substr(string,(nc-suffix+nchar(bump)+1),nc))
}

as.cvec <- function(x) {
    x <- gsub("^\\s+|\\s+$", "", x, perl=TRUE)
    unlist(strsplit(as.character(x),"\\s*(\n|,|\\s+)\\s*",perl=TRUE))
}
as.cvec2 <- function(x) {
    x <- gsub("^\\s+|\\s+$", "", x, perl=TRUE)
    unlist(strsplit(as.character(x),"\\s*(\n|,)\\s*",perl=TRUE))
}



vgrep <- function(pattern,x) {
    ##unique(unlist(sapply(pattern, grep, x=x, value=TRUE)))
    unique(unlist(lapply(pattern, grep, x=x, value=TRUE)))
}

render_errors <- function(x) {
    if(length(x)==0) return(x)
    x <- paste("  >", x)
    x <- c(" ", x)
    paste(x, collapse="\n")
}


test_stop <- function() {
    foo <- try(.Call("mrgsolve_test_stop", package="mrgsolve"))
    return(foo)
}


decorr <- function(x) {
    off <- x[lower.tri(x)]
    if(any(off < -1 | off > 1)) stop("For correlation matrix, all off-diagonal elements must be in [-1,1].")
    return(invisible(.Call("mrgsolve_decorr", x)))
}


##' Create an idata data set
##'
##'
##' @param ... passed to expand.grid
##' @param KEEP.OUT.ATTRS passed to expand.grid
##' @param stringsAsFactors passed to expand.grid
##'
idata <- function(...,KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) {
    x <- expand.grid(..., KEEP.OUT.ATTRS=KEEP.OUT.ATTRS, stringsAsFactors=FALSE)
    if(!exists("ID", x)) x <- cbind(data.frame(ID=1:nrow(x)),x)
    if(any(sapply(x,mode) !="numeric")) warning("non-numeric data were found.")
    return(x)



}


fixnames <- function(x,prefix,...) {
    return(x)
    if(is.null(names(x))) {
        names(x) <- rep(prefix,length(x))
    }
    names(x)[nchar(names(x))==0] <- prefix

    return(x)
}

##' Create data sets.
##'
##' @param ... passed to \code{\link{expand.grid}}
##' @export
##' @details
##' An ID column is added as \code{1:nrow(ans)}
##'
##' @examples
##' idata <- expand.idata(CL=c(1,2,3), VC=c(10,20,30))
##'
##' doses <- expand.ev(amt=c(300,100), ii=c(12,24), cmt=1)
##'
expand.idata <- function(...) {
  ans <- expand.grid(...,stringsAsFactors=FALSE)
  ans$ID <- 1:nrow(ans)
  shuffle(ans,"ID")
}
##' @export
##' @rdname expand.idata
expand.ev <- function(...) {
    ans <- expand.grid(...,stringsAsFactors=FALSE)
    ans$ID <- 1:nrow(ans)
    if(!exists("evid", ans)) ans$evid <- 1
    if(!exists("cmt", ans)) ans$cmt <- 1
    if(!exists("time", ans)) ans$time <- 0
    shuffle(ans,"ID")
}



rbind_fill <- function(...) {
    data.frame(bind_rows(...))
}


mapvalues <- function (x, from, to, warn_missing = FALSE) {
    if (length(from) != length(to)) {
        stop("`from` and `to` vectors are not the same length.")
    }
    if (!is.atomic(x)) {
        stop("`x` must be an atomic vector.")
    }
    if (is.factor(x)) {
        levels(x) <- mapvalues(levels(x), from, to, warn_missing)
        return(x)
    }
    mapidx <- match(x, from)
    mapidxNA <- is.na(mapidx)
    from_found <- sort(unique(mapidx))
    if (warn_missing && length(from_found) != length(from)) {
        message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
    }
    x[!mapidxNA] <- to[mapidx[!mapidxNA]]
    x
}




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

map_data_set <- function(dd,inpar) {
    .Call("mrgsolve_map_data_set", data.matrix(dd),inpar, PACKAGE="mrgsolve")
}



tolist <- function(x,concat=TRUE,envir=list()) {

    if(is.null(x)) return(list())
    x <- gsub("(,|\\s)+$", "", x)
    x <- x[!(grepl("^\\s*$",x,perl=TRUE))]
    x <- x[x!=""]
    if(length(x)>1) x <- paste(x, collapse=',')
    return(eval(parse(text=paste0("list(", x, ")")),envir=envir))

}

toolist <- function(x,backticks=TRUE) {
    x <- unlist(.Call("mrgsolve_tokens", x, c(",", "\"")))
    x <- .Call("mrgsolve_tokens", x[x!=""], c("=", "\""))

    s <- sapply(x,length)==1
    v <- lapply(x, FUN=function(x) as.character(x[2]))
    n <- sapply(x, FUN=function(x) gsub("(^\\s*|\\s*$)", "", x[1]))

    if(any(s)) {
        v[s] <- n[s]
        n[s] <- ""
    }

    names(v) <- n

    if(backticks) {
        hasbt <- grepl("^`.*`$", unlist(v))
        v[hasbt] <- lapply(v[hasbt], eval.backticks)
        v[hasbt] <- lapply(v[hasbt], as.character)
    }

    v <- lapply(v, type.convert, as.is=TRUE)
    v
}

tovec <- function(x,concat=TRUE) {
    if(is.null(x)) return(numeric(0))
    ##x <- gsub(eol.comment, "\\1", x)
    x <- gsub("(,|\\s)+$", "", x)
    if(concat) {
        x <- x[!(grepl("^\\s*$",x,perl=TRUE))]
        x <- x[x!=""]
        if(length(x)>1) x <- paste(x, collapse=',')
    }
    x <- type.convert(unlist(strsplit(x,split="\\,|\n|\\s+",perl=TRUE)), as.is=TRUE)
    x[nchar(x)>0]
}

parens <- function(x) paste0("(",x,")")

eval.backticks <- function(x) {
    hasbt <- grepl("^`.*`$", x)
    if(!any(hasbt)) return(x)
    x <- gsub("`", "", x)
    x <- eval(parse(text=x))
    x
}




##' Create create character vectors.
##'
##' @param x comma-separated quoted string (for \code{cvec})
##' @param ... unquoted strings (for \code{ch})
##' @export
##' @examples
##'
##' cvec("A,B,C")
##' ch(A,B,C)
##' s(A,B,C)
##'
setGeneric("cvec", function(x,...) standardGeneric("cvec"))
##' @export
##' @rdname cvec
setMethod("cvec", "character", as.cvec)
##' @export
##' @rdname cvec
ch <- function(...) as.character(match.call(expand.dots=TRUE))[-1]
##' @export
##' @rdname cvec
s <- ch

##' Access or clear arguments for \code{mrgsim}.
##'
##' @export
##' @param x model object
##' @param clear logical indicating whether or not clear args from the model object
##' @param ... passed along
##' @return If \code{clear} is \code{TRUE}, the argument list is cleared and the model object is returned.  Otherwise, the argument list is returned.
simargs <- function(x,...) UseMethod("simargs")
##' @export
##' @rdname simargs
simargs.mrgmod <- function(x,clear=FALSE,...) {

    if(clear) {
        x@args <- list()
        return(x)
    }
    x@args
}



shuffle <- function (x, who, after = NA)  {
    names(x) <- make.unique(names(x))
    who <- names(x[, who, drop = FALSE])
    nms <- names(x)[!names(x) %in% who]
    if (is.null(after))
        after <- length(nms)
    if (is.na(after))
        after <- 0
    if (length(after) == 0)
        after <- length(nms)
    if (is.character(after))
        after <- match(after, nms, nomatch = 0)
    if (after < 0)
        after <- length(nms)
    if (after > length(nms))
        after <- length(nms)
    nms <- append(nms, who, after = after)
    x[nms]
}


filename <-  function (dir, run = NULL, ext = NULL,short=FALSE) {
    if(short) dir <- build_path(dir)
    file.path(dir, paste0(run, ext))
}



mesg <- function(...) cat(...,"\n",sep=" ", file="MESSAGES",append=TRUE)


## https://github.com/RcppCore/Rcpp/commit/59d3bf2e22dafb853c32d82b5e42899152f85c20
build_path <- function(x) {
    if(.Platform$OS.type != "windows") return(x)
    x <- normalizePath(x)
    if (grepl(' ', x, fixed=TRUE)) x <- utils::shortPathName(x)
    x <- gsub("\\\\", "/", x)
    return(x)
}


##' Set RNG to use L'Ecuyer-CMRG.
##'
##'
##' @export
##'
mcRNG <- function() base::RNGkind("L'Ecuyer-CMRG")


if.file.remove <- function(x) {
    if(file.exists(x)) file.remove(x)
}

#' rename columns from vector for new names
#' @param .df dataframe to rename
#' @param new_names vector of names using syntax "<newname>" = "<oldname>"
#' @examples 
#' rename_cols(Theoph, c("dv" = "conc", "ID" = "Subject"))
rename_cols <- function(.df, new_names) {
  if (!all(new_names %in% names(.df))) {
    missing <- new_names[which(!new_names %in% names(.df))]
    stop(paste("the following columns do not exist in the dataset: ", paste(missing, collapse = ", ")))
  }
  matches <- match(new_names, names(.df))
  names(.df)[matches] <- names(new_names)                 
  return(.df)
}

as_character_args <- function(x) {
  x <- deparse(x)
  x <- gsub("^.*\\(|\\)$", "", x)
  x
}


set_altname <- function(x) {
  if(length(x)==0) return(as.character(x))
  y <- strsplit(as.character(x),"\\s*=\\s*")
  to <- sapply(y,`[`,1)
  from <- sapply(y,`[`,2)
  from <- ifelse(is.na(from), to, from)
  if(identical(from,to)) return(as.character(from))
  return(structure(list(from=from,to=to,rename=!identical(from,to)),class="altname"))

}

altname <- function(x,...) UseMethod("altname")
altname.default <- function(x,y,...) return(y)
altname.altname <- function(x,y, ...) {
  old <- match(y,x[["from"]])
  old <- old[!is.na(old)]
  nw <- match(x[["from"]],y)
  nw <- nw[!is.na(nw)]
  y[nw] <- x[["to"]][old]
  return(y)
}
as.character.altname <- function(x,...) {
    as.character(x[["from"]])
}


get_tokens <- function(x,unlist=FALSE) {
    if(!is.character(x)) return(character(0))
    if(unlist) return(.Call("mrgsolve_get_tokens", x)[["tokens"]])
    .Call("mrgsolve_get_tokens", x)
}

