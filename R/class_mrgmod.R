# Copyright (C) 2013 - 2020  Metrum Research Group
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

valid_funs <- function(x) {
  x1 <- length(x)==4
  x2 <- identical(names(x), c("main", "ode", "table", "config"))
  if(x1 & x2) return(list(TRUE,NULL))
  msg <- c(
    "Invalid functions specification.",
    "This model object is not compatible with the current mrgsolve version.",
    "Rebuild the model object or upgrade the mrgsolve version."
  )
  return(list(FALSE, msg))
}

check_names <- function(x,par,cmt) {
  
  x <- x[!is.element(x,c(".", "..."))]
  
  dups <- any(duplicated(x))
  us <-  any(charthere(x,"_"))
  res <- any(is.element(x,Reserved))
  alph <- !all(grepl("^[[:alpha:]]",x))
  ans <- character(0)
  
  ## Duplicate names are not allowed
  if(dups) {
    tmp <- paste(x[duplicated(x)], collapse=" ")
    ans <- c(ans,paste0("Duplicated model names: ", tmp))
  }
  ## Look for names in the Reserved word list
  if(res) {
    tmp <- paste0(x[is.element(x,Reserved)],collapse=" ")
    ans <- c(ans,paste0("Reserved words in model names: ",tmp))
  }
  ## names that don't start with alpha
  if(alph) {
    tmp <- paste0(x[!grepl("^[[:alpha:]]", x)], collapse = " ")
    ans <- c(ans, paste0("Names without leading alpha character: ", tmp))
  }
  ## Scan for names with underscores
  ## Leading underscores are not allowed
  ## Also, look for any name that conflicts with
  ##   bioav, lag-time, or infusion duration or ate
  if(us) {
    ans <- check_us(x,cmt,ans)
  }
  return(ans)
}

check_us <- function(x,cmt,ans) {
  leading <- x[substr(x,1,1)=="_"]
  if(length(leading) > 0) {
    ans <- c(ans, 
             paste0("leading underscore not allowed: ", 
                    paste(leading, collapse=" "))) 
  }
  check <- sapply(
    c("F_", "ALAG_", "D_", "R_", "N_"), 
    paste0,
    cmt, 
    simplify=TRUE
  )
  iv_name <- intersect(x,check)
  if(length(iv_name) > 0) {
    ans <- c(ans, 
             paste0("reserved symbols in model names: ", iv_name))
  } 
  return(ans)
}

check_globals <- function(x,cmt) {
  ans <- character(0)
  us <-  any(charthere(x,"_"))
  res <- any(is.element(x,Reserved_cvar))
  if(res) {
    tmp <- paste(x[is.element(x,Reserved_cvar)],collapse=" ")
    ans <- c(ans,paste0("Reserved words in model names: ",tmp))
  }
  if(us) {
    ans <- check_us(x,cmt,ans) 
  }
  return(ans)
}

protomod <- list(model=character(0),
                 modfile = character(0),
                 package=character(0),
                 soloc=tempdir(),
                 project='.',
                 start = 0.0,
                 end=24.0,
                 delta=1.0,
                 add = numeric(0),
                 tscale = as.double(1),
                 digits=-1,
                 quiet = FALSE,
                 verbose = FALSE,
                 debug=FALSE,
                 preclean=FALSE,
                 atol=1E-8,
                 rtol=1E-8,
                 ss_rtol=1e-8, 
                 ss_atol=1e-8,
                 maxsteps=20000,
                 hmin=0,
                 hmax=0,
                 ixpr=0,
                 mxhnil=2,
                 shlib=list(date="",par="", cmt="", compiled=FALSE, 
                            version=NULL, source=""),
                 funs = c(main=character(0),
                          ode=character(0),
                          table=character(0),
                          config=character(0)),
                 omega=new("omegalist"),
                 sigma = new("sigmalist"),
                 request="(all)",
                 param = new("parameter_list"),
                 init=new("cmt_list"),
                 capture=character(0),
                 Icap = integer(0),
                 capL = character(0),
                 Icmt = integer(0), 
                 cmtL = character(0),
                 args = list(),
                 fixed  = list(),
                 advan=13,
                 trans=1,
                 mindt=10*.Machine$double.eps,
                 code = character(0),
                 annot = list(),
                 envir = new.env(),
                 plugin = character(0), 
                 ss_cmt = integer(0)
)

slot.names <- names(protomod)
slots <- sapply(protomod, class)
names(slots) <- names(protomod)

# this will never get counted as covered; copied into class
# nocov start
valid.mrgmod <- function(object) {
  tags <- names(object)
  tags[["capture"]] <- NULL
  tags <- unlist(tags, use.names=FALSE)
  x <- check_names(tags,Pars(object),Cmt(object))
  x1 <- length(x)==0
  x2 <- object@advan %in% c(0,1,2,3,4,13)
  x3 <- !any(object@capture %in% Cmt(object))
  fun <- valid_funs(object@funs)
  cool <- x1 & x2 & fun[[1]] & x3
  if(cool) return(TRUE)
  x <- c(x,fun[[2]])
  if(!x2) x <- c(x,"advan must be 1, 2, 3, 4, or 13")
  if(!x3) {
    invalid <- intersect(object@capture,Cmt(object))
    invalid <- paste0(invalid,collapse=",")
    invalid <- paste0("compartment should not be in $CAPTURE: ", invalid)
    x <- c(x,invalid)
  }
  return(x)
}
# nocov end



#' S4 class for mrgsolve model object
##'
#' @section Notes:
#' \itemize{
#' \item Spaces in paths (\code{project} and \code{soloc}) are prohibited.
##'
#' }
##'
#' @slot model model name \code{<character>}
#' @slot modfile source model specification file name \code{<character>}
#' @slot package the shared object file name \code{character>}
#' @slot project working directory; must be writeable with no spaces 
#' \code{<character>}
#' @slot start simulation start time \code{<numeric>}
#' @slot end simulation end time \code{<numeric>}
#' @slot delta simulation time interval \code{<numeric>}
#' @slot add additional simulation times \code{<numeric-vector>}
#' @slot param \code{<parameter_list>}
#' @slot fixed a \code{<parameter_list>} of fixed value parameters; 
#' these are not updatable from \code{R}
#' @slot init \code{<cmt_list>}
#' @slot digits significant digits in simulated output; negative integer means 
#' ignore \code{<numeric>}
#' @slot hmin passed to \code{\link[=solversettings]{dlsoda}}  \code{<numeric>}
#' @slot hmax passed to \code{\link[=solversettings]{dlsoda}} \code{<numeric>}
#' @slot mxhnil passed to \code{\link[=solversettings]{dlsoda}} 
#' \code{<numeric>}
#' @slot ixpr passed to \code{\link[=solversettings]{dlsoda}} \code{<numeric>}
#' @slot atol passed to \code{\link[=solversettings]{dlsoda}} \code{<numeric>}
#' @slot rtol passed to \code{\link[=solversettings]{dlsoda}} \code{<numeric>}
#' @slot ss_rtol relative tolerance to use when finding PK steady state \code{<numeric>}
#' @slot ss_atol absolute tolerance to use when finding PK steady state \code{<numeric>}
#' @slot maxsteps passed to \code{\link[=solversettings]{dlsoda}} \code{<numeric>}
#' @slot preclean passed to R CMD SHLIB during compilation \code{<logical>}
#' @slot verbose print run information to screen \code{<logical>}
#' @slot quiet print various information to screen \code{<logical>}
#' @slot debug not used
#' @slot tscale used to scale time in simulated output \code{<numeric>}
#' @slot omega \code{\link{matlist}} for simulating individual-level random 
#' effects
#' @slot sigma \code{\link{matlist}} for simulating residual error variates
#' @slot args \code{<list>} of arguments to be passed to \code{\link{mrgsim}}
#' @slot advan either 2, 4, or 13 \code{<numeric>}
#' @slot trans either 1, 2, 4, or 11 \code{<numeric>}
#' @slot request  vector of compartments to request \code{<character>}
#' @slot soloc directory path for storing the model shared object 
#' \code{<character>}
#' @slot code a character vector of the model code
#' @slot capture a character vector of variables that are captured from 
#' the simulation \code{<character>}
#' @slot mindt minimum time between simulation records \code{<numeric>}
#' @slot envir internal model environment \code{<environment>}
#' @slot shlib a list of data related to build outcome \code{<list>}
#' @slot funs symbol names for model functions in the shared object
#' @slot annot model annotations \code{<list>}
#' @slot plugin model plugins \code{<character>}
#' @slot Icap capture indices to recover in the simulation \code{<integer>}
#' @slot capL labels for \code{Icap};  \code{<character>}
#' @slot Icmt compartment indices to recover in the simulation \code{<integer>}
#' @slot cmtL labels for \code{Icmt}; \code{<character>}
#' @slot ss_cmt compartments numbers to be considered when advancing the system
#' to steady state \code{<integer>}
#' 
#' @seealso \code{\link[mrgsolve]{update}}, \code{\link{solversettings}}
#' @keywords internal
#' @export
setClass("mrgmod",slots=slots, validity=valid.mrgmod, prototype=protomod)

setClass("packmod",
         prototype = list(shlib=list(compiled=TRUE, date="date of package compile"),
                          package="", src="", header=""),
         contains="mrgmod",
         slots=c(
           package="character",
           src="character", 
           header="character"
         )
)

initialize_mrgmod <- function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@shlib[["cmt"]] <- Cmt(.Object)
  .Object@shlib[["par"]] <- Pars(.Object)
  .Object@shlib[["neq"]] <- length(.Object@shlib[["cmt"]])
  .Object@shlib[["version"]] <- GLOBALS[["version"]]
  .Object <- default_outputs(.Object)
  .Object@ss_cmt <- seq_along(Cmt(.Object)) - 1L
  .Object
}
setMethod("initialize", "mrgmod", initialize_mrgmod)

#' Return a pre-compiled, PK/PD model
#' 
#' @param ... passed to \code{\link[mrgsolve]{update}}
#' 
#' @return 
#' A \code{packmod} object, ready to simulate.
#' 
#' @examples
#' 
#' mod <- mrgsolve::house()
#' 
#' see(mod)
#' 
#' mod %>% ev(amt=100) %>% mrgsim %>% plot
#' 
#' @keywords internal
#' @export
house <- function(...) {
  project <- mrgsolve_file("project")
  att <- as_pack_mod("housemodel", project, "mrgsolve")
  x <- new("packmod",
           att[["mod"]],
           package = "mrgsolve",
           model = "housemodel"
  )
  x@soloc <- dirname(sodll(x))
  x <- compiled(x,TRUE)
  x <- update(x,...,open=TRUE)
  x
}

as_pack_mod <- function(model, project, PACKAGE) {
  x <- mread(model, project, compile=FALSE, udll=FALSE, ns=FALSE)
  code <- readLines(cfile(x),warn=FALSE)
  x <- new("packmod",
           x,
           package=PACKAGE,
           model = model
  )
  soloc <- soloc(x)
  source <- file.path(soloc,compfile(model(x)))
  x@shlib$par <- Pars(x)
  x@shlib$cmt <- Cmt(x)
  x@shlib$source <- NULL
  x@code <- code
  x <- relocate_funs(x, PACKAGE)
  x@soloc <- ""
  return(list(mod=x, soloc=soloc, source=source))
}

#' Check if an object is a model object 
#' 
#' The function checks to see if the object is either
#'  \code{mrgmod} or \code{packmod}.
#' 
#' @param x any object
#' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
#' @export
is.mrgmod <- function(x) inherits(x,c("mrgmod","packmod"))

see_compfile <- function(x) {
  file <- file.path(soloc(x),compfile(model(x)))
  if(!file_exists(file)) {
    message("Could not find the compiled code for this model.")
  }
  cat(readLines(file),sep="\n")
}

setMethod("project", "mrgmod", function(x,...) {
  return(x@project)
})

setMethod("project", "packmod", function(x,...) {
  return(file.path(path.package(x@package),"project"))
})

#' @rdname cmtn
#' @param tag compartment name
#' @export
setMethod("cmtn", "mrgmod", function(x,tag,...) {
  return(which(cmt(x)==tag))
})

neq <- function(x) length(Init(x))
npar <- function(x) length(Pars(x))
pars <- function(x) names(param(x))
shlib <- function(x) x@shlib
cmt <- function(x) names(Init(x))
dllname <- function(x) x@package
model <- function(x) x@model
modfile <- function(x) x@modfile
Param <- function(x) x@param
Param_list <- function(x) x@param@data
Pars <-  function(x) names(x@param@data)
Init <- function(x) x@init
Cmt <-  function(x) names(x@init@data)
Cmti <- function(x) x@Icmt
Capturei <- function(x) x@Icap
CAPTUREI <- function(x) c(length(x@capture),x@Icap-1L)

#' Return the location of the model shared object
##'
#' @param x model object
#' @param short logical; if \code{TRUE}, \code{soloc} will
#'  be rendered  with a short path name
#' 
#' @rdname soloc
#' 
#' @examples
#' mod <- mrgsolve::house()
#' soloc(mod)
#' 
#' @export
soloc <- function(x,short=FALSE) {
  if(short) return(build_path(x@soloc))
  return(x@soloc)
}

cfile <- function(x,...) {
  return(pathfun(filename(project(x),modfile(x),...)))
}

setMethod("sodll", "mrgmod", function(x,...) {
  return(pathfun(file.path(soloc(x,...),dllfile(x))))
})

setMethod("sodll", "packmod", function(x,...) {
  return(pathfun(getLoadedDLLs()[[x@package]][["path"]]))
})

#' Get all names from a model object
#' 
#' @param x the model object
#' 
#' @aliases names,mrgmod-method
#' 
#' @examples
#' mod <- mrgsolve::house()
#' names(mod)
#' 
#' @export
setMethod("names", "mrgmod", function(x) {
  ans <- list()
  ans$param <- Pars(x)
  ans$init <- Cmt(x)
  ans$capture <- unname(x@capture)
  ans$omega <- names(omat(x))
  ans$sigma <- names(smat(x))
  ans$omega_labels <- labels(omat(x))
  ans$sigma_labels <- labels(smat(x))
  return(ans)
})



#' Coerce a model object to list
#' 
#' @param x mrgmod object
#' @param deep if `TRUE`, extra information is returned
#' (see details). 
#' @param ... not used
#' 
#' @details 
#' If `deep` is `TRUE`, then the values for
#' `trans`,`advan`, and `mindt` are
#' returned as well as a summary of internal model 
#' functions (with a call to `mrgsolve:::funset`).
#' 
#' @section Slots:
#' - `npar`: number of parameters
#' - `neq`: number of compartments or differential equations
#' - `pars`: names of model parameters
#' - `covariates`: names of parameters identified as covariates
#' - `cmt`: names of model compartments
#' - `param`: the parameter list
#' - `init`: initial condition list
#' - `omega`: `$OMEGA` matrices, as a `matlist` object
#' - `sigma`: `$SIGMA` matrices, as a `matlist` object
#' - `fixed`: named list of `$FIXED` values
#' - `model`: model name
#' - `project`: model project directory
#' - `soloc`: directory where the model is being built
#' - `sodll`: complete path to the model shared object
#' - `cfile`: path for the model source code file 
#' - `shlib`: list of compilation information
#' - `start`: simulation start time
#' - `end`: simulation end time
#' - `delta`: simulation time step
#' - `add`: additional simulation times
#' - `capture`: names of captured data items
#' - `request`: compartments requested upon simulation
#' - `cmti`: named indices for current output compartments
#' - `capturei`: named indices for current output capture
#' - `random`: names and labels of `$OMEGA` and `$SIGMA`
#' - `code`: model source code from `cfile`
#' - `details`: model details data frame
#' - `atol`: see [solversettings]
#' - `rtol`: see [solversettings]
#' - `ss_atol`: absolute tolerance to use when advancing to PK steady state
#' - `ss_rtol`: relative tolerance to use when advancing to PK steady state
#' - `maxsteps`: see [solversettings]
#' - `hmin`: see [solversettings]
#' - `hmax`: see [solversettings]
#' - `envir`: the model environment
#' - `plugins`: plugins invoked in the model
#' - `digits`: number of digits to request in simulated data

#' - `tscale`: multiplicative scalar for time in results only
#' - `mindt`: simulation output time below which there model will assume to 
#'   have not advanced
#' - `preclean`: logical indicating to clean up compilation artifacts prior
#'   to compiling
#' - `debug`: print debugging information during simulation run
#' - `verbose`: print extra information during setup for model run
#' 
#' @md
#' @export
setMethod("as.list", "mrgmod", function(x, deep = FALSE, ...) {
  
  within(list(), {
    verbose <- x@verbose
    debug <- x@debug
    preclean <- x@preclean
    mindt <- x@mindt
    tscale <- x@tscale
    request <- x@request
    digits <- x@digits
    plugins <- x@plugin
    envir <- x@envir
    hmax <- x@hmax
    hmin <- x@hmin
    maxsteps <- x@maxsteps
    ss_atol <- x@ss_atol
    ss_rtol <- x@ss_rtol
    rtol <- x@rtol
    atol <- x@atol
    if(deep) { 
      trans <- x@trans 
      advan <- x@advan
      functions <- funset(x)
    }
    details <- x@annot
    code <- x@code
    random <- names(x)[c("omega", "sigma", "omega_labels", "sigma_labels")]
    out_cmt <- x@cmtL
    out_cap <- x@capL
    request <- x@request
    capture <- x@capture
    add <- x@add
    delta <- x@delta
    end <- x@end
    start <- x@start
    shlib <- shlib(x)
    cfile <- cfile(x)
    sodll <- sodll(x)
    soloc <- soloc(x)
    project <- project(x)
    model <- model(x)
    fixed <- as.list(x@fixed)
    sigma <- as.list(smat(x))
    omega <- as.list(omat(x))
    init <- as.list(init(x))
    param <- as.list(param(x))
    cmt <- cmt(x)
    
    covariates <- as.character(x@shlib$covariates)
    pars <- pars(x)
    neq <- neq(x)
    npar <- npar(x)
  })
})


#' Select parameter values from a model object
#' 
#' The \code{$} and \code{[[} operators get the value 
#' of a single parameter in the model.  The 
#' \code{[} gets several values, returning a 
#' named list.  
#' 
#' @param x mrgmod object
#' @param name parameter to take
#' @param i an element to select
#' @param exact not used
#' @rdname mrgmod_extract
#' @export
setMethod("$", "mrgmod", function(x, name) {
  x[[name]]
})

#' @rdname mrgmod_extract
#' @export
setMethod("[[", "mrgmod", function(x, i, exact=TRUE) {
  if(i %in% Pars(x)) {
    return(unname(as.numeric(allparam(x))[i]))  
  }
  if(i %in% Cmt(x)) {
    return(unname(as.numeric(init(x))[i]))  
  }
  l <- as.list(x)
  if(exists(i,l)) {
    return(unname(l[[i]]))  
  }
  wstop(
    "item '", i, "' not found or not extractable with $ or [[ operator."
  )
})

#' @rdname mrgmod_extract
#' @export
setMethod("[", "mrgmod", function(x, i) {
  env <- c(as.list(param(x)),as.list(init(x)))
  if(!all(i %in% names(env))) {
    env <- c(env,as.list(x))  
  }
  if(!all(i %in% names(env))) {
    wrong <- shQuote(setdiff(i, names(env)))
    for(j in seq_along(wrong)) {
      message(" Problem: item ", wrong[j], " not found")    
    }
    wstop("requested items(s) not found or not extractable with [ operator.")      
  }
  env[i]
})

#' @export
as.environment.mrgmod <- function(x) {
  list2env(c(as.list(param(x)),as.list(init(x)),as.list(x)))  
}

#' @rdname see
#' @export
setMethod("see", "mrgmod", function(x,raw=FALSE, ...) {
  if(raw) return(x@code)
  what <- x@code
  if(length(what)==0) {
    if(file_exists(cfile(x))) what <- readLines(cfile(x), warn=FALSE)
  }
  if(length(what)==0) {
    warning("No code to show.")
  } else {
    cat("\nModel file: ", basename(cfile(x)), "\n")
    cat(what, sep="\n")
  }
  return(invisible(NULL))
})

#' @rdname loadso
#' @export
loadso.mrgmod <- function(x,...) {
  if(inherits(x,"packmod")) {
    return(invisible(x))  
  }
  sofile <- sodll(x)
  if(!file.exists(sofile)) {
    wstop("[loadso] the model dll file doesn't exist")  
  }
  if(.Platform$OS.type!="unix") {
    try(dyn.unload(sofile),silent=TRUE)
  }
  foo <- try(dyn.load(sofile))
  if(class(foo)=="try-catch") {
    wstop("[loadso] failed to load the model dll file")
  }
  return(invisible(x))
}

unloadso.mrgmod <- function(x, ...) {
  out <- try(dyn.unload(sodll(x)),silent=TRUE)
  if(inherits(out, "try-error")) {
    stop(out[1])
  } else {
    message("unloaded ", basename(sodll(x)))
  }
  return(invisible(NULL))
}

#' @rdname tgrid
#' @export
setMethod("stime", "mrgmod",  function(x,...) {
  render_time(x)
})

#' @rdname revar
#' @export
setMethod("revar", "mrgmod", function(x,...) {
  return(list(omega=x@omega,sigma=x@sigma))
})

#' @rdname blocks
#' @export
setMethod("blocks", "mrgmod", function(x,...) {
  what <- as.character(match.call()[-1])[-1]
  blocks_(cfile(x),what)
})

#' @rdname blocks
#' @export
setMethod("blocks", "character", function(x,...) {
  what <- as.character(match.call()[-1])[-1]
  blocks_(x,what)
})

prvec <- function(x, ...) {
  x <- strwrap(paste0(x,collapse=", "),...)    
  paste0(x,collapse="\n")
}

#' Print summary of a mrgmod object
#' @param object a mrgmod object
#' @param ... not used
#' @export
summary.mrgmod <- function(object,...) {
  l <- as.list(object)
  ncmt <- l[["neq"]]
  npar <- l[["npar"]]
  cat("Model: ", l$model, "\n",sep="")
  cat("- Parameters: [", npar, "]", "\n",sep="")
  cat(prvec(l$pars,width = 50,prefix="  "), "\n",sep="")
  cat("- Compartments: [", ncmt, "]", "\n",sep="")
  cat(prvec(l$cmt,width = 50, prefix = "  "), "\n",sep="")
  cat("- Captured: [", length(l[["capture"]]), "]", "\n",sep="")
  o <- l$capture
  if(length(o)==0) o <- "<none>"
  cat(prvec(o, width = 50, prefix = "  "), "\n",sep="")
  outputs <- c(l$out_cmt,l$out_cap)
  cat("- Outputs: [",length(outputs),"]", "\n",sep="")
  cat(prvec(outputs,width = 50, prefix="  "), "\n",sep="")
  return(invisible(NULL))
}

blocks_ <- function(file,what) {
  if(length(what)==0) what <- c("PARAM","MAIN", "ODE","DES", "TABLE")
  if(!file_exists(file)) wstop("can't find model file.")
  if(grepl("\\.Rmd$", file)) {
    bl <- modelparse_rmd(readLines(file, warn=FALSE))
  } else {
    bl <- modelparse(readLines(file, warn=FALSE))    
  }
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

parin <- function(x) {
  list(
    rtol=x@rtol,atol=x@atol,ss_rtol=x@ss_rtol,ss_atol=x@ss_atol,
    hmin=as.double(x@hmin), hmax=as.double(x@hmax),
    maxsteps=x@maxsteps,ixpr=x@ixpr,mxhnil=x@mxhnil,
    verbose=as.integer(x@verbose),debug=x@debug,
    digits=x@digits, tscale=x@tscale,
    mindt=x@mindt, advan=x@advan, 
    ss_n = 500, ss_fixed = FALSE, 
    ss_cmt = x@ss_cmt
  )
}

#' Show model specification and C++ files
#' 
#' @param x model object
#' @param spec logical; show the model specification file
#' @param source logical; show the C++ file that is actually compiled
#' @param ... not used
#' @export
#' @keywords internal
file_show <- function(x,spec=TRUE,source=TRUE,...) {
  stopifnot(is.mrgmod(x))
  what <- list()
  if(spec) what$spec <- cfile(x)
  if(source) what$source <- x@shlib$source
  do.call(base::file.show,what)
}


#' @export
all.equal.mrgmod <- function(target, current,...) {
  target.env <- as.list(target@envir)
  current.env <- as.list(current@envir)
  target@envir <- current@envir <- new.env()
  t1 <- isTRUE(identical(target,current))
  t2 <- identical(target.env, current.env)
  all(t1,t2)
}

#' @export
.DollarNames.mrgmod <- function(x, pattern){
  grep(pattern, names(param(x)), value=TRUE)
}

#' Show names of current output variables
#' 
#' @param x mrgmod object
#' @param unlist if `TRUE` then a character vector (rather than list) is 
#' returned
#' 
#' @return
#' When `unlist` is `FALSE` (default) : a named list, with `cmt` showing names 
#' of output compartments and `capture` giving names of output variables in 
#' capture.  When `unlist` is `TRUE`, then a single, unnamed character vector
#' of outvar names is returned.
#' 
#' @examples
#' 
#' outvars(mrgsolve::house())
#' 
#' @md
#' @export
outvars <- function(x, unlist = FALSE) {
  ans <- list(cmt = x@cmtL, capture = x@capL)
  if(unlist) unlist(ans, use.names=FALSE)
  ans
}
