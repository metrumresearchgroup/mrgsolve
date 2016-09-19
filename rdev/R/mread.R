
##' @include modspec.R
NULL


##' Write, compile, and load model code.
##'
##' This is a convenience function that ultimately calls \code{\link{mread}}.
##'
##' @param model model name
##' @param project project name
##' @param code character string specifying a \code{mrgsolve} model
##' @param ... passed to \code{\link{mread}}
##' @details
##' Note that the arguments are in slightly different order than \code{\link{mread}}.  The default \code{project} is \code{tempdir()}.
##'
##' @examples
##'
##' code <- '
##' $CMT DEPOT CENT
##' $PKMODEL ncmt=1, depot=TRUE
##' $MAIN
##' double CL = 1;
##' double V = 20;
##' double KA = 1;
##' '
##'
##' mod <- mcode("example",code)
##'
##' @export
##'
mcode <- function(model,code, project=tempdir(),...) {
  mread(model=model,project=project,code=code,...)
}




##' Read a model specification file
##'
##' @param model model name
##' @param project working directory
##' @param code a character string with model specification code
##' @param ignore.stdout passed to system call for compiling model
##' @param raw if TRUE, return a list of raw output
##' @param compile try to compile the model and load the shared object
##' @param check.bounds check boundaries of parameter list
##' @param audit check the model specification file for errors
##' @param warn logical; if \code{TRUE}, print warning messages that may arise
##' @param soloc directory where model shared object is stored
##' @param udll use unique name for shared object
##' @param quiet don't print messages when compiling
##' @param preclean logical; if \code{TRUE}, compilation artifacts are cleaned up first
##' @param ... passed along
##' @export
##' 
##' @section Model Library:
##' 
##' \code{mrgsolve} comes bundled with several precoded PK, PK/PD, and other systems models
##' that are accessible via the \code{mread} interface.  
##' 
##' Models available in the library include:
##' 
##' \itemize{
##'   \item PK models: \code{pk1cmt}, \code{pk2cmt}, \code{pk3cmt}, \code{tmdd}
##'   \item PKPD models: \code{irm1}, \code{irm2}, \code{irm3}, \code{irm4}, \code{emax}, \code{effect}
##'   \item Other models: \code{viral1}, \code{viral2}
##' }
##' 
##' When the library model is accessed, \code{mrgsolve} will compile and load
##' the model as you would for any other model.  It is only necessary to 
##' reference the correct model name and point the \code{project} argument
##' to the \code{mrgsolve} model library location via \code{\link{modlib}}.
##' 
##' For more details, see \code{\link{modlib_pk}}, \code{\link{modlib_pkpd}}, 
##' \code{\link{modlib_tmdd}}, \code{\link{modlib_viral}}, and \code{\link{modlib_details}}
##' for more information about the state variables and parameters in each model.
##' 
##' @examples
##'
##'
##' code <- '
##' $PARAM CL = 1, VC = 5
##' $CMT CENT
##' $ODE dxdt_CENT = -(CL/VC)*CENT;
##' '
##'
##' mod <- mcode("ex_mread",code)
##'
##' mod
##'
##' mod %>% init(CENT=1000) %>% mrgsim %>% plot
##'
##' 
##' mod <- mread("irm3", modlib())
##' 
##' mod
##'
mread <- function(model=character(0),project=getwd(),code=NULL,udll=TRUE,
                  ignore.stdout=TRUE,
                  raw=FALSE,compile=TRUE,audit=FALSE,
                  quiet=getOption("mrgsolve_mread_quiet",FALSE),
                  check.bounds=FALSE,warn=TRUE,soloc=tempdir(),
                  preclean=FALSE,...) {
  
  quiet <- as.logical(quiet)
  warn <- warn & (!quiet)
  
  if(!missing(code) & missing(model)) model <- "_mrgsolve_temp"
  
  ## Check for spaces in the model name
  if(grepl(" +", model,perl=TRUE)) {
    stop("model name cannot contain spaces.")
  }
  
  if(any(grepl("\n", project))) {
    stop("project argument contains newline(s); did you mean to call mcode?",call.=FALSE) 
  }
  
  ## Both project and soloc get normalized
  project <- normalizePath(project, mustWork=TRUE, winslash="/")
  soloc <-   normalizePath(soloc, mustWork=TRUE, winslash="/")
  soloc <-   setup_soloc(soloc,model)  
  
  
  
  ## The model file is <stem>.cpp in the <project> directory
  modfile <- file.path(project,paste0(model, ".cpp"))
  
  ## If code is passed in as character:
  if(!missing(code)) {
    mod.con <- file(modfile, open="w")
    cat(code, "\n", file=mod.con)
    close(mod.con)
  }
  
  if(!file.exists(modfile)) {
    if(project==modlib()) {
      return(mintern(model)) 
    } else {
      stop(paste0("Could not find model file ", modfile), call.=FALSE)
    }
  }
  
  ## If we need a unique dll name, use rfile otherwise model
  ## This is also the "package"
  package <- ifelse(udll,rfile(model),model)
  
  if(audit) warn <- TRUE
  
  ## Read the model spec and parse:
  spec  <- modelparse(readLines(modfile,warn=FALSE))
  
  ## Block name aliases
  names(spec) <- gsub("DES", "ODE",  names(spec), fixed=TRUE)
  names(spec) <- gsub("POST", "TABLE", names(spec), fixed=TRUE)
  names(spec) <- gsub("^PK$",  "MAIN", names(spec), fixed=FALSE)
  
  
  ## Expand partial matches
  index <- pmatch(names(spec),block_list,duplicates.ok=TRUE)
  names(spec) <- ifelse(is.na(index),names(spec),block_list[index])
  
  ## Do a check on what we found in the spec
  check_spec_contents(names(spec),warn=warn,...)
  
  ## Pull out the settings now
  ## We might be passing parse settings in here ...
  SET <- tolist(spec[["SET"]])
  spec[["SET"]] <- NULL
  
  ENV <- eval_ENV_block(spec[["ENV"]])
  
  # Make a list of NULL equal to length of spec
  # Each code block can contribute to / occupy one
  # slot for each of param/fixed/init/omega/sigma
  mread.env <- parse_env(length(spec),ENV)
  
  ## The main sections that need R processing:
  spec <- move_global(spec,mread.env)
  
  ## Parse blocks
  ## Each block gets assigned a class to dispatch the handler function
  ## Also, we use a position attribute so we know 
  ## where we are when we're handling the block
  specClass <- paste0("spec", names(spec))
  for(i in seq_along(spec)) {
    spec[[i]] <- structure(.Data=spec[[i]],
                           class=specClass[i],
                           pos=i)
  }
  
  ## Call the handler for each block
  spec <- lapply(spec,handle_spec_block,env=mread.env)

  
  ## Collect the results
  param <- as.list(do.call("c",unname(mread.env$param)))
  fixed <- as.list(do.call("c",unname(mread.env$fixed)))
  init <-  as.list(do.call("c",unname(mread.env$init)))
  annot <- dplyr::bind_rows(nonull.list(mread.env$annot))
  omega <- omat(do.call("c", nonull.list(mread.env$omega)))
  sigma <- smat(do.call("c", nonull.list(mread.env$sigma)))
  
  
  ## Collect potential multiples
  subr  <- collect_subr(spec)
  table <- unname(unlist(spec[names(spec)=="TABLE"]))
  plugin <- get_plugins(spec[["PLUGIN"]])

  ## Look for compartments we're dosing into: F/ALAG/D/R
  ## and add them to CMTN
  dosing <- dosing_cmts(spec[["MAIN"]], names(init))
  SET[["CMTN"]] <- c(spec[["CMTN"]],dosing)
  
  ## Virtual compartments
  if(any(is.element("VCMT", names(spec)))) {
    what <- which(names(spec)=="VCMT")
    vcmt <- unique(names(unlist(mread.env$init[what])))
    spec[["ODE"]] <- c(spec[["ODE"]], paste0("dxdt_",vcmt,"=0;"))
  }
  
  if(raw) {
    return(list(param=as.numeric(param),
                init=as.numeric(init),
                sigma=omega,
                sigma=sigma,
                SET=unlist(SET),
                ENV=unlist(ENV),
                GLOBAL=spec[["GLOBAL"]],
                fixed=fixed,
                table=table))
  }
  
  ## Constructor for model object:
  x <- new("mrgmod",
           model=model,
           soloc=as.character(soloc),
           package=package,
           project=project,
           fixed=fixed,
           advan=subr[["advan"]],
           trans=subr[["trans"]],
           omega=omega,
           sigma=sigma,
           param=as.param(param),
           init=as.init(init),
           funs  = funs_create(model),
           capture=as.character(spec[["CAPTURE"]])
  )
  
  x <- store_annot(x,annot)
  
  ## ADVAN 13 is the ODEs
  ## Two compartments for ADVAN 2, 3 compartments for ADVAN 4
  ## Check $MAIN for the proper symbols
  if(x@advan != 13) {
    if(subr[["n"]] != neq(x)) {
      stop("$PKMODEL requires  ", subr[["n"]] , " compartments in $CMT or $INIT.",call.=FALSE)
    }
    check_pred_symbols(x,spec[["MAIN"]])
  }
  
  ## First update with what we found in the model specification file
  x <- update(x, data=SET, strict=FALSE)
  
  ## Arguments in $SET that will be passed to mrgsim
  simargs <- SET[is.element(names(SET),set_args)]
  
  if(length(simargs) > 0) x@args <- merge(x@args,simargs, strict=FALSE)
  
  ## Next, update with what the user passed in as arguments
  args <- list(...)
  x <- update(x, data=args,strict=FALSE)
  
  if(audit) audit_spec(x,spec,warn=warn)
  
  ## This must come after audit
  if(is.null(spec[["ODE"]])) spec[["ODE"]] <- "DXDTZERO();"
  
  ## These are the various #define statements
  ## that go at the top of the .cpp.cpp file
  rd <-generate_rdefs(pars=names(param),
                      cmt=names(init),
                      ode_func(x),
                      main_func(x),
                      table_func(x),
                      config_func(x),
                      model=model(x),
                      omats=omat(x),
                      smats=smat(x),
                      parsedata=SET,
                      check.bounds=check.bounds)
  
  
  ## Write the model code to temporary file
  temp_write <- tempfile()
  def.con <- file(temp_write, open="w")
  cat(
    paste0("// Source MD5: ", tools::md5sum(modfile), "\n"),
    plugin_code(plugin),
    "#include \"modelheader.h\"",
    "\n// DEFS:",
    rd, 
    ## This should get moved to rd
    "\n// FIXED:",
    fixed_parameters(fixed,SET[["fixed_type"]]),
    "\n// INCLUDES:",
    form_includes(spec[["INCLUDE"]],project),
    "\n// GLOBAL CODE BLOCK:",
    spec[["GLOBAL"]],
    "\n// CONFIG CODE BLOCK:",
    "BEGIN_config",
    "END_config",
    "\n// MAIN CODE BLOCK:",
    "BEGIN_main",
    spec[["MAIN"]],
    advtr(x@advan,x@trans),
    "END_main",
    "\n// DIFFERENTIAL EQUATIONS:",
    "BEGIN_ode",
    spec[["ODE"]],
    "END_ode",
    "\n// TABLE CODE BLOCK:",
    "BEGIN_table",
    table,
    write_capture(x@capture),
    "END_table",
    sep="\n", file=def.con)
  close(def.con)
  
  ## lock some of this down so we can check order later
  x@shlib$cmt <- cmt(x)
  x@shlib$par <- pars(x)
  x@code <- readLines(modfile, warn=FALSE)
  x@shlib$version <- GLOBALS[["version"]]
  x@shlib$source <- file.path(soloc,compfile(model))
  
  
  ## IN soloc directory
  cwd <- getwd()
  setwd(soloc)
  
  to_restore <- set_up_env(plugin,clink=c(project(x),SET$clink))
  
  ## this gets written in soloc
  write_win_def(x)
  
  on.exit({
    do_restore(to_restore)
    setwd(cwd)
  })
  
  
  same <- check_and_copy(from = temp_write,
                         to = compfile(model),
                         preclean)
  
  if(!compile) return(x)
  
  if(ignore.stdout & !quiet) {
    message("Compiling ",model(x)," ... ", appendLF=FALSE)
  }
  
  # Wait at least 2 sec since last compile
  safe_wait(x)
  cleanso(x)
  
  ## Compile the model
  ## The shared object is model-mread-source.cpp
  syst <- paste0(R.home(component="bin"), 
                 .Platform$file.sep,
                 "R CMD SHLIB ",
                 ifelse(preclean, " --preclean ", ""),
                 compfile(model))
  
  status <- suppressWarnings(system(syst,
                                    intern=ignore.stdout,
                                    ignore.stdout=ignore.stdout))
  
  
  if(!ignore.stdout) { ## not intern
    
    if(status != "0") {
      cat("\n\n")
      stop("There was a problem when compiling the model.",call.=FALSE)
    }
    
    
  } else { ## intern
    
    output <- status
    
    attributes(output) <- NULL
    
    status <- attr(status, "status")
    
    comp_success <- is.null(status) & file.exists(compout(model))
    
    if(!comp_success) {
      cat(output, sep="\n") 
      cat("\n\n")
      stop("There was a problem when compiling the model.",call.=FALSE);
    } 
    if(!quiet) message("done.")
  }
  
  
  ## Rename the shared object to unique name
  ## e.g model2340239403.so
  z <- file.copy(compout(model),sodll(x))
  
  dyn.load(sodll(x))
  
  stopifnot(all_loaded(x))
  
  x <- compiled(x,TRUE)
  
  return(x)
}

