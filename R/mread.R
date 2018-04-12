# Copyright (C) 2013 - 2018  Metrum Research Group, LLC
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

##' @include modspec.R
NULL



##' Read a model specification file
##' 
##' \code{mread} reads and parses a \code{mrgsolve} model 
##' specification file, builds the model, and returns 
##' a model object for simulation.
##' 
##'
##' @param model model name
##' @param project location of the model specification file an any 
##' headers to be included
##' @param file the full file name (with extension, but without path)
##' where the model is specified
##' @param soloc directory where model shared object is stored
##' @param code a character string with model specification code to be 
##' used instead of a model file
##' @param ignore.stdout passed to system call for compiling model
##' @param raw if TRUE, return a list of raw output
##' @param compile logical; if \code{TRUE}, the model will be built
##' @param check.bounds check boundaries of parameter list
##' @param audit check the model specification file for errors
##' @param warn logical; if \code{TRUE}, print warning messages that may arise
##' @param udll use unique name for shared object
##' @param quiet don't print messages when compiling
##' @param preclean logical; if \code{TRUE}, compilation artifacts are 
##' cleaned up first
##' @param ... passed along
##' 
##' @details
##' The \code{model} argument is required.  For typical use, 
##' the \code{file} argument is omitted and the value 
##' for \code{file} is generated from the value for \code{model}.
##' To determine the source file name, \code{mrgsolve} will look for 
##' a file extension in the value of \code{model}.  A file extension is 
##' assumed when it find sa period followed by one to three alpha-numeric 
##' characters at the end of the string (e.g. \code{mymodel.txt} but not 
##' \code{my.model}).  If no file extension is found, the extension \code{.cpp} 
##' is assumed (e.g. \code{file} is \code{<model-name>.cpp}).  If a file 
##' extension is found, \code{file} is \code{<model-name>}.    
##' 
##' Best practice is to avoid using \code{.} in \code{model} unless
##' you are using \code{model} to point to the model specification 
##' file name. Otherwise, use \code{\link{mread_file}}. 
##' 
##' 
##' @section Model Library:
##' 
##' \code{mrgsolve} comes bundled with several precoded PK, PK/PD, and 
##' other systems models that are accessible via the \code{mread} interface.  
##' 
##' Models available in the library include:
##' 
##' \itemize{
##'   \item PK models: \code{pk1cmt}, \code{pk2cmt}, \code{pk3cmt},
##'                    \code{pk1}, \code{pk2}, \code{popex}, \code{tmdd}
##'   \item PKPD models: \code{irm1}, \code{irm2}, \code{irm3}, \code{irm4},
##'                       \code{emax}, \code{effect}
##'   \item Other models: \code{viral1}, \code{viral2}
##' }
##' 
##' When the library model is accessed, \code{mrgsolve} will compile and load
##' the model as you would for any other model.  It is only necessary to 
##' reference the correct model name and point the \code{project} argument
##' to the \code{mrgsolve} model library location via \code{\link{modlib}}.
##' 
##' For more details, see \code{\link{modlib_pk}}, \code{\link{modlib_pkpd}}, 
##' \code{\link{modlib_tmdd}}, \code{\link{modlib_viral}}, and 
##' \code{\link{modlib_details}} for more information about the state 
##' variables and parameters in each model.
##' 
##' @examples
##'
##' \dontrun{
##' code <- '
##' $PARAM CL = 1, VC = 5
##' $CMT CENT
##' $ODE dxdt_CENT = -(CL/VC)*CENT;
##' '
##'
##' mod <- mcode("ex_mread", code)
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
##' # if the model is in the file mymodel.cpp
##' mod <- mread("mymodel")
##' 
##' # if the model is in the file mymodel.txt
##' mod <- mread(file = "mymodel.txt")
##' 
##' or
##' 
##' mod <- mread_file("mymodel.txt")
##' 
##' 
##' }
##' 
##' @seealso \code{\link{mcode}}, \code{\link{mcode_cache}}
##' 
##' @export
mread <- function(model, project = getwd(), code = NULL, 
                  file = NULL, 
                  udll = TRUE, ignore.stdout=TRUE,
                  raw = FALSE, compile = TRUE, audit = TRUE,
                  quiet = getOption("mrgsolve_mread_quiet",FALSE),
                  check.bounds = FALSE, warn = TRUE, soloc = tempdir(),
                  preclean = FALSE, ...) {
  
  if(charthere(model, "/")) {
    project <- dirname(model)
    model <- basename(model)
  }
  
  quiet <- as.logical(quiet)
  
  warn <- warn & (!quiet)
  
  if(missing(model) & !missing(file)) {
    model <- file  
  }
  
  build <- new_build(file = file, model = model, project = project, 
                     soloc = soloc, code = code, preclean = preclean, 
                     udll = udll)
  
  model <- build$model
  
  ## Read the model spec and parse:
  spec  <- modelparse(readLines(build$modfile,warn=FALSE))
  
  ## Block name aliases
  names(spec) <- gsub("DES", "ODE",  names(spec), fixed=TRUE)
  names(spec) <- gsub("POST", "TABLE", names(spec), fixed=TRUE)
  names(spec) <- gsub("^PK$",  "MAIN", names(spec), fixed=FALSE)
  
  ## Expand partial matches
  index <- pmatch(names(spec),block_list,duplicates.ok=TRUE)
  names(spec) <- ifelse(is.na(index),names(spec),block_list[index])
  
  ## Do a check on what we found in the spec
  check_spec_contents(names(spec),warn=warn,...)
  
  ## Pull out the settings and ENV now
  ## We might be passing parse settings in here ...
  SET <- tolist(spec[["SET"]])
  ENV <- eval_ENV_block(spec[["ENV"]],build$project)
  spec[["SET"]] <- spec[["ENV"]] <-  NULL
  
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
  annot_list_maybe <- nonull.list(mread.env$annot)
  
  # bind_rows in the new version of dplyr (as of 3/28/2017) errors on bind_rows
  # for an empty list() with Error in bind_rows_(x, .id) : not compatible with STRSXP
  # the behavior, based on the bind_rows expectation of a consistent api should
  # be an empty data frame. This is different from do.call(rbind, list()), which returns
  # null. Chose to maintain consistency of annot always being a dataframe
  if (!length(annot_list_maybe)) {
    annot <- dplyr::data_frame()
  } else {
    annot <- dplyr::bind_rows(annot_list_maybe)
  }
  omega <- omat(do.call("c", nonull.list(mread.env$omega)))
  sigma <- smat(do.call("c", nonull.list(mread.env$sigma)))
  namespace <- do.call("c", mread.env$namespace)
  
  # capture is a vector that may be name or to_name = from_name
  # capture will be to_names and it's names are from names 
  capture <- unlist(do.call("c", nonull.list(mread.env$capture)))
  capture <- .ren.create(as.character(capture))
  
  annot <- capture_param(annot,.ren.new(capture))
  
  check_globals_err <- check_globals(mread.env$move_global,names(init))
  if(length(check_globals_err) > 0) {
    stop(check_globals_err, call.=FALSE)
  }
  
  ## Collect potential multiples
  subr  <- collect_subr(spec)
  table <- unlist(spec[names(spec)=="TABLE"], use.names=FALSE)
  plugin <- get_plugins(spec[["PLUGIN"]])
  
  ## Look for compartments we're dosing into: F/ALAG/D/R
  ## and add them to CMTN
  dosing <- dosing_cmts(spec[["MAIN"]], names(init))
  SET[["CMTN"]] <- c(spec[["CMTN"]], dosing)
  
  # This section checks the contents of the spec and makes some special 
  # interventions
  # Virtual compartments
  if(any(is.element("VCMT", names(spec)))) {
    what <- which(names(spec)=="VCMT")
    vcmt <- unique(names(unlist(mread.env$init[what])))
    spec[["ODE"]] <- c(spec[["ODE"]], paste0("dxdt_",vcmt,"=0;"))
  }
  
  if(is.element("Rcpp", names(plugin))) {
    spec <- global_rcpp(spec)
  }
  
  if(is.element("mrgx", names(plugin))) {
    toglob <- wrap_namespace("Rcpp::Environment _env;", NULL)
    topream <- "_env = mrgx::get_envir(self);"
    spec[["PREAMBLE"]] <- c(topream, spec[["PREAMBLE"]])
    spec[["GLOBAL"]] <-  c(toglob, spec[["GLOBAL"]])
  }
  


  ## Constructor for model object:
  x <- new("mrgmod",
           model = model,
           soloc = build$soloc,
           package = build$package,
           project = build$project,
           fixed = fixed,
           advan = subr[["advan"]],
           trans = subr[["trans"]],
           omega = omega,
           sigma = sigma,
           param = as.param(param),
           init = as.init(init),
           funs = funs_create(model),
           capture = .ren.chr(capture),
           envir = ENV, 
           plugin = names(plugin),
           modfile = basename(build$modfile)
  )
  
  x <- store_annot(x,annot)
  
  ## ADVAN 13 is the ODEs
  ## Two compartments for ADVAN 2, 3 compartments for ADVAN 4
  ## Check $MAIN for the proper symbols
  if(x@advan != 13) {
    if(subr[["n"]] != neq(x)) {
      stop("$PKMODEL requires  ", subr[["n"]] , 
           " compartments in $CMT or $INIT.",call.=FALSE)
    }
    check_pred_symbols(x,spec[["MAIN"]])
  }
  
  ## This must come after audit
  if(!has_name("ODE", spec)) {
    spec[["ODE"]] <- "DXDTZERO();"
  } else if(audit) {
    audit_spec(x,spec,warn=warn)
  }
  
  ## First update with what we found in the model specification file
  x <- update(x, data=SET, open=TRUE)
  
  ## Arguments in $SET that will be passed to mrgsim
  simargs <- SET[is.element(names(SET),set_args)]
  if(length(simargs) > 0) {
    x@args <- combine_list(x@args,simargs)
  }
  
  ## Next, update with what the user passed in as arguments
  args <- list(...)
  
  x <- update(x, data=args,open=TRUE)
  
  ## These are the various #define statements
  ## that go at the top of the .cpp.cpp file
  rd <- generate_rdefs(pars = names(param),
                      cmt = names(init),
                      ode_func(x),
                      main_func(x),
                      table_func(x),
                      config_func(x),
                      model = model(x),
                      omats = omat(x),
                      smats = smat(x),
                      set = SET,
                      check.bounds = check.bounds)
  
  ## Write the model code to temporary file
  temp_write <- tempfile()
  def.con <- file(temp_write, open="w")
  cat(
    paste0("// Source MD5: ", build$md5, "\n"),
    plugin_code(plugin),
    ## This should get moved to rd
    "\n// FIXED:",
    fixed_parameters(fixed,SET[["fixed_type"]]),
    "\n// INCLUDES:",
    form_includes(spec[["INCLUDE"]],build$project),
    "\n// NAMESPACES:",
    namespace,
    "\n// BASIC MODELHEADER FILE:",
    "#include \"modelheader.h\"",
    "\n// GLOBAL CODE BLOCK:",
    "// GLOBAL VARS FROM BLOCKS & TYPEDEFS:",
    mread.env[["global"]],
    "\n// GLOBAL START USER CODE:",
    spec[["GLOBAL"]],
    "\n// DEFS:",
    rd, 
    "\n// PREAMBLE CODE BLOCK:",
    "__BEGIN_config__",
    spec[["PREAMBLE"]],
    "__END_config__",
    "\n// MAIN CODE BLOCK:",
    "__BEGIN_main__",
    spec[["MAIN"]],
    advtr(x@advan,x@trans),
    "__END_main__",
    "\n// DIFFERENTIAL EQUATIONS:",
    "__BEGIN_ode__",
    spec[["ODE"]],
    "__END_ode__",
    "\n// TABLE CODE BLOCK:",
    "__BEGIN_table__",
    table,
    write_capture(.ren.old(capture)),
    "__END_table__",
    sep="\n", file=def.con)
  close(def.con)
  
  ## lock some of this down so we can check order later
  x@shlib$cmt <- names(Init(x))
  x@shlib$par <- names(param(x))
  x@code <- readLines(build$modfile, warn=FALSE)
  x@shlib$version <- GLOBALS[["version"]]
  x@shlib$source <- file.path(build$soloc,build$compfile)
  x@shlib$md5 <- build$md5
  x@shlib$covariates <- mread.env$covariates
  
  ## IN soloc directory
  cwd <- getwd()
  setwd(build$soloc)
  
  
  to_restore <- set_up_env(plugin,clink=c(project(x),SET$clink))
  on.exit({
    setwd(cwd)
    do_restore(to_restore)
  })
  ## this gets written in soloc
  #write_build_env(build)
  write_win_def(x)
  #do_restore(to_restore)
  
  same <- check_and_copy(from = temp_write,
                         to = build$compfile)
  
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
                 build$compfile)
  
  ## Windows: always intern; output.on.console if not ignore.stdout
  args <- list(command=syst)
  
  if(build$win) {
    args$intern <- TRUE
    args$show.output.on.console <- !ignore.stdout
  } else {
    args$intern <- ignore.stdout
    args$ignore.stdout <- ignore.stdout
  }
  
  output <- suppressWarnings(do.call(system,args))
  
  status <- attr(output,"status")
  
  if(args$intern) {
    comp_success <- is.null(status) & file.exists(build$compout)
  } else {
    comp_success <- output=="0" & file.exists(build$compout)
  }
  
  if(!comp_success) {
    ## Always on windows
    ## on unix only if ignore.stdout
    if(args$intern) cat(output,sep="\n")
    cat("-------------\n")
    stop("there was a problem building the model.",call.=FALSE)
  } 
  
  if(ignore.stdout) {
    if(!quiet) message("done.")
  }  else {
    if(args$intern) cat(output,sep="\n") 
  }
  
  
  ## Rename the shared object to unique name
  ## e.g model2340239403.so
  z <- file.copy(build$compout,sodll(x))
  
  dyn.load(sodll(x))
  
  stopifnot(all_loaded(x))
  
  x <- compiled(x,TRUE)
  
  return(x)
}

##' @rdname mread 
##' @export
mread_cache <- function(model = NULL, project = getwd(), 
                        file = paste0(model, ".cpp"),
                        code = NULL, soloc = tempdir(), 
                        quiet = FALSE, 
                        preclean = FALSE, ...) {
  
  build <- new_build(file, model, project, soloc, code, preclean) 
  
  cache_file <- file.path(build$soloc, "mrgmod_cache.RDS")
  
  ## If the cache file doesn't exist, build and return
  te <- file.exists(cache_file) & !preclean
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
    return(update(x,...))
  }
  
  x <- mread(build$model, project, soloc=soloc, quiet=quiet, 
             file = basename(build$modfile), ...)
  
  saveRDS(x,file=cache_file)
  
  return(x) 
}

##' @export
##' @rdname mread
mread_file <- function(file, ...) {
  model <- tools::file_path_sans_ext(file)
  mread(model = model, file = file, ...)
}



