# Copyright (C) 2013 - 2024 Metrum Research Group
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

#' @include modspec.R
NULL

#' Read a model specification file
#' 
#' `mread()` reads and parses the mrgsolve model specification file,
#' builds the model, and returns a model object for simulation. 
#' `mread_cache()` does the same, but caches the compilation result for 
#' later use. `mread_file()` can be used for convenience, taking the model 
#' file name as the first argument. 
#' 
#'
#' @param model model name.
#' @param project location of the model specification file an any 
#' headers to be included; see also the discussion about model; this argument
#' can be set via [options()].
#' library under details as well as the [modlib()] help topic.
#' @param file the full file name (with extension, but without path)
#' where the model is specified.
#' @param soloc the directory location where the model shared object is built
#' and stored; see details; this argument can be set via [options()]; 
#' if the directory does not exist, `mread()` will attempt to create it.
#' @param code a character string with model specification code to be 
#' used instead of a model file.
#' @param ignore.stdout passed to system call when compiling the model; set this
#' to `FALSE` to print output to the R console. 
#' @param raw if `TRUE`, return model content as a list, bypassing the compile
#' step; this argument is typically used for debugging problems with the 
#' model build. 
#' @param compile logical; if `TRUE`, the model will be built.
#' @param check.bounds check boundaries of parameter list.
#' @param audit check the model specification file for errors.
#' @param warn logical; if `TRUE`, print warning messages that may arise while
#' building the model.
#' @param udll use unique name for shared object.
#' @param quiet don't print messages from mrgsolve when compiling.
#' @param preclean logical; if `TRUE`, compilation artifacts are 
#' cleaned up first.
#' @param recover if `TRUE`, a list of build will be returned in case
#' the model shared object fails to compile; use this option to and 
#' the returned object to collect information assist in debugging.
#' @param capture a character vector or comma-separated string of additional 
#' model variables to capture; these variables will be added to the capture 
#' list for the current call to `mread()` only.
#' @param ... passed to [mrgsolve::update()]; also arguments passed
#' to `mread()` from `mread_cache()`.
#' 
#' @details
#' The `model` argument is required.  For typical use, 
#' the `file` argument is omitted and the value 
#' for `file` is generated from the value for `model`.
#' To determine the source file name, mrgsolve will look for 
#' a file extension in `model`.  A file extension is 
#' assumed when it finds a period followed by one to three alpha-numeric 
#' characters at the end of the string (e.g. `mymodel.txt` but not 
#' `my.model`).  If no file extension is found, the extension `.cpp` 
#' is assumed (e.g. `file` is `<model-name>.cpp`).  If a file 
#' extension is found, `file` is `<model-name>`.    
#' 
#' Best practice is to avoid using `.` in `model` unless
#' you are using `model` to point to the model specification 
#' file name. Otherwise, use `mread_file()`. 
#' 
#' Use the `soloc` argument to specify a directory location for building
#' the model.  This is the location where the model shared object will be 
#' stored on disk.  The default is a temporary directory, so compilation 
#' artifacts are lost when R restarts when the default is used.  Changing
#' `soloc` to a persistent directory location will preserve those 
#' artifacts across R restarts. Also, if simulation from a single model is 
#' being done in separate processes on separate compute nodes, it might be 
#' necessary to store these compilation artifacts in a local directory 
#' to make them accessible to the different nodes. If the `soloc` 
#' directory does not exist, `mread()` will attempt to create it.
#' 
#' Similarly, using `mread_cache()` will cache results in the temporary 
#' directory and the cache cannot be accessed after the R process is 
#' restarted.
#' 
#' @section Model Library:
#' 
#' mrgsolve comes bundled with several pre-coded PK, PK/PD, and 
#' other systems models that are accessible via the `mread()` interface.  
#' 
#' Models available in the library include:
#' 
#' * PK models: `pk1cmt`, `pk2cmt`, `pk3cmt`, `pk1`, `pk2`, `popex`, `tmdd`
#' * PKPD models: `irm1`, `irm2`, `irm3`, `irm4`, `emax`, `effect`
#' * Other models: `viral1`, `viral2`
#' 
#' When the library model is accessed, mrgsolve will compile and load
#' the model as you would for any other model.  It is only necessary to 
#' reference the correct model name and point the `project` argument
#' to the mrgsolve model library location via [modlib()].
#' 
#' For more details, see [modlib_pk], [modlib_pkpd], [modlib_tmdd], 
#' [modlib_viral], and [modlib_details] for more information about the state 
#' variables and parameters in each model.
#' 
#' @examples
#'
#' \dontrun{
#' code <- '
#' $PARAM CL = 1, VC = 5
#' $CMT CENT
#' $ODE dxdt_CENT = -(CL/VC)*CENT;
#' '
#'
#' mod <- mcode("ex_mread", code)
#' mod
#'
#' mod %>% init(CENT=1000) %>% mrgsim() %>% plot()
#'
#' mod <- mread("irm3", modlib())
#' 
#' # if the model is in the file mymodel.cpp
#' mod <- mread("mymodel")
#' 
#' # if the model is in the file mymodel.txt
#' mod <- mread(file = "mymodel.txt")
#' 
#' or
#' 
#' mod <- mread_file("mymodel.txt")
#' }
#' 
#' @seealso [mcode()], [mcode_cache()]
#' 
#' @md
#' @export
mread <- function(model, project = getOption("mrgsolve.project", getwd()), 
                  code = NULL, file = NULL, 
                  udll = TRUE, ignore.stdout = TRUE,
                  raw = FALSE, compile = TRUE, audit = TRUE,
                  quiet = getOption("mrgsolve_mread_quiet", FALSE),
                  check.bounds = FALSE, warn = TRUE, 
                  soloc = getOption("mrgsolve.soloc", tempdir()),
                  capture = NULL,
                  preclean = FALSE, recover = FALSE, ...) {
  
  if(!identical(basename(model), as.character(model))) {
    project <- dirname(model)
    model <- basename(model)
  }
  
  quiet <- isTRUE(quiet)
  
  warn <- isTRUE(warn)
  
  if(missing(model) & !missing(file)) {
    model <- file  
  }
  
  build <- new_build(
    file = file, model = model, project = project, 
    soloc = soloc, code = code, preclean = preclean, 
    udll = udll, recover = recover
  )
  
  model <- build[["model"]]
  
  ## Read the model spec and parse:
  is_rmd <- grepl("\\.[rR]md$", build[["modfile"]])[1]
  if(is_rmd) {
    spec <- modelparse_rmd(readLines(build[["modfile"]],warn = FALSE))
  } else {
    spec  <- modelparse(readLines(build[["modfile"]], warn = FALSE))
  }
  
  ## Block name aliases
  incoming_block_names <- names(spec)
  names(spec) <- toupper(names(spec))
  
  names(spec) <- sub("^DES$",   "ODE",   names(spec))
  names(spec) <- sub("^POST$",  "TABLE", names(spec))
  names(spec) <- sub("^ERROR$", "TABLE", names(spec))
  names(spec) <- sub("^PK$",    "MAIN",  names(spec))
  
  ## Expand partial matches
  index <- pmatch(names(spec), block_list, duplicates.ok = TRUE)
  names(spec) <- ifelse(is.na(index), names(spec), block_list[index])
  
  ## Do a check on what we found in the spec
  check_spec_contents(names(spec), warn = warn,...)
  
  ## Pull out the settings and ENV now
  ## We might be passing parse settings in here ...
  SET <- handle_SET(spec)
  ENV <- eval_ENV_block(spec[["ENV"]],build[["project"]])
  if("SET" %in% names(spec)) spec[["SET"]] <- ""
  if("ENV" %in% names(spec)) spec[["ENV"]] <- ""
  
  # mread parse env ----
  mread.env <- parse_env(
    spec,
    incoming_block_names,
    build,
    ENV
  )
  
  # Find globals  ----
  # The main sections that need R processing:  
  # NOTE: this must happen prior to handling the various blocks
  # `captured` items are injected into `$CAPTURE`; this can be changed but 
  # for now leave this here
  spec <- move_global2(
    spec, 
    mread.env, 
    build
  )
  
  # Find cpp objects with dot syntax; saved to mread.env$cpp_dot ----
  find_cpp_dot(spec, mread.env)
  
  # Parse blocks ----
  # Each block gets assigned a class to dispatch the handler function
  # Also, we use a position attribute so we know 
  # where we are when we're handling the block
  
  specClass <- paste0("spec", names(spec))
  
  for(i in seq_along(spec)) {
    spec[[i]] <- structure(
      .Data = spec[[i]],
      class = specClass[i],
      pos = i
    )
  }
  
  # Call the handler for each block
  spec <- lapply(spec, handle_spec_block, env = mread.env)
  
  # collect -----
  # TODO: move this to the plugin handler
  plugin <- get_plugins(spec[["PLUGIN"]], mread.env)
  param <- as.list(do.call("c",unname(mread.env[["param"]])))
  fixed <- as.list(do.call("c",unname(mread.env[["fixed"]])))
  init <-  as.list(do.call("c",unname(mread.env[["init"]])))
  ode <- do.call("c", unname(mread.env[["ode"]]))
  namespace <- do.call("c", mread.env[["namespace"]])
  omega <- omat(do.call("c", nonull.list(mread.env[["omega"]])))
  sigma <- smat(do.call("c", nonull.list(mread.env[["sigma"]])))
  if(isTRUE(SET[["collapse_omega"]])) {
    omega <- collapse_matrix(omega)  
  }
  if(isTRUE(SET[["collapse_sigma"]])) {
    sigma <- collapse_matrix(sigma)  
  }
  annot_list_maybe <- nonull.list(mread.env$annot)
  if (!length(annot_list_maybe)) {
    annot <- tibble()
  } else {
    annot <- bind_rows(annot_list_maybe)
  }
  
  # capture  ----
  capture_more <- capture
  capture <- unlist(nonull.list(mread.env[["capture"]]))
  capture <- .ren.create(capture)
  capture <- .ren.sanitize(capture)
  annot <- capture_param(annot, .ren.new(capture))
  
  # Collect potential multiples
  subr  <- collect_subr(spec)
  table <- unlist(spec[names(spec)=="TABLE"], use.names = FALSE)
  if("ODE" %in% names(spec)) {
    spec[["ODE"]] <- unlist(spec[names(spec)=="ODE"], use.names = FALSE)
  }
  if("PLUGIN" %in% names(spec)) {
    spec[["PLUGIN"]] <- unlist(spec[names(spec)=="PLUGIN"], use.names = FALSE)
  }
  
  # TODO: deprecate audit argument
  mread.env[["audit_dadt"]] <- 
    isTRUE(audit) && isTRUE(mread.env[["audit_dadt"]])
  
  # new model object ----
  x <- new(
    "mrgmod",
    model = model,
    soloc = build[["soloc"]],
    package = build[["package"]],
    project = build[["project"]],
    fixed = fixed,
    advan = subr[["advan"]],
    trans = subr[["trans"]],
    omega = omega,
    sigma = sigma,
    param = as.param(param),
    init = as.init(init),
    funs = funs_create(model),
    envir = ENV, 
    capture = .ren.chr(capture),
    plugin = names(plugin),
    modfile = basename(build[["modfile"]])
  )
  
  # updates -----
  # First update with what we found in the model specification file
  x <- update(x, data = SET, open = TRUE, strict = FALSE)
  
  # Next, update with what the user passed in as arguments
  args <- list(...)
  x <- update(x, data = args, open = TRUE, strict = FALSE)
  x <- store_annot(x, annot)
  
  # Arguments in $SET that will be passed to mrgsim
  x <- set_simargs(x, SET)

  # Modify SS compartments
  x <- set_ss_cmt(x, SET[["ss_cmt"]])

  # Look for compartments we're dosing into: F/ALAG/D/R and add them to CMTN
  dosing <- dosing_cmts(spec[["MAIN"]], names(init))
  CMTN <- c(spec[["CMTN"]], dosing)
  
  # model r defs ----
  # These are the various #define statements
  # that go at the top of the .cpp.cpp file
  rd <- generate_rdefs(
    pars = Pars(x),
    cmt = Cmt(x),
    func = ode_func(x),
    init_fun = main_func(x),
    table_fun = table_func(x),
    event_fun = event_func(x),
    config_fun = config_func(x),
    model = model(x),
    omats = omat(x),
    smats = smat(x),
    cmtn = CMTN,
    check.bounds = check.bounds, 
    plugin = plugin,
    dbsyms = args[["dbsyms"]]
  )
  
  # Handle plugins ----
  # Virtual compartments
  if("VCMT" %in% names(spec)) {
    what <- which(names(spec)=="VCMT")
    vcmt <- unique(names(unlist(mread.env[["init"]][what])))
    spec[["ODE"]] <- c(spec[["ODE"]], paste0("dxdt_", vcmt, "=0;"))
  }
  
  # Handle nm-vars plugin; initializing `nmv` to `NULL` here for use in 
  # the audit check later on
  nmv <- NULL
  if("nm-vars" %in% names(plugin)) {
    nmv  <- find_nm_vars(spec)
    dfs <- generate_nmdefs(nmv)
    rd <- c(rd, dfs)
    plugin[["nm-vars"]][["nm-def"]] <- dfs
    build[["nm-vars"]] <- nmv
    audit_nm_vars(
      spec,
      x,
      build = build, 
      nmv = nmv, 
      env = mread.env
    )
  }
  # autodec
  if("autodec" %in% names(plugin)) {
    auto_blocks <- c("PREAMBLE", "MAIN", "PRED", "ODE", "TABLE", "EVENT")
    auto_skip <- cvec_cs(ENV[["MRGSOLVE_AUTODEC_SKIP"]])
    autov <- autodec_vars(spec, blocks = auto_blocks)
    autov <- autodec_clean(
      autov, 
      rdefs = rd, 
      build = build, 
      skip = auto_skip
    )
    autodec_nm_vars(autov, mread.env)
    autodec_save(autov, build, mread.env)
    mread.env[["autodec"]] <- autodec_namespace(build, mread.env)
  }
  
  # Rcpp
  if("Rcpp" %in% names(plugin)) {
    spec <- global_rcpp(spec)
  }
  # mrgx  
  if("mrgx" %in% names(plugin)) {
    toglob <- wrap_namespace("Rcpp::Environment _env;", NULL)
    topream <- "_env = mrgx::get_envir(self);"
    spec[["PREAMBLE"]] <- c(topream, spec[["PREAMBLE"]])
    spec[["GLOBAL"]] <-   c(toglob, spec[["GLOBAL"]])
  }
  
  # more captures ----
  
  # Process @etas 1:n -----
  x <- capture_etas(x, mread.env)
  
  # Process capture passed into mread -----
  if(is.character(capture_more)) {
    valid_capture <- get_valid_capture(
      param = param, omega = omega, sigma = sigma, build = build, 
      mread.env = mread.env
    )
    if(identical(capture_more, "(everything)")) {
      capture_more <- valid_capture[valid_capture != "."]  
    }
    capture_vars <- .ren.create(capture_more)
    capture_vars <- .ren.sanitize(capture_vars)
    if(!all(capture_vars[["old"]] %in% valid_capture)) {
      bad <- setdiff(capture_vars[["old"]], valid_capture)
      for(b in bad) {
        msg <- glue(" - item `{b}` does not exist in model `{build$model}`")
        message(msg)
      }
      stop(
        "all requested `capture` variables must exist in the model",
        call. = FALSE
      )
    }
    x <- update_capture(x, .ren.chr(capture_vars))
    build$preclean <- TRUE
  }
  
  # Check mod ----
  check_pkmodel(x, subr, spec)
  check_globals(mread.env[["move_global"]], Cmt(x))
  check_cpp_dot(mread.env, x)
  check_sim_eta_eps_n(x, spec)
  
  # This must come after nm-vars is processed; nmv is NULL if 
  # not using nm-vars
  # TODO: harmonize with other audit process
  if(!has_name("ODE", spec)) {
    spec[["ODE"]] <- "DXDTZERO();"
  } else {
    spec[["ODE"]] <- c(spec[["ODE"]], ode)
    audit_spec(x, spec, nmv, mread.env, warn = warn)
  }
  
  # set up shlib ----
  # lock some of this down so we can check order later
  x@code <- readLines(build[["modfile"]], warn=FALSE)
  x@shlib[["covariates"]] <- mread.env[["covariates"]]
  x@shlib[["param_tag"]] <- mread.env[["param_tag"]]
  x@shlib[["cpp_variables"]] <- build$cpp_variables
  inc <- spec[["INCLUDE"]]
  if(is.null(inc)) inc <- character(0)
  x@shlib[["include"]] <- inc
  x@shlib[["nm_import"]] <- mread.env[["nm_import"]]
  x@shlib[["source"]] <- file.path(build[["soloc"]],build[["compfile"]])
  x@shlib[["md5"]] <- build[["md5"]]
  x@shlib[["call_event"]] <- "EVENT" %in% names(spec)
  
  # build----
  # In soloc directory
  cwd <- getwd()
  setwd(build[["soloc"]])
  to_restore <- set_up_env(
    x = plugin,
    clink = c(project(x), SET[["clink"]]), 
    CXX = ENV[["PKG_CXXFLAGS"]]
  )
  on.exit({
    setwd(cwd)
    do_restore(to_restore)
  })
  
  incl <- function(x) paste0('#include "', x, '"')
  header_file <- paste0(build[["model"]], "-mread-header.h")
  
  dbs <- NULL
  if(isTRUE(args[["dbsyms"]])) {
    dbs <- debug_symbols(names(init(x)))
  }
  
  cat(
    paste0("// Source MD5: ", build[["md5"]]),
    "\n// PLUGINS:",
    plugin_code(plugin),
    ## This should get moved to rd
    "\n// FIXED:",
    fixed_parameters(fixed, SET[["fixed_type"]]),
    "\n// NAMESPACES:",
    namespace,
    "\n// MODEL HEADER FILES:",
    incl("mrgsolv.h"), 
    incl("modelheader.h"),
    "\n// INCLUDE databox functions:",
    incl("databox_cpp.h"),
    "\n// USING plugins:",
    plugin_using(plugin),
    "\n// INCLUDES:",
    form_includes(spec[["INCLUDE"]]),
    "\n// GLOBAL CODE BLOCK:",
    "// GLOBAL VARS FROM BLOCKS & TYPEDEFS:",
    "// DECLARED BY USER", 
    mread.env[["global"]],
    "// DECLARED VIA AUTODEC",
    mread.env[["autodec"]],
    "\n// GLOBAL START USER CODE:",
    spec[["GLOBAL"]],
    "\n// DEFS:",
    rd,
    "",
    sep="\n", file = header_file)
  
  ## Write the model code to temporary file
  temp_write <- tempfile()
  def.con <- file(temp_write, open="w")
  cat(
    paste0("// Source MD5: ", build[["md5"]], "\n"),
    incl(header_file),
    "\n// PREAMBLE CODE BLOCK:",
    "__BEGIN_config__",
    spec[["PREAMBLE"]],
    "__END_config__",
    "\n// MAIN CODE BLOCK:",
    "__BEGIN_main__",
    dbs[["cmt"]],
    spec[["MAIN"]],
    advtr(x@advan,x@trans),
    "__END_main__",
    "\n// DIFFERENTIAL EQUATIONS:",
    "__BEGIN_ode__",
    dbs[["ode"]],
    spec[["ODE"]], 
    "__END_ode__",
    "\n// MODELED EVENTS:", 
    "__BEGIN_event__", 
    spec[["EVENT"]],
    "__END_event__",
    "\n// TABLE CODE BLOCK:",
    "__BEGIN_table__",
    dbs[["cmt"]],
    table,
    spec[["PRED"]],
    write_capture(names(x@capture)),
    "__END_table__",
    "", 
    sep="\n", file=def.con)
  close(def.con)
  
  ## this gets written in soloc
  #write_build_env(build)
  write_win_def(x)
  
  same <- check_and_copy(
    from = temp_write,
    to = build[["compfile"]]
  )
  
  if(!compile) return(x)
  
  if(ignore.stdout & !quiet) {
    message("Building ", model(x), " ... ", appendLF = FALSE)
  }
  
  # Wait at least 2 sec since last compile
  safe_wait(x)
  cleanso(x)
  
  # Compile the model
  # The shared object is model-mread-source.cpp
  
  out <- suppressWarnings(build_exec(build))
  
  comp_success <- out[["status"]]==0 & file.exists(build[["compout"]])
  
  if(!comp_success) {
    if(ignore.stdout) message("error.\n", appendLF = FALSE)
    return(build_failed(out, build, x, spec, ignore.stdout))
  } 
  
  if(ignore.stdout) {
    if(!quiet) message("done.\n", appendLF = FALSE)
  }  else {
    out <- build_output_cleanup(out, build) 
    cat(out[["stdout"]], sep = "\n")
  }
  
  # Rename the shared object to unique name
  # e.g model2340239403.so
  z <- file.copy(build[["compout"]], sodll(x))
  
  dyn.load(sodll(x))
  
  stopifnot(all_loaded(x))
  
  x <- compiled(x,TRUE)
  
  return(x)
}

#' @rdname mread
#' @export
mread_cache <- function(model = NULL, 
                        project = getOption("mrgsolve.project", getwd()), 
                        file = paste0(model, ".cpp"),
                        code = NULL, 
                        soloc = getOption("mrgsolve.soloc", tempdir()), 
                        quiet = FALSE, 
                        preclean = FALSE, 
                        capture = NULL, ...) {
  
  if (!identical(basename(model), as.character(model))) {
    project <- dirname(model)
    model <- basename(model)
  }
  
  if(is.character(capture)) preclean <- TRUE
  
  build <- new_build(file, model, project, soloc, code, preclean) 
  
  cache_file <- file.path(build$soloc, "mrgmod_cache.RDS")
  
  ## If the cache file doesn't exist, build and return
  te <- file.exists(cache_file) & !preclean
  t0 <- t1 <- t2 <- t3 <- t4 <- FALSE
  
  if(te) {
    x <- readRDS(cache_file)
    t0 <- is.mrgmod(x)
    if(t1 <- is.character(x@shlib$md5)) {
      t2 <- x@shlib$md5 == build$md5
    }
    t3 <- file.exists(sodll(x))
    t4 <- length(x@shlib[["include"]])==0
  }
  
  if(all(t0,t1,t2,t3,t4,te)) {
    if(!quiet) message("Loading model from cache.")
    loadso(x)
    return(update(x,...,strict=FALSE))
  }
  
  x <- mread(
    build$model, project, soloc=soloc, quiet=quiet, 
    file = basename(build$modfile), capture = capture, ...
  )
  
  saveRDS(x,file=cache_file)
  
  return(x) 
}

#' @export
#' @rdname mread
mread_file <- function(file, ...) {
  model <- tools::file_path_sans_ext(file)
  mread(model = model, file = file, ...)
}
