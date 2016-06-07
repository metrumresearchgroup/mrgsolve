## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

##' @include utils.R complog.R nmxml.R matrix.R

## TO BE REMOVED 4/29/16
##c_com_start <- "/\\*"
##c_com_end <- "\\*/"
##GlobalVarRe <- "\\s*Global\\s+(double|int|bool|).*"
##eol.comment <- "^([^#]*)\\#+.*$"; dhash <- "^([^#]*)\\##+.*$"; dslash <- "^(.*)//.*$"
## labre <- "\\s*\\$([A-Z,a-z,0-9]+)\\s*.*"
## labre.rep <- "\\1"
## drop.labre <- "\\$[A-Z,a-z,0-9]+\\s*(.*)"
## drop.labre.rep <- "\\1"

globalre2 <- "^\\s*(predpk|double|bool|int)\\s+\\w+"
block_re <-  "^\\s*(\\$([A-Z]\\w*)|\\[\\s*([A-Z]\\w*)\\s*])(.*)"

## Generate an advan/trans directive
advtr <- function(advan,trans) {
  if(advan==13 | trans==1) return("")
  if((advan %in% c(1,2)) & !(trans %in% c(2,11))) {
    stop("ADVAN 1 and 2 can only use trans 1, 2, or 11", call.=FALSE)
  }
  if((advan %in% c(3,4)) & !(trans %in% c(4,11))) {
    stop("ADVAN 3 and 4 can only use trans 1, 4, or 11", call.=FALSE)
  }
  return(paste0("__ADVAN", advan, "_TRANS", trans, "__"))
}


## A random file name
rfile <- function(pattern="",tmpdir=normalizePath(getwd(),winslash="/")){
  basename(tempfile(pattern=pattern,tmpdir='.'))
}

## These are arguments to mrgsim that
## can be stated in $SET and then passed to mrgsim
set_args <- c("Req", "obsonly","mtime", "recsort",
              "carry.out","Trequest","trequest")

## REMOVE 5/18/2016
is_loaded <- function(x) is.loaded(x[1],x[2],type="Call")
funs_loaded <- function(x) sapply(list(ode=x@func,main=x@init_fun,table=x@table_fun),is_loaded)

check_spec_contents <- function(x,crump=TRUE,warn=TRUE,...) {
  invalid <- setdiff(x,block_list)
  valid <- intersect(x,block_list)
  
  if(sum(is.element("TABLE",x)) > 1) stop("Only one $TABLE block allowed.",call.=FALSE)
  if(sum(is.element("MAIN",x))  > 1) stop("Only one $MAIN block allowed.",call.=FALSE)
  if(sum(is.element("ODE",x))   > 1) stop("Only one $ODE block allowed.",call.=FALSE)
  if(sum(is.element("SET",x))   > 1) stop("Only one $SET block allowed.",call.=FALSE)
  if(sum(is.element("MAIN",x))  > 1) stop("Only one $MAIN block allowed.",call.=FALSE)
  
  if(warn) {
    if(sum(is.element(c("INIT", "CMT"),x)) == 0)  warning("Could not find a $INIT or $CMT block", call.=FALSE)
    if(length(invalid)>0) {
      warning(paste0("Invalid blocks found: ", paste(invalid, collapse=" ")), call.=FALSE)
    }
  }
  if(length(valid)==0) stop("No valid blocks found.", call.=FALSE)
}

audit_spec <- function(x,spec,warn=TRUE) {
  
  cmt <- names(init(x))
  
  if(warn) {
    if(exists("ODE", spec)) {
      eq <- paste0("dxdt_",cmt)
      z <- rep(FALSE, length(cmt))
      for(i in seq_along(cmt)) z[i] <- !any(grepl(eq[i],spec$ODE, perl=TRUE)  )
      if(any(z)) {
        ans <- paste(cmt[z], collapse=',')
        warning(paste0("Audit: missing differential equation(s) for ", ans), call.=FALSE)
      }
    }
  }
  
  return(invisible(NULL))
}


define_digits <- function(x) {
  x <- as.character(x)
  fix <- grep("[.+-]", x, invert=TRUE)
  x[fix] <- paste0(x[fix], '.0')
  x
}

fixed_parameters <- function(x,fixed_type) {
  if(length(x)==0) return("")
  if(is.null(fixed_type))  fixed_type <-  "define"
  if(!(fixed_type %in% c("define", "const"))) stop("fixed_type must be either const or define.", call.=FALSE)
  switch(fixed_type,
         `const` =  paste0("const double ", paste0(names(x) ,"= " ,unlist(x), ";")),
         `define` = paste0("#define ", names(x), "  (", define_digits(unlist(x)),")")
  )
}

## Form a file name / path for the file that is actually compiled
compfile <- function(x,project) file.path(project,paste0(x, ".cpp.cpp"))

##' Parse model specification text.
##' @param txt model specification text
##' @param split logical
##' @param ... arguments passed along
##' @export
modelparse <- function(txt,split=FALSE,...) {
  
  ## Take in model text and parse it out
  
  if(split) txt <- strsplit(txt,"\n",perl=TRUE)[[1]]
  
  txt <- strsplit(txt, "//+|##+",perl=TRUE)
  txt <- sapply(txt, `[`,1L)
  txt <- txt[!is.na(txt) & !grepl("^\\s*$",txt,perl=TRUE)]
  
  ##txt <- txt[!(is.na(txt) | grepl("^\\s*$",txt))]
  
  start <- grep(block_re,txt,perl=TRUE)
  
  if(length(start)==0) stop("No model specification file blocks were found.", call.=FALSE)
  
  labs <- gsub(block_re,"\\2\\3", txt[start],perl=TRUE)
  
  txt[start] <- gsub(block_re, "\\4", txt[start],perl=TRUE)
  
  end <- c((start-1),length(txt))[-1]
  
  spec <- lapply(seq_along(start), function(i) {
    y <- txt[start[i]:end[i]]
    y[y!=""]
  })
  
  names(spec) <- labs
  
  return(spec)
  
}


## ----------------------------------------------------------------------------
## New function set for finding double / bool / int
## and moving to global
move_global_re_find <- "\\b(double|int|bool)\\s+\\w+\\s*="
move_global_re_sub <- "\\b(double|bool|int)\\s+(\\w+\\s*=)"
local_var_typedef <- c("typedef double localdouble;","typedef int localint;","typedef bool localbool;")
move_global <- function(x,what=c("MAIN", "ODE", "TABLE")) {
  what <- intersect(what,names(x))
  if(length(what)==0) return(x)
  l <- lapply(x[what], get_c_vars) %>% unlist
  x[["GLOBAL"]] <- c(x[["GLOBAL"]],l,local_var_typedef)
  for(w in what) {
    x[[w]] <- gsub(move_global_re_sub,"\\2",
                   x[[w]],perl=TRUE)
  }
  return(x)
}
get_c_vars <- function(y) {
  m <- gregexpr(move_global_re_find,y,perl=TRUE)
  regmatches(y,m) %>%
    unlist %>%
    gsub(pattern="\\s*=$",
         replacement=";",
         perl=TRUE)
}
## ----------------------------------------------------------------------------


## Replaced by move_global
altglobal <- function(code,moveto="GLOBAL",
                      what=grepl("MAIN|ODE|TABLE",names(code),perl=TRUE)) {
  
  check <- grep("^\\s*(bool|int|double)", unlist(code[what]), value=TRUE,perl=TRUE)
  
  check <- check[grepl("=",check)]
  
  if(any(sapply(strsplit(gsub("[><!=]=", " ",check), "=",perl=TRUE),length)>2)) {
    
    warning("Multiple variable declarations are not allowed in MAIN, ODE, or TABLE.")
  }
  
  vars <- regmatches(check,regexpr(globalre2,check,perl=TRUE))
  
  code[what] <- lapply(code[what], gsub, pattern="^\\s*(double|int|bool)\\s+(\\w+\\s*=)", replacement="\\2", perl=TRUE)
  
  vars <- unlist(vars)
  
  if(length(vars)>0) vars <- paste0(vars, ";")
  
  code[[moveto]] <- c(code[[moveto]], "typedef double localdouble;","typedef int localint;","typedef bool localbool;",vars)
  
  return(code)
}


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
##' $ADVAN2
##' $MAIN
##' pred_CL = 1;
##' pred_VC= 20;
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
##' @examples
##'
##'
##' code <- '
##' $PARAM CL = 1, VC = 5
##' $CMT CENT
##' $ODE dxdt_CENT = -(CL/VC)*CENT;
##' '
##'
##' mod <- mread(code=code)
##'
##' mod
##'
##' mod %>% init(CENT=1000) %>% mrgsim %>% plot
##'
mread <- function(model=character(0),project=getwd(),code=NULL,udll=TRUE,
                  ignore.stdout=TRUE,raw=FALSE,compile=TRUE,audit=FALSE,
                  quiet=getOption("mrgsolve_mread_quiet",FALSE),
                  check.bounds=FALSE,warn=TRUE,soloc=tempdir(),preclean=FALSE,...) {
  
  quiet <- as.logical(quiet)
  
  warn <- warn & (!quiet)
  
  ## Both project and soloc get normalized
  project <- normalizePath(project, mustWork=TRUE, winslash="/")
  
  soloc <- normalizePath(soloc ,mustWork=TRUE, winslash="/")
  
  if(!missing(code) & missing(model)) model <- "_mrgsolve_temp"
  
  ## Check for spaces in the model name
  if(grepl(" +", model,perl=TRUE)) stop("model name cannot contain spaces.")
  
  ## The model file is <stem>.cpp in the <project> directory
  modfile <- file.path(project,paste0(model, ".cpp"))
  
  ## If code is passed in as character:
  if(!missing(code)) {
    mod.con <- file(modfile, open="w")
    cat(code, "\n", file=mod.con)
    close(mod.con)
  }
  
  if(!file.exists(modfile)) {
    stop(paste0("Could not find model file ", modfile), call.=FALSE)
  }
  
  ## If we need a unique dll name, use rfile otherwise model
  ## This is also the "package"
  package <- ifelse(udll,rfile(model),model)
  
  ## Where to write the temp file
  ## This is <package>.cpp.cpp
  package_write <- compfile(package,project)
  
  if(audit) warn <- TRUE
  
  ## Copy the main model header into project:
  modelheaders <- file.path(system.file(package="mrgsolve"), "include", c("mrgsolv.h","modelheader.h"))
  file.copy(modelheaders,project, overwrite=TRUE)
  
  ## Read the model spec and parse:
  spec  <- modelparse(readLines(modfile,warn=FALSE))
  
  ## Block name aliases
  names(spec) <- gsub("DES", "ODE",  names(spec), fixed=TRUE)
  names(spec) <- gsub("^PK$",  "MAIN", names(spec), fixed=FALSE)
  
  ## Expand partial matches
  index <- pmatch(names(spec),block_list,duplicates.ok=TRUE)
  names(spec) <- ifelse(is.na(index),names(spec),block_list[index])
  
  ## Do a check on what we found in the spec
  check_spec_contents(names(spec),warn=warn,...)
  
  ## The main sections that need R processing:
  spec <- move_global(spec)
  
  ## Parse blocks
  specClass <- paste0("spec", names(spec))
  for(i in seq_along(spec)) class(spec[[i]]) <- specClass[i]
  
  ## Call the handler for each block
  spec <- lapply(spec,handle_spec_block)
  
  ## Collect potential multiples
  subr  <- collect_subr(spec)
  omega <- collect_omat(spec)
  sigma <- collect_smat(spec)
  param <- collect_param(spec)
  fixed <- collect_fixed(spec)
  table <- collect_table(spec)
  init  <- collect_init(spec)
  do_plugin <- length(spec[["PLUGIN"]]) > 0
  
  SET <- as.list(spec[["SET"]])
  
  ENV <- spec[["ENV"]]
  
  ## Look for compartments we're dosing into: F/ALAG/D/R
  ## and add them to CMTN
  dosing <- dosing_cmts(spec[["MAIN"]], names(init))
  SET[["CMTN"]] <- c(spec[["CMTN"]],dosing)
  
  ## Virtual compartments
  if(any(is.element("VCMT", names(spec)))) {
    vcmt <- names(collect_init(spec,"VCMT"))
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
           omega=omega,sigma=sigma,
           param=as.param(param),
           init=as.init(init))
  
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
  if(is.null(spec[["ODE"]])) spec[["ODE"]] <- "DXDTZERO();\n"
  
  ## These are the symbols:
  x <- assign_symbols(x)
  
  ## These are the various #define statements
  ## that go at the top of the .cpp.cpp file
  rd <-generate_rdefs(pars=names(param),
                      cmt=names(init),
                      ode_symbol(x),
                      main_symbol(x),
                      table_symbol(x),
                      model=model(x),
                      omat(x),
                      smat(x),
                      parsedata=SET,
                      check.bounds=check.bounds)
  
  ## Write the .cpp.cpp file
  def.con <- file(package_write, open="w")
  cat(
    plugin_code(spec[["PLUGIN"]]),
    "#include \"modelheader.h\"",
    rd,
    ## This should get moved to rd
    fixed_parameters(fixed,SET[["fixed_type"]]),
    "\n// GLOBAL VARIABLES:\n",
    spec[["GLOBAL"]],
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
    "END_table",
    sep="\n", file=def.con)
  close(def.con)
  
  ## lock some of this down so we can check order later
  x@shlib$cmt <- cmt(x)
  x@shlib$par <- pars(x)
  x@code <- readLines(modfile, warn=FALSE)
  x@shlib$date <- shdate(ntime())
  
  if(!compile) return(x)
  
  if(do_plugin) {
    set_clink(spec[["PLUGIN"]])
    on.exit(Sys.unsetenv("CLINK_CPPFLAGS"))
  }
  
  ## This name is suitable for use in the build path
  cfile <- compfile(model,build_path(project))
  
  if(ignore.stdout & !quiet) message("Compiling ",basename(cfile)," ... ", appendLF=FALSE)
  
  preclean <- preclean | (!logged(model(x)))
  
  clean <- ifelse(preclean, " --preclean ", "")
  
  # Wait at least 2 sec since last compile
  safe_wait(x)
  
  ## The temp file is copied to the actual file compile file
  check_and_copy(x,package,preclean)
  
  purge_model(cfile(x))
  
  ## Compile the model
  status <- system(paste0("R CMD SHLIB ",
                          ifelse(preclean, " --preclean ", ""),
                          build_path(cfile),
                          " -o ",
                          sodll(x,short=TRUE)),
                   ignore.stdout=ignore.stdout)
  
  
  file.remove(compfile(package,project(x)))
  
  if(status!=0) {
    warning("Compile did not succeed.  Returning NULL.", immediate.=TRUE,call.=FALSE);
    return(NULL)
  }
  
  if(ignore.stdout & !quiet) message("done.")
  
  store(x)
  
  dyn.load(sodll(x))
  
  if(!dll_loaded(x)) stop("Model was not found after attempted loading.")
  
  x <- compiled(x,TRUE)
  
  return(x)
}


## These only really work in code blocks
opts_only <- function(x,def=list(),all=FALSE) {
  opts <- scrape_opts(x)
  merge(def,opts, strict=!all,warn=FALSE,context="opts")
}
scrape_opts <- function(x,def=list(),all=FALSE) {
  x <- unlist(strsplit(x, "\n",perl=TRUE))
  opts <- grepl("=",x,perl=TRUE)
  data <- unlist(strsplit(x[!opts],"\\s+",perl=TRUE))
  opts <- merge(def, tolist(x[opts]),strict=!all,warn=FALSE,context="opts")
  c(list(x=data), opts)
}
scrape_and_pass <- function(x,pass,...) {
  o <- scrape_opts(x,...)
  ret <- do.call(pass,o)
  list(opts=o,data=ret)
}

## Functions for handling code blocks
parseNMXML <- function(x) {
  x <- tolist(x)
  xml <- do.call(nmxml,x)
  return(xml)
}

parseTHETA <- function(x) {
  opts <- scrape_opts(x,all=TRUE)
  x <- as.numeric(opts$x)
  x <- x[!is.na(x)]
  if(!exists("name", opts)) opts$name <- "THETA"
  names(x) <- paste0(opts$name, 1:length(x))
  as.param(x)
}
parsePARAM <- function(x) as.param(tolist(x))

parseFIXED <- function(x) tolist(x)

parseINIT <- function(x) tolist(x)

parseCMT <- function(x) {
  x <- tovec(x)
  y <- rep(0,length(x))
  names(y) <- x
  y
}

parseCMTN <- function(x) as.cvec(x)

## Used to parse OMEGA and SIGMA matrix blocks
specMATRIX <- function(x,class) {
  
  if(length(x)==0) stop("No data found in matrix block.")
  
  ret <- scrape_and_pass(x,"modMATRIX",def=list(name="...",prefix=""), all=TRUE)
  
  if(is.null(ret[["opts"]][["labels"]])) {
    
    ret[["opts"]][["labels"]] <- rep(".", nrow(ret[["data"]]))
    
  } else {
    ret[["opts"]][["labels"]] <- paste0(ret[["opts"]][["prefix"]],ret[["opts"]][["labels"]])
  }
  
  structure(list(data=  ret[["data"]],
                 labels=ret[["opts"]][["labels"]],
                 name=  ret[["opts"]][["name"]]),class=class)
}

specOMEGA <- function(x,y) specMATRIX(x,"omega_block")

specSIGMA <- function(x,y) specMATRIX(x,"sigma_block")

parseCAPTURE <- function(x) {
  x <- as.cvec(x)
  if(length(x)==0) return(NULL)
  paste0("capture(",as.cvec(x),");")
}


## S3 methods for processing code blocks
## All of these need to be exported
handle_spec_block <- function(x) UseMethod("handle_spec_block")
##' @export
handle_spec_block.default <- function(x) return(x)
##' @export
handle_spec_block.specPARAM <- function(x) parsePARAM(x)
##' @export
handle_spec_block.specINIT <- function(x) parseINIT(x)
##' @export
handle_spec_block.specCMT <- function(x) parseCMT(x)
##' @export
handle_spec_block.specSET <- function(x) tolist(x)
##' @export
handle_spec_block.specENV <- function(x) tolist(x)
##' @export
handle_spec_block.specOMEGA <- function(x) specOMEGA(x)
##' @export
handle_spec_block.specSIGMA <- function(x) specSIGMA(x)
##' @export
handle_spec_block.specNMXML <- function(x) parseNMXML(x)
##' @export
handle_spec_block.specTHETA <- function(x) parseTHETA(x)
##' @export
handle_spec_block.specCMTN <- function(x) parseCMTN(x)
##' @export
handle_spec_block.specFIXED <- function(x) parseFIXED(x)
##' @export
handle_spec_block.specCAPTURE <- function(x) parseCAPTURE(x)
##' @export
handle_spec_block.specVCMT <- function(x) parseCMT(x)
##' @export
handle_spec_block.specPKMODEL <- function(x) {
  x <- scrape_opts(x,all=TRUE)
  do.call("PKMODEL",x)
}


##' @export
handle_spec_block.specPLUGIN <- function(x) {
  x <- unique(as.cvec(x))
  if("mrgx" %in% x) {
    warning("There are currently no functions provided by the mrgx plugin. All functions previously provided by mrgx can be called from the R namespace (e.g. R::rnorm(10,2)).", call.=FALSE)
  }
  if(length(x) ==0) return(list())
  return(get_plugins(x))
}




##' Parse data from \code{$PKMODEL}
##'
##' @param ncmt number of compartments; must be 1 (one-compartment, not including a depot dosing compartment) or 2 (two-compartment model, not including a depot dosing compartment)
##' @param depot logical indicating whether to add depot compartment
##' @param trans the parameterization for the PK model; must be 1, 2, 4, or 11
##' @param ... not used
##'
##' @details
##' When using \code{$PKMODEL}, certain symbols must be defined in the model specification depending
##' on the value of \code{ncmt}, \code{depot} and \code{trans}.
##'
##' \itemize{
##' \item \code{ncmt} 1, \code{depot FALSE}, trans 2: \code{CL}, \code{V}
##' \item \code{ncmt} 1, \code{depot TRUE} , trans 2: \code{CL}, \code{V}, \code{KA}
##' \item \code{ncmt} 2, \code{depot FALSE}, trans 4: \code{CL}, \code{V2}, \code{Q}, \code{V3}
##' \item \code{ncmt} 2, \code{depot TRUE} , trans 4: \code{CL}, \code{V2}, \code{Q}, \code{V3}, \code{KA}
##'
##' }
##'
##' If \code{trans=11} is specfied, use the symbols listed above for the \code{ncmt} / \code{depot} combination, but append \code{i} at the end (e.g. \code{CLi} or \code{Qi} or \code{KAi}).
##'
##' If \code{trans=1}, the user must utilize the following symbols:
##'
##' \itemize{
##' \item \code{pred_CL} for clearance
##' \item \code{pred_V}  or \code{pred_V2} for central compartment volume of distribution
##' \item \code{pred_Q}  for intercompartmental clearance
##' \item \code{pred_V3} for for peripheral compartment volume of distribution
##' \item \code{pred_KA} for absorption rate constant
##'
##' }
##'
##'
PKMODEL <- function(ncmt=1, depot=FALSE, trans = pick_trans(ncmt,depot), ...) {
  stopifnot(ncmt %in% c(1,2))
  advan <- pick_advan(ncmt,depot)
  return(list(advan=advan, trans=trans, n=ncmt))
}


## Used to collect OMEGA and SIGMA matrices
collect_matrix <- function(x,what,class,xmlname) {
  what <- c(what, "NMXMLDATA")
  
  x <- x[sapply(x,inherits,what)]
  
  xmli <- unlist(sapply(x,inherits, "NMXMLDATA"))
  
  x[xmli] <- unname(lapply(x[xmli], function(x) x[[xmlname]]))
  
  if(length(x)==0) return(create_matlist(class=class))
  
  nr <- sapply(x,function(y) nrow(y[["data"]]))
  x <- x[nr > 0]
  
  d <- lapply(x,function(y) y[["data"]])
  l <- lapply(x,function(y) y[["labels"]])
  NAMES <- lapply(x,function(y) y[["name"]]) %>% unlist %>% unname
  names(d) <- NAMES
  names(l) <- NAMES
  x <- create_matlist(x=d, class=class, labels=l)
  return(x)
  
}
collect_omat <- function(x,what=c("omega_block")) collect_matrix(x,what,"omegalist", "omega")
collect_smat <- function(x,what=c("sigma_block")) collect_matrix(x,what,"sigmalist","sigma")


## May have multiple $FIXED
collect_fixed <- function(x, what=c("fixed_list")) {
  
  x <- x[grepl("FIXED",names(x),perl=TRUE)]
  
  names(x) <- NULL
  
  x <- do.call("c",x)
  
  if(length(x)>0) return(x)
  
  return(list())
}

## May have multiple $PARAM blocks; also needs to collect from $NMXML
collect_param <- function(x, what=c("parameter_list")) {
  
  what <- c(what, "NMXMLDATA")
  
  x <- x[sapply(x,inherits,what)]
  
  xmli <- unlist(sapply(x,inherits,"NMXMLDATA"))
  
  x[xmli] <- lapply(x[xmli], function(x) x$theta)
  
  names(x) <- NULL
  
  x <- unlist(lapply(x, as.numeric))
  
  if(length(x)>0) return(as.param(x))
  
  return(as.param(list()))
}

## Merges code from $TABLE and $CAPTURE
collect_table <- function(x,what=c("CAPTURE", "TABLE")) {
  x <- x[names(x) %in% what]
  unname(unlist(x))
}

## Look for initial conditions in $INIT, $CMT, and $VCMT
collect_init <- function(x,what=c("INIT", "CMT", "VCMT")) {
  x <- x[names(x) %in% what]
  if(length(x)==0) return(as.init(list()))
  names(x) <- NULL
  x <- do.call("c",x)
  return(as.init(x))
}

## Collect PKMODEL information; hopefully will be deprecating ADVAN2 and ADVAN4 soon
collect_subr <- function(x,what=c("ADVAN2", "ADVAN4","PKMODEL")) {
  
  ans <- list(advan=13,trans=1,strict=FALSE)
  
  y <- x[names(x) %in% what]
  
  if(length(y) >  1) stop("Only one of $ADVAN2, $ADVAN4, or $PKMODEL are allowed.",call.=FALSE)
  if(length(y) == 0) return(ans)
  ## Get rid of this once ADVANn are deprecated
  if(names(y)=="ADVAN2") {
    ans$advan <- 2
  }
  if(names(y)=="ADVAN4") {
    ans$advan <- 4
  }
  if(names(y) %in% c("PKMODEL")) {
    ans <- y[[1]]
  }
  
  if(ans[["advan"]] != 13) {
    if(any(is.element(c("VCMT"),names(x)))) stop("Found $VCMT and $ADVANn in the same control stream.")
    if(any(is.element("ODE", names(x)))) stop("Found $ODE and $ADVANn in the same control stream.")
  }
  
  ans[["n"]] <- ans[["advan"]] - as.integer(ans[["advan"]] > 2)
  
  return(ans)
}


dosing_cmts <- function(x,what) {
  if(!is.character(x)) return(character(0))
  x <- unlist(strsplit(x,"\n"))
  m <- regexpr("(ALAG|F|R|D)\\_[^= ]+", x, perl=TRUE)
  m <- regmatches(x,m)
  m <- unique(gsub("(ALAG|F|R|D)\\_", "",m))
  m <- intersect(m,what)
  return(m)
}

## Picks the default trans
pick_trans <- function(ncmt,depot) {
  switch(pick_advan(ncmt,depot),
         `1` = 2,
         `2` = 2,
         `3` = 4,
         `4` = 4
  )
}

## Picks advan based on ncmt and depot status
pick_advan <- function(ncmt,depot) {
  ncmt + as.integer(depot) + as.integer(ncmt==2)
}

check_pred_symbols <- function(x,code) {
  p <- pars(x)
  code <- unlist(get_tokens(code,TRUE))
  have <- unique(c(p,code))
  
  if(x@trans==1) return(invisible(NULL))
  
  need <- GLOBALS$ADVAN_PARMS[[as.character(x@advan)]]
  # assuming error checking has already processed for a valid advan,
  # however could add error check here with if (is.null(need)) {stop(...)}
  if(x@trans==11) need <- paste0(need,"i")
  if(!all(need %in% have)) {
    diff <- setdiff(need,have)
    stop(GLOBALS$PKMODEL_NOT_FOUND,paste(diff, collapse=","),call.=FALSE)
  }
  return(invisible(NULL))
}


