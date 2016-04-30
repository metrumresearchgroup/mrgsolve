## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

## TO BE REMOVED 4/29/16
##c_com_start <- "/\\*"
##c_com_end <- "\\*/"
##GlobalVarRe <- "\\s*Global\\s+(double|int|bool|).*"
##eol.comment <- "^([^#]*)\\#+.*$"; dhash <- "^([^#]*)\\##+.*$"; dslash <- "^(.*)//.*$"
labre <- "\\s*\\$([A-Z,a-z,0-9]+)\\s*.*"
drop.labre <- "\\$[A-Z,a-z,0-9]+\\s*(.*)"
globalre2 <- "^\\s*(predpk|double|bool|int)\\s+\\w+"


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


##' @include utils.R complog.R nmxml.R
rfile <- function(pattern="",tmpdir=normalizePath(getwd(),winslash="/")){
  basename(tempfile(pattern=pattern,tmpdir='.'))
}

set_args <- c("Req", "obsonly","mtime", "recsort",
              "carry.out","Trequest","trequest")

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


fixed_parameters <- function(x) {
  if(length(x)==0) return("")
  paste0(paste(paste("const double", names(x)), unlist(x), sep="=", collapse=";\n"),';')
}

## TO BE REMOVED 4/29/16
## default.nmext <- list(theta=TRUE,omega=FALSE,sigma=FALSE,prefix="THETA",omprefix="OMEGA", sgprefix="SG")

## TO BE REMOVED 4/29/16
## ulist <- function(x, combine=unique(names(x)),fromLast=FALSE,...) {
##   if(is.null(x)) return(list())
##   if(class(x)!="list") stop("x must be a list", call.=FALSE)
##   combine <- as.cvec(combine)
##   lnames <- names(x)
##   dupn <- intersect(combine,unique(lnames[duplicated(lnames)]))
##   dup <- lapply(dupn, function(i) {
##     paste(unlist(x[grep(i,lnames)]),collapse=",")
##   })
##   names(dup) <- dupn
##   ans <- c(x[!is.element(names(x),dupn)],dup)
##   return(ans[!duplicated(names(ans), fromLast=fromLast)])
## }


compfile <- function(x,project) file.path(project,paste0(x, ".cpp.cpp"))

##' Parse model specification text
##' @param txt model specification text
##' @param split logical
##' @param ... arguments passed along
##' @export
modelparse <- function(txt,split=FALSE,...) {
  ## Take in model text and parse it out

  if(split) txt <- strsplit(txt,"\n",perl=TRUE)[[1]]

  txt <- strsplit(txt, "##+|//+",perl=TRUE)
  txt <- sapply(txt, FUN=function(x) x[1])
  txt[is.na(txt)] <- ""

  start <- grep(labre,txt)

  if(length(start)==0) stop("No model specification file blocks were found.", call.=FALSE)

  labs <- gsub(labre,"\\1", txt[start])
  txt[start] <- gsub(drop.labre, "\\1", txt[start])

  end <- c((start-1),length(txt))[-1]

  foo <- lapply(seq_along(start), function(i) {
    y <- txt[start[i]:end[i]]
    y[y!=""]
  })

  names(foo) <- labs

  foo
}


altglobal <- function(code,moveto="GLOBAL",
                      what=grepl("MAIN|ODE|TABLE",names(code),perl=TRUE)) {

  check <- grep("^\\s*(predpk|bool|int|double)", unlist(code[what]), value=TRUE,perl=TRUE)

  if(any(sapply(strsplit(gsub("[><!=]=", " ",check), "=",perl=TRUE),length)>2)) {

    warning("Multiple variable declarations are not allowed in MAIN, ODE, or TABLE.")
  }

  vars <- regmatches(check,regexpr(globalre2,check,perl=TRUE))

  code[what] <- lapply(code[what], gsub, pattern="^\\s*(predpk|double|int|bool)\\s+", replacement="")

  vars <- unlist(vars)

  if(length(vars)>0) vars <- paste0(vars, ";")

  code[[moveto]] <- c(code[[moveto]], "typedef double localdouble;","typedef int localint;","typedef bool localbool;",vars)

  return(code)
}

##' TO BE REMOVED 4/29/16
## inclu <- function(x) paste0("#include \"",x,".h\"")

## block_x <- function(x,y="",z="DONE") {
##   x <- c(y, x, z)
##   paste(x, collapse="\n")
## }


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

  project <- normalizePath(project,mustWork=TRUE, winslash="/")
  soloc <- normalizePath(soloc,mustWork=TRUE, winslash="/")

  if(!missing(code) & missing(model)) model <- "_mrgsolve_temp"
  if(grepl(" +", model,perl=TRUE)) stop("model name cannot contain spaces.")

  modfile <- file.path(project,paste0(model, ".cpp"))

  temp <- rfile(model)
  temp <- ifelse(udll,temp,model)
  temp_write <- compfile(temp,project)

  package <- temp

  if(audit) warn <- TRUE

  args <- list(...)

  ## Copy the main model header into project:
  modelheaders <-file.path(system.file(package="mrgsolve"), "include", c("mrgsolv.h","modelheader.h"))
  file.copy(modelheaders,project, overwrite=TRUE)

  ## If code is passed in as character:
  if(!missing(code)) {
    mod.con <- file(modfile, open="w")
    cat(code, "\n", file=mod.con)
    close(mod.con)
  }

  if(!file.exists(modfile)) {
    stop(paste0("Could not find model file ", modfile), call.=FALSE)
  }

  ## Read the model spec and parse:
  cfile <- compfile(model,build_path(project))

  spec  <- modelparse(readLines(modfile,warn=FALSE))

  ## Block name aliases and partial matches to block_list
  names(spec) <- gsub("DES", "ODE",  names(spec), fixed=TRUE)
  names(spec) <- gsub("^PK$",  "MAIN", names(spec), fixed=FALSE)
  index <- pmatch(names(spec),block_list,duplicates.ok=TRUE)
  names(spec) <- ifelse(is.na(index),names(spec),block_list[index])

  check_spec_contents(names(spec),warn=warn,...)

  ## The main sections that need R processing:
  spec <- altglobal(spec)

  ## Parse blocks
  specClass <- paste0("spec", names(spec))
  for(i in seq_along(spec)) class(spec[[i]]) <- specClass[i]
  spec <- lapply(spec,handle_spec_block)

  ## Collect potential multiples
  subr  <- collect_subr(spec)
  omega <- collect_omat(spec)
  sigma <- collect_smat(spec)
  param <- collect_param(spec)
  fixed <- collect_fixed(spec)
  table <- collect_table(spec)
  init  <- collect_init(spec)

  SET <- as.list(spec[["SET"]])
  ENV <- spec[["ENV"]]

  dosing <- dosing_cmts(spec[["MAIN"]], names(init))

  SET[["CMTN"]] <- c(spec[["CMTN"]],dosing)

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
           package=temp,
           project=project,
           fixed=fixed,
           advan=subr[["advan"]],
           trans=subr[["trans"]],
           omega=omega,sigma=sigma,
           param=as.param(param),
           init=as.init(init))

  ## Two compartments for ADVAN 2, 3 compartments for ADVAN 4
  if(x@advan != 13) {
      if(subr[["n"]] != neq(x)) {
          stop("$PKMODEL requires  ", subr[["n"]] , " compartments in $CMT or $INIT.",call.=FALSE)
      }
  }

  ## First update with what we found in the model specification file
  x <- update(x, data=SET, strict=FALSE)

  ## Arguments in $SET that will be passed to mrgsim
  simargs <- SET[is.element(names(SET),set_args)]
  if(length(simargs)>0) x@args <- merge(x@args,simargs, strict=FALSE)

  ## Next, update with what the user passed in as arguments
  x <- update(x, data=args,strict=FALSE)

  if(audit) audit_spec(x,spec,warn=warn)

  ## This must come after audit
  if(is.null(spec[["ODE"]])) spec[["ODE"]] <- "DXDTZERO();\n"

  ## These are the symbols:
  x <- assign_symbols(x)

  if(x@advan !=13) {
      check_pred_symbols(x,spec[["MAIN"]])
  }

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

  def.con <- file(temp_write, open="w")
  cat(rd, sep="\n", file=def.con)
  cat("\n// GLOBAL VARIABLES:\n",
      fixed_parameters(fixed),
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
      "END_table",sep="\n", file=def.con)
  close(def.con)

  if(!compile) return(x)

  if(ignore.stdout & !quiet) message("Compiling ",basename(cfile)," ... ", appendLF=FALSE)

  preclean <- preclean | (!logged(model(x)))

  clean <- ifelse(preclean, " --preclean ", "")

  safe_wait(x)

  check_and_copy(x,temp,preclean)

  purge_model(cfile(x))

  status <- system(paste0("R CMD SHLIB ", clean, cfile, " -o ",sodll(x,short=TRUE)),
                   ignore.stdout=ignore.stdout)

  file.remove(compfile(temp,project(x)))

  if(status!=0) {
    warning("Compile did not succeed.  Returning NULL.", immediate.=TRUE,call.=FALSE);
    return(NULL)
  }

  if(ignore.stdout & !quiet) message("done.")
  store(x)
  if(!quiet) message("Loading: ", basename(sodll(x)))

  dyn.load(sodll(x))

  if(!dll_loaded(x)) stop("Model was not found after attempted loading.")
  x <- compiled(x,TRUE)
  x@shlib$date <- shdate(ntime())
  x@shlib$cmt <- cmt(x)
  x@shlib$par <- pars(x)
  x@shlib$npar <- length(pars(x))
  x@shlib$ncmt <- length(cmt(x))
  x@code <- readLines(modfile, warn=FALSE)

  return(x)
}

##' Create a square numeric matrix from the lower-triangular elements
##' @param x numeric data
##' @param prefix used to generate column names
##' @return a square symmetric numeric matrix with column names
lower2matrix <- function(x, prefix=NULL) {
  x <- as.numeric(x)
  if(length(x)==1) return(matrix(x,nrow=1, ncol=1 ))
  n <- 0.5*(sqrt(1-4*(-2*length(x)))-1)
  if(!n==as.integer(n)) {
    stop(paste("Block matrix has invalid specification: ", prefix),call.=FALSE)
  }
  mat <- diag(n)
  mat[upper.tri(mat,diag=TRUE)] <- x
  mat <- mat+t(mat) - diag(diag(mat))
  mat
}

##' Create a diagonal numeric matrix from diagonal elements
##' @param x numeric data
##' @param prefix used to generate column names
##' @return a numeric diagonal matrix
numeric2diag <- function(x,prefix=NULL) {
  x <- as.numeric(x)
  if(length(x)==1) {
    mat <- matrix(x)
  } else {
    mat <- diag(x)
  }
  mat
}


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


##' Create a matrix.
##'
##' @param x data for building the matrix.  Data in \code{x} are assumed to be on-diagonal elements if \code{block} is \code{FALSE} and lower-triangular elements if \code{block} is \code{TRUE}
##' @param name name
##' @param use logical; if FALSE, all matrix elements are set to 0
##' @param block logical; if TRUE, try to make a block matrix; diagonal otherwise
##' @param correlation logical; if TRUE, off diagonal elements are assumed to be correlations and converted to covariances; if correlation is TRUE, then block is set to TRUE
##' @param digits if value of this argument is greater than zero, the matrix is passed to signif (along with digits) prior to returning
##' @param ... passed along
##'
##' @examples
##' modMATRIX("1 2.2 333")
##' modMATRIX("1 1.1 2.2", block=TRUE)
##' modMATRIX("23 234 234 5234", use=FALSE)
##'
##' ans <- modMATRIX("1.1 0.657 2.2", correlation=TRUE, block=TRUE)
##' ans
##' cov2cor(ans)
##'
##' @export
##'
##'

modMATRIX <- function(x,name="",use=TRUE,block=FALSE,correlation=FALSE,digits=-1,...) {
  if(correlation) block <- TRUE
  if(is.character(x)) x <- unlist(strsplit(x, "\\s+",perl=TRUE))
  if(!use) x <- rep(0,length(x))
  if(block) {
    x <- lower2matrix(x)
    if(correlation) decorr(x)
  } else {
    x <- numeric2diag(x)
  }
  if(digits > 0) x <- signif(x, digits=digits)

  return(x)
}


Diag <- function(x) {
  if(is.matrix(x)) return(x)
  diag(x, nrow=length(x),ncol=length(x))
}

##' Create matrices from vector input
##' @param ... matrix data
##' @param correlation logical; if TRUE, off diagonal elements are assumed to be correlations and converted to covariances
##' @param digits if greater than zero, matrix is passed to signif (along with digits) prior to returning
##' @details
##' \code{bmat} makes a block matrix.  \code{cmat} makes a correlation matrix.  \code{dmat} makes a diagonal matrix.
##' \code{BLOCK} is a synonym for \code{bmat}.
##'
##' @export
##' @examples
##'
##' dmat(1,2,3)/10
##'
##' bmat(0.5,0.01,0.2)
##'
##' cmat(0.5, 0.87,0.2)
##'
##'
##' @seealso as_bmat
bmat <- function(...,correlation=FALSE, digits=-1) {
  x <- lower2matrix(unlist(list(...)),prefix="bmat")
  if(correlation) decorr(x)
  if(digits>0) x <- signif(x,digits=digits)
  return(x)
}
##' @export
##' @rdname bmat
BLOCK <- bmat

##' @export
##' @rdname bmat
cmat <- function(...,digits=-1) {
  bmat(...,digits=digits,correlation=TRUE)
}

##' @export
##' @rdname bmat
##' @seealso as_dmat
dmat <- function(...) {
  Diag(as.numeric(unlist(list(...))))
}

grepn <- function(x,pat,warn=FALSE) {
  if(is.null(names(x))) {
    if(warn) warning("grepn: pattern was specified, but names are NULL.", call.=FALSE)
    return(x)
  }
  if(pat=="*") return(x)
  x[grepl(pat,names(x),perl=TRUE)]
}


##' Coerce R objects to block or diagonal matrices.
##'
##' @param x an R object
##' @param pat regular expression, character
##' @param ... passed along
##' @return A numeric matrix for list and numeric methods.  For data.frames, a list of matrices are returned.
##' @seealso bmat, dmat
##' @export
##' @examples
##'
##' df <- data.frame(OMEGA1.1 = c(1,2),
##'                  OMEGA2.1 = c(11,22),
##'                  OMEGA2.2 = c(3,4),
##'                  SIGMA1.1 = 1,
##'                  FOO=-1)
##'
##' as_bmat(df, "OMEGA")
##' as_dmat(df,"SIGMA")
##' as_dmat(df[1,],"OMEGA")
##'

setGeneric("as_bmat", function(x,...) standardGeneric("as_bmat"))
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "list", function(x,...) {as_bmat(unlist(x),...)})
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "numeric", function(x,pat="*",...) {
  x <- grepn(x,pat, !missing(pat))
  do.call("bmat", list(x))
})
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "data.frame", function(x,pat="*", ...) {
  x <- x[,grepl(pat,names(x)),drop=FALSE]
  lapply(seq_len(nrow(x)), function(i) bmat(unlist(x[i,])))
})
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "ANY", function(x,...) {as_bmat(as.data.frame(x),...)})

##' @export
##' @rdname as_bmat
setGeneric("as_dmat", function(x,...) standardGeneric("as_dmat"))
##' @export
##' @rdname as_bmat
setMethod("as_dmat", "list", function(x,...) {as_dmat(unlist(x),...)})
##' @export
##' @rdname as_bmat
setMethod("as_dmat", "ANY", function(x,...) {as_dmat(as.data.frame(x),...)})

##' @export
##' @rdname as_bmat
setMethod("as_dmat", "numeric", function(x,pat="*",...) {
  x <- grepn(x,pat, !missing(pat))
  do.call("dmat", list(x))
})
##' @export
##' @rdname as_bmat
setMethod("as_dmat", "data.frame", function(x,pat="*", ...) {
  x <- x[,grepl(pat,names(x)),drop=FALSE]
  lapply(seq_len(nrow(x)), function(i) dmat(unlist(x[i,])))
})



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


specMATRIX <- function(x,class) {

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
  paste0("capture(",as.cvec(x),");")
}


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

##' Parse data from \code{$PKMODEL}
##'
##' @param ncmt number of compartments
##' @param depot logical indicating whether to add depot compartment
##' @param trans the parameterization for the PK model
##' @param ... not used
PKMODEL <- function(ncmt=1, depot=FALSE, trans = pick_trans(ncmt,depot), ...) {
    stopifnot(ncmt %in% c(1,2))
    advan <- pick_advan(ncmt,depot)
    return(list(advan=advan, trans=trans, n=ncmt))
}


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

collect_fixed <- function(x, what=c("fixed_list")) {

  x <- x[grepl("FIXED",names(x),perl=TRUE)]

  names(x) <- NULL

  x <- do.call("c",x)

  if(length(x)>0) return(x)

  return(list())
}

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


collect_table <- function(x,what=c("CAPTURE", "TABLE")) {
  x <- x[names(x) %in% what]
  unname(unlist(x))
}


collect_init <- function(x,what=c("INIT", "CMT", "VCMT")) {
  x <- x[names(x) %in% what]
  if(length(x)==0) return(as.init(list()))
  names(x) <- NULL
  x <- do.call("c",x)
  return(as.init(x))
}

collect_subr <- function(x,what=c("ADVAN2", "ADVAN4","PKMODEL")) {

    ans <- list(advan=13,trans=1,strict=FALSE)

    y <- x[names(x) %in% what]

    if(length(y) >  1) stop("Only one of $ADVAN2, $ADVAN4, or $PKMODEL are allowed.",call.=FALSE)
    if(length(y) == 0) return(ans)

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

    ans[["n"]] <-switch(ans[["advan"]],`1` = 1,`2` = 2,`3` = 2,`4` = 3)

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



## picks the default trans
pick_trans <- function(ncmt,depot) {
  switch(pick_advan(ncmt,depot),
    `1` = 2,
    `2` = 2,
    `3` = 4,
    `4` = 4
  )
}

pick_advan <- function(ncmt,depot) {
    ncmt + as.integer(depot) + as.integer(ncmt==2)
}


PK_ERR <- "Required PK parameters not found: "

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
        stop(PK_ERR,paste(diff, collapse=","),call.=FALSE)
    }
    return(invisible(NULL))

}


