# Copyright (C) 2013 - 2019  Metrum Research Group
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

## S3 methods for processing code blocks
## All of these need to be exported


# UTILS ------------------------------------------------------------------------
get_length <- function(what) sum(sapply(what,length))

# GENERIC ----------------------------------------------------------------------
handle_spec_block <- function(x,...) UseMethod("handle_spec_block")

#' @export
handle_spec_block.default <- function(x,...) {
  return(dump_opts(x))
}

# OMEGA /SIGMA -----------------------------------------------------------------

#' @export
handle_spec_block.specOMEGA <- function(x,...) {
  scrape_and_call(
    x,
    pass="specMATRIX",
    def=list(oclass="omegalist",type="omega"),
    narrow=FALSE,...
  )
}

#' @export
handle_spec_block.specSIGMA <- function(x,...) {
  scrape_and_call(
    x,
    pass="specMATRIX",
    def=list(oclass="sigmalist",type="sigma"),
    narrow=FALSE,...
  )
}

specMATRIX <- function(x,
                       oclass,type, annotated = FALSE,
                       env, pos=1, as_object = FALSE,
                       name="...", prefix="", labels=NULL,
                       object=NULL, unlinked=FALSE,...) {
  
  if(is.null(object)) check_block_data(x,env$ENV,pos)
  
  if(as_object) {
    expect <- paste0(type, "list")
    expect <- c("matrix",expect)
    if(type=="omega") {
      fun <- omat  
    } else {
      fun <- smat  
    }
    x <- evaluate_at_code(x, expect, toupper(type), pos, env, fun)
    env[[type]][[pos]] <- x
    return(NULL)
  }
  
  anl <- grepl(":",x,fixed=TRUE)
  if(annotated) {
    types <- charcount(x[anl],":")
    if(all(types==1)) {
      unlinked <- TRUE 
      novalue <- TRUE
    } else if(all(types==2)) {
      unlinked <- FALSE
      novalue <- FALSE
    } else {
      stop(
        "Ambigious or mixed annotations in ",
        paste0("$",toupper(type)),
        call.=FALSE
      )
    }
    
    l <- parse_annot(
      x[anl],
      name_value=FALSE,
      block=toupper(type),
      envir=env$ENV,novalue=novalue
    )
    
    if(unlinked) {
      l[["v"]] <- as.numeric(cvec_cs(x[!anl])) 
    }
    
    d <- modMATRIX(l[["v"]],context=oclass,...)
    labels <- l[["an"]][["name"]]
    env[["annot"]][[pos]] <- l[["an"]]
    
    if(unlinked & nrow(d) != length(labels)) {
      stop(
        "Annotated matrix in unlinked configuration is misspecified", 
        call. = FALSE
      )
    }
    
  } else {
    if(any(anl)) x <- x[!anl]
    if(is.null(object)) {
      d <- modMATRIX(x,context=oclass,...) 
    } else {
      d <- get(object,env$ENV)
    }
  }
  
  if(nrow(d)==0) return(NULL)
  
  if(is.null(labels)) {
    labels <- rep(".", nrow(d))
  } else {
    labels <- paste0(prefix,cvec_cs(labels))
  }
  
  d <- setNames(list(d),name)
  
  x <- create_matlist(d,class=oclass,labels=list(labels))
  
  env[[type]][[pos]] <- x
  
  return(NULL)
}

# specTABLE --------------------------------------------------------------------

#' @export 
handle_spec_block.specTABLE <- function(x,env,...) {
  
  x <- dump_opts(x)
  
  pos <- attr(x,"pos")
  
  check_block_data(x,env$ENV,pos)
  
  if(any(grepl("table(", x,fixed=TRUE))) {
    stop("The table(name) = value; macro has been deprecated.\n",  
         "Save your output to double and pass to $CAPTURE instead:\n",
         "   $TABLE double name = value;\n   $CAPTURE name")
  }
  
  return(x)
}

#' Functions to parse code blocks
#' 
#' Most of the basic blocks are listed in this help topic.  
#' But see also \code{\link{PKMODEL}} which has more-involved 
#' options and is documented separately.
#' 
#' @param x data
#' @param env parse environment
#' @param annotated logical
#' @param covariates logical
#' @param name block name
#' @param pos block position
#' @param fill data to use for block contents
#' @param as_object indicates that object code is being provided
#' @param ... passed
#' 
#' @name BLOCK_PARSE
#' @rdname BLOCK_PARSE
#' 
#' @seealso \code{\link{PKMODEL}}
NULL

# PARAM / FIXED ----------------------------------------------------------------

#' @export
handle_spec_block.specPARAM <- function(x,...) {
  scrape_and_call(x,pass="PARAM",...)
}

#' @rdname BLOCK_PARSE
PARAM <- function(x,env,annotated=FALSE,covariates=FALSE,pos=1,as_object=FALSE,...) {
  
  check_block_data(x,env$ENV,pos)
  
  if(as_object) {
    x <- evaluate_at_code(x, c("list", "parameter_list"), "PARAM", pos, env)
    env[["param"]][[pos]] <- x
    return(NULL)
  }
  
  if(annotated) {
    context <- glue("parse annotated parameter block ({pos})")
    l <- parse_annot(x,block="PARAM",envir=env$ENV,context = context)
    env[["param"]][[pos]] <- l[["v"]]
    env[["annot"]][[pos]] <- l[["an"]]
  } else {
    x <- tolist(x,envir=env$ENV) 
    env[["param"]][[pos]] <- x
  }
  
  if(covariates) {
    env[["covariates"]] <- c(
      env[["covariates"]], names(env[["param"]][[pos]])
    )
  }
  
  return(NULL)
}

#' @export
handle_spec_block.specFIXED <- function(x,...) {
  scrape_and_call(x,pass="FIXED",...)
}

#' @rdname BLOCK_PARSE
FIXED <- function(x,env,annotated=FALSE,pos=1,...) {
  
  check_block_data(x,env$ENV,pos)
  
  if(annotated) {
    context <- glue("parse annotated fixed block ({pos})")
    l <- parse_annot(x,block="FIXED",envir=env$ENV,context=context)
    env[["fixed"]][[pos]] <- l[["v"]]
    env[["annot"]][[pos]] <- l[["an"]]
  } else {
    x <- tolist(x,envir=env$ENV) 
    env[["fixed"]][[pos]] <- x
  }
  
  return(NULL)
}

# THETA ------------------------------------------------------------------------

#' @export
handle_spec_block.specTHETA <- function(x,...) {
  scrape_and_call(x,pass="THETA",narrow=FALSE,...)
}

#' @rdname BLOCK_PARSE
THETA <- function(x, env, annotated=FALSE, pos=1, name="THETA", fill = NULL,
                  ...) {
  if(!is.null(fill)) {
    x <- eval(parse(text = fill))   
  }
  
  check_block_data(x,env$ENV,pos)
  
  if(annotated) {
    l <- parse_annot(x,noname=TRUE,block="THETA",envir=env$ENV)
    x <- as.numeric(l[["v"]])
  } else {
    x <- tolist(paste0(cvec_cs(x),collapse=','),envir=env$ENV)
  }
  
  x <- x[!is.na(x)]
  
  if(length(x)==0) name <- character(0)
  
  names(x) <- paste0(name, seq_along(x))
  
  env[["param"]][[pos]] <- x
  
  return(NULL)
}

# INIT /CMT --------------------------------------------------------------------

#' @export
handle_spec_block.specINIT <- function(x,...) {
  scrape_and_call(x,pass="INIT",...)
}

#' @rdname BLOCK_PARSE
INIT <- function(x,env,annotated=FALSE,pos=1,as_object=FALSE,...) {
  
  check_block_data(x,env$ENV,pos)
  
  if(as_object) {
    x <- evaluate_at_code(x, "list", "INIT", pos, env, as.list)
    env[["init"]][[pos]] <- x
    return(NULL)
  }
  
  if(annotated) {
    context <- glue("parse annotated init block ({pos})")
    l <- parse_annot(x,block="INIT",envir=env$ENV,context=context)
    env[["init"]][[pos]] <- l[["v"]]
    env[["annot"]][[pos]] <- l[["an"]]
  } else {
    x <- tolist(x,envir=env$ENV) 
    env[["init"]][[pos]] <- x
  }
  return(NULL)
}

#' @export
handle_spec_block.specCMT <- function(x,...) {
  scrape_and_call(x,pass="CMT",narrow=FALSE,...)
}

#' @rdname BLOCK_PARSE
CMT <- function(x,env,annotated=FALSE,pos=1, as_object = FALSE, ...) {
  check_block_data(x,env$ENV,pos)
  if(as_object) {
    x <- evaluate_at_code(x, "character", "CMT", pos, env, as.character)
    x <- setNames(rep(0,length(x)), x)
    env[["init"]][[pos]] <- x
    return(NULL)
  }
  if(annotated) {
    context <- glue("parse annotated compartment block ({pos})")
    l <- parse_annot(x,novalue=TRUE,block="CMT",envir=env$ENV,context=context)
    env[["annot"]][[pos]] <- l[["an"]]
    x <- names(l[["v"]])
  } else {
    x <- cvec_cs(x)
  }
  
  l <- rep(0,length(x))
  names(l) <- x
  env[["init"]][[pos]] <- as.list(l)
  return(NULL)
}

#' @export
handle_spec_block.specVCMT <- handle_spec_block.specCMT


#' @export
handle_spec_block.specCMTN <- function(x,...) {
  cvec_cs(dump_opts(x))
}

# SET --------------------------------------------------------------------------

#' @export
handle_spec_block.specSET <- function(x,...) {
  tolist(dump_opts(x))
}

#' @export
handle_spec_block.specNMXML <- function(x,...) {
  parseNMXML(dump_opts(x),...)
}

## Functions for handling code blocks
parseNMXML <- function(x,env,...) {
  pos <- attr(x,"pos")
  x <- tolist(x,envir=env$ENV)
  xml <- do.call(nmxml,x)
  env[["param"]][[pos]] <- xml$theta
  env[["omega"]][[pos]] <- xml$omega
  env[["sigma"]][[pos]] <- xml$sigma
  return(NULL)
}

# CAPTURE ----------------------------------------------------------------------

#' @export
handle_spec_block.specCAPTURE <- function(x,...) {
  scrape_and_call(x,pass="CAPTURE",narrow=TRUE,...)
}

#' @rdname BLOCK_PARSE
CAPTURE <- function(x,env,annotated=FALSE,pos=1,...) {
  
  if(annotated) {
    context <- glue("parse annotated capture block ({pos})")
    l <- parse_annot(x,novalue=TRUE,block="CAPTURE",envir=env$ENV,context=context)
    env[["annot"]][[pos]] <- l[["an"]]
    x <- names(l[["v"]])
  } else {
    x <- cvec_cs(x)
  }
  
  check_block_data(x,env$ENV,pos)
  
  env[["capture"]][[pos]] <- x
  
  return(NULL)
}

# PRED -------------------------------------------------------------------------

#' @export
handle_spec_block.specPRED <- function(x,env,...) {
  x <- scrape_opts(x)
  x$env <- env
  x$pos <- attr(x,"pos")
  do.call("PRED",x)
}

PRED <- function(x,env,...) {
  if(any("MAIN"==env[["blocks"]])) {
    stop("$MAIN not allowed when $PRED is used",call.=FALSE)  
  }
  if(any("TABLE"==env[["blocks"]])) {
    stop("$TABLE not allowed when $PRED is used",call.=FALSE)  
  }
  if(any("PKMODEL"==env[["blocks"]])) {
    stop("$PKMODEL not allowed when $PRED is used",call.=FALSE)  
  }
  if(any("CMT"==env[["blocks"]])) {
    stop("$CMT not allowed when $PRED is used",call.=FALSE)  
  }
  if(any("INIT"==env[["blocks"]])) {
    stop("$INIT not allowed when $PRED is used",call.=FALSE)  
  }
  if(any("ODE"==env[["blocks"]])) {
    stop("$ODE not allowed when $PRED is used",call.=FALSE)  
  }
  return(x)
}

# INCLUDE ----------------------------------------------------------------------

#' @export
handle_spec_block.specINCLUDE <- function(x,env,...) { 
  
  x <- cvec_c_tr(dump_opts(x))
  
  pos <- attr(x, "pos")
  
  check_block_data(x,env$ENV,pos)
  
  if(any(grepl("[\"\']",x,perl=TRUE))) {
    stop("Items in $INCLUDE should not contain quotation marks.",call.=FALSE) 
  }
  
  if(any(!grepl("^.*\\.h$",x,perl=TRUE))) {
    warning(
      "$INCLUDE expects file names ending with '.h'",
      call.=FALSE,
      immediate.=TRUE
    ) 
  }
  
  x <- file.path(env[["project"]],x)
  
  if(!all(file_exists(x))) {
    message("Attempting to include:\n",paste0(" ",x, collapse = "\n"))
    stop(
      "All header files in $INCLUDE must exist in the project directory.",
      call.=FALSE
    ) 
  }
  
  return(x)
}

form_includes <- function(files) {
  if(is.null(files)) return(character(0))
  md <- tools::md5sum(files)
  paste0("#include \"", files, "\" // ", md)
}

# PLUGIN -----------------------------------------------------------------------

#' @export
handle_spec_block.specPLUGIN <- function(x,env,...) {
  
  x <- unique(cvec_c_tr(x))
  
  pos <- attr(x,"pos")
  
  check_block_data(x,env$ENV,pos)
  
  if(length(x) ==0) return(list())
  
  return(x)
}

# TRANSIT ----------------------------------------------------------------------
# nocov start
#' @export
handle_spec_block.specTRANSIT <- function(x,env,...) {
  x <- scrape_opts(x, narrow=FALSE)
  x$env <- env
  x$pos <-  attr(x,"pos")
  do.call("TRANSIT",x)
}

TRANSIT <- function(x,env, n, tr, name = "TRANSIT", pos=1) {
  nto <- get_length(env$init)
  cmtn <- nto  + seq(n)
  init <- as.list(vector("numeric", n))
  cmt <- paste0(name, cmtn)
  cmt[1] <- paste0("a",name)
  cmt[length(cmt)] <- paste0("z",name)
  names(init) <- cmt
  dx <- paste0("dxdt_", cmt, " = ")
  eq <- c()
  for(i in seq_along(cmt)) {
    if(i==1) {
      a <- "0.0"
    } else {
      a <- cmt[i-1]
    }
    b <- cmt[i]
    this_rhs <- paste0(tr, "*(", a, "-", b,");")
    eq <- c(eq,this_rhs)
  }
  env[["ode"]][[pos]] <- paste0(dx,eq)
  env[["init"]][[pos]] <- init
  return(NULL)
}
# nocov end

# PKMODEL ----------------------------------------------------------------------

#' @export
handle_spec_block.specPKMODEL <- function(x,env,...) {
  pos <- attr(x,"pos")
  x <- scrape_opts(x, narrow=FALSE)
  x$env <- env
  x$pos <-  pos
  do.call("PKMODEL",x)
}

#' Parse PKMODEL BLOCK data
#' @param cmt compartment names as comma-delimited character
#' @param ncmt number of compartments; must be 1 (one-compartment, 
#' not including a depot dosing compartment) or 2 (two-compartment model, 
#' not including a depot dosing compartment)
#' @param depot logical indicating whether to add depot compartment
#' @param trans the parameterization for the PK model; must be 1, 2, 4, or 11
#' @param env parse environment
#' @param pos block position number
#' @param ... not used
#'
#' @details
#' When using \code{$PKMODEL}, certain symbols must be defined in the 
#' model specification depending on the value of \code{ncmt}, \code{depot} 
#' and \code{trans}.
#'
#' \itemize{
#' \item \code{ncmt} 1, \code{depot FALSE}, trans 2: \code{CL}, \code{V}
#' \item \code{ncmt} 1, \code{depot TRUE} , trans 2: \code{CL}, \code{V},  
#' \code{KA}
#' \item \code{ncmt} 2, \code{depot FALSE}, trans 4: \code{CL}, \code{V1}, 
#' \code{Q}, \code{V2}
#' \item \code{ncmt} 2, \code{depot TRUE} , trans 4: \code{CL}, \code{V2}, 
#' \code{Q}, \code{V3}, \code{KA}
#'
#' }
#'
#' If \code{trans=11} is specified, use the symbols listed above for the 
#' \code{ncmt} / \code{depot} combination, but append \code{i} at the end 
#' (e.g. \code{CLi} or \code{Qi} or \code{KAi}).
#'
#' If \code{trans=1}, the user must utilize the following symbols:
#'
#' \itemize{
#' \item \code{pred_CL} for clearance
#' \item \code{pred_V}  or \code{pred_V2} for central compartment volume of 
#' distribution
#' \item \code{pred_Q}  for intercompartmental clearance
#' \item \code{pred_V3} for for peripheral compartment volume of distribution
#' \item \code{pred_KA} for absorption rate constant
#'
#' }
#' 
#' @seealso \code{\link{BLOCK_PARSE}}
PKMODEL <- function(ncmt=1,depot=FALSE,cmt=NULL, trans=pick_trans(ncmt,depot),
                    env=list(), pos=1,...) {
  if(is.character(cmt)) {
    cmt <- cvec_cs(cmt)
    ncmt <- length(cmt)
    init <- as.list(vector(mode="integer", length=ncmt))
    names(init) <- cmt
    env[["init"]][[pos]] <- init  
    ncmt <- ncmt-depot
  }
  stopifnot(ncmt %in% c(1,2))
  advan <- pick_advan(ncmt,depot)

  return(list(advan=advan, trans=trans, n=ncmt))
}

## Collect PKMODEL information; hopefully will be deprecating ADVAN2 and ADVAN4 soon
collect_subr <- function(x,what=c("PKMODEL")) {
  
  if("PRED" %in% names(x)) {
    ans <- list(advan = 0, trans = 0, n = 0)
    return(ans)
  }
  
  ans <- list(advan=13,trans=1)
  
  y <- x[names(x) %in% what]
  
  if(length(y) >  1) {
    stop("Only one $PKMODEL block is allowed.",call.=FALSE)
  }
  if(length(y) == 0) return(ans)
  ## Get rid of this once ADVANn are deprecated
  if(names(y) %in% c("PKMODEL")) {
    ans <- y[[1]]
  }
  if(ans[["advan"]] != 13) {
    if(any(is.element(c("VCMT"),names(x)))) {
      stop("Found $VCMT and $PKMODEL in the same control stream.")
    }
    if(any(is.element("ODE", names(x)))) {
      stop("Found $ODE and $PKMODEL in the same control stream.")
    }
  }
  ans[["n"]] <- ans[["advan"]] - as.integer(ans[["advan"]] > 2)
  return(ans)
}

dosing_cmts <- function(x,what) {
  if(!is.character(x)) return(character(0))
  x <- unlist(strsplit(x,"\n",fixed=TRUE),use.names=FALSE)
  m <- regexpr("(ALAG|F|R|D)\\_[^= ]+", x, perl=TRUE)
  m <- regmatches(x,m)
  m <- unique(gsub("(ALAG|F|R|D)\\_", "",m))
  m <- intersect(m,what)
  return(m)
}

# Picks the default trans
pick_trans <- function(ncmt,depot) {
  switch(pick_advan(ncmt,depot),
         `1` = 2,
         `2` = 2,
         `3` = 4,
         `4` = 4
  )
}

# Picks advan based on ncmt and depot status
pick_advan <- function(ncmt,depot) {
  ncmt + as.integer(depot) + as.integer(ncmt==2)
}

check_pred_symbols <- function(x,code) {
  p <- Pars(x)
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

# YAML -------------------------------------------------------------------------
# nocov start
#' @export
handle_spec_block.specYAML <- function(x,env,...) {
  
  if(!requireNamespace("yaml")) {
    stop("the yaml package must be installed to process YAML blocks.",
         call.=FALSE)
  }
  
  pos <- attr(x,"pos")
  x <- paste0(x, collapse = "\n")  
  x <- yaml::yaml.load(x, eval.expr=TRUE)
  
  annotated3 <- function(value=0, descr = '.', unit = '.') {
    tibble(value = value, descr = descr, unit = unit)
  }
  annotated2 <- function(descr = '.', unit = '.') {
    annotated3(value = NA_real_,descr,unit)
  }
  handle_annotated <- function(data,what,block) {
    x <- lapply(data,as.list)
    x <- lapply(x,FUN=do.call,what = what) %>% bind_rows()
    mutate(x, block=block, name = names(data))
  }
  
  names(x) <- tolower(names(x))
  pars <- handle_annotated(x$param, annotated3, "PARAM")
  cmt <- handle_annotated(x$cmt, annotated2,"CMT")
  cmt <- mutate(cmt, value = 0)
  init <- handle_annotated(x$init, annotated3, "INIT")
  out <- handle_annotated(x$capture, annotated2, "CAPTURE")
  ann <- bind_rows(pars,cmt,init,out) %>% select(c("block","name","descr","value","unit"))
  parameters <- setNames(as.list(pars[["value"]]),pars[["name"]])
  compartments <- setNames(as.list(cmt[["value"]]),cmt[["name"]])
  initials <- setNames(as.list(init[["value"]]),init[["name"]])
  initials <- c(compartments,initials)
  outputs <- out[["name"]]
  env[["annot"]][[pos]] <- ann
  if(nrow(pars) > 0) env[["param"]][[pos]] <- parameters
  if(length(initials) > 0) env[["init"]][[pos]] <- initials
  if(nrow(out) > 0) env[["capture"]][[pos]] <- outputs
  return(invisible(NULL))
}
# nocov end

# NAMESPACE --------------------------------------------------------------------

#' @export
handle_spec_block.specNAMESPACE <- function(x,...) {
  scrape_and_call(x,pass="NAMESPACE",narrow=FALSE,...)
}

NAMESPACE <- function(x,env,name,unnamed=FALSE,pos=1,...) {
  if(unnamed) name <-  NULL
  env[["namespace"]][[pos]] <- wrap_namespace(x,name)
  return(NULL)
}

# ODE --------------------------------------------------------------------------

#' @export
handle_spec_block.specODE <- function(x,env,...) {
  pos <- attr(x,"pos")
  con <- scrape_opts(x,allow_multiple = TRUE) 
  x <- con[["x"]]
  if(isTRUE(con[["code"]])) {
    x <- eval(parse(text = x), envir=env$ENV)   
  }
  re <- "\\bETA\\([0-9]+\\)"
  chk <- grepl(re,x)
  if(any(chk)) {
    er1 <- "ETA(n) is not allowed in ODE block:"
    er2 <- paste0("\n * ", x[which(chk)])
    stop(er1,er2,call.=FALSE)
  }
  if("param" %in% names(con)) {
    env[["param"]][[pos]] <- tolist(con[["param"]])
  }
  return(x)
}

# BLOCK ------------------------------------------------------------------------

#' @export
handle_spec_block.specBLOCK <- function(x,env,...) {
  a <- tolist(x)
  x <- scrape_opts(x, narrow=FALSE)
  x$env <- env
  x$pos <-  attr(x,"pos")
  do.call("BLOCK",x)
}

BLOCK <- function(x,env,type,code=NULL,get=NULL,...) {
  x <- structure(.Data=code, class=paste0("spec",type),pos=1)
  handle_spec_block(x,env)
}
