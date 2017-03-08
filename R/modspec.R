# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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


# @include complog.R nmxml.R annot.R

globalre2 <- "^\\s*(predpk|double|bool|int)\\s+\\w+"
block_re <-  "^\\s*\\$[A-Z]\\w*|\\[\\s*[A-Z]\\w*\\s*]"

## Generate an advan/trans directive
advtr <- function(advan,trans) {
  if(advan==13 | trans==1) return(NULL)
  if((advan %in% c(1,2)) & !(trans %in% c(2,11))) {
    stop("ADVAN 1 and 2 can only use trans 1, 2, or 11", call.=FALSE)
  }
  if((advan %in% c(3,4)) & !(trans %in% c(4,11))) {
    stop("ADVAN 3 and 4 can only use trans 1, 4, or 11", call.=FALSE)
  }
  return(paste0("__ADVAN", advan, "_TRANS", trans, "__"))
}

write_capture <- function(x) {
  if(length(x)==0) return(NULL)
  i <- seq_along(x)
  paste0("_capture_[",i-1,"] = ", x[i], ";") 
}

## These are arguments to mrgsim that
## can be stated in $SET and then passed to mrgsim
set_args <- c("Req", "obsonly","mtime", "recsort",
              "carry.out","Trequest","trequest")

check_spec_contents <- function(x,crump=TRUE,warn=TRUE,...) {
  invalid <- setdiff(x,block_list)
  valid <- intersect(x,block_list)
  
  if(sum("MAIN"  == x) > 1) stop("Only one $MAIN block allowed in the model.",call.=FALSE)
  if(sum("ODE"   == x) > 1) stop("Only one $ODE block allowed in the model.",call.=FALSE)
  if(sum("SET"   == x) > 1) stop("Only one $SET block allowed in the model.",call.=FALSE)
  
  if(warn) {
    warn_cmt <- length(intersect(c("INIT", "CMT", "VCMT"),x)) == 0
    warn_cmt <- warn_cmt & is.element("ODE",x)
    
    if(warn_cmt)  warning("Could not find a $INIT or $CMT block", call.=FALSE)
    
    if(length(invalid) > 0) {
      warning(paste0("Invalid blocks found: ", paste(invalid, collapse=" ")), call.=FALSE)
    }
  }
  if(length(valid)==0) stop("No valid blocks found.", call.=FALSE)
}

audit_spec <- function(x,spec,warn=TRUE) {
  
  cmt <- names(init(x))
  
  if(!has_name("ODE", spec) | !warn | length(cmt) ==0) {
    return(invisible(NULL))
  }
  
  z <- sapply(paste0("dxdt_",cmt), function(dx) {
    !any(grepl(dx,spec[["ODE"]],fixed=TRUE))
  })
  
  if(any(z)) {
    ans <- paste(cmt[z], collapse=',')
    warning(paste0("Audit: missing differential equation(s) for ", ans), call.=FALSE)
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
  if(length(x)==0) return("// No fixed parameters.")
  if(is.null(fixed_type))  fixed_type <-  "define"
  if(!(fixed_type %in% c("define", "const"))) stop("fixed_type must be either const or define.", call.=FALSE)
  switch(fixed_type,
         `const` =  paste0("const double ", paste0(names(x) ,"= " ,unlist(x), ";")),
         `define` = paste0("#define ", names(x), "  (", define_digits(unlist(x)),")")
  )
}


##' Parse model specification text.
##' @param txt model specification text
##' @param split logical
##' @param drop_blank logical; \code{TRUE} if blank lines are to be dropped
##' @param comment_re regular expression for comments
##' @param ... arguments passed along
##' @export
modelparse <- function(txt, 
                       split=FALSE,
                       drop_blank = TRUE, 
                       comment_re=c("//", "##"),...) {
  
  ## Take in model text and parse it out
  
  if(split) txt <- strsplit(txt,"\n",perl=TRUE)[[1]]
  
  if(drop_blank) txt <- txt[!grepl("^\\s*$",txt)]
  
  # Take out comments
  for(comment in comment_re) {
    m <- as.integer(regexpr(comment,txt,fixed=TRUE))
    w <- m > 0
    txt[w] <- substr(txt[w],1,m[w]-1)
  }
  
  # Look for block lines
  m <- regexec(block_re,txt)
  
  # Where the block starts
  start <- which(sapply(m,"[",1L) > 0)
  
  if(length(start)==0) {
    stop("No model specification file blocks were found.", call.=FALSE)
  }
  
  # Get the matches
  mm <- regmatches(txt[start],m[start])
  
  # Block labels
  labs <- gsub("[][$ ]", "", sapply(mm, "[",1L), perl=TRUE)
  
  # Remove block label text
  txt[start] <- mytriml(substr(txt[start], nchar(unlist(mm,use.names=FALSE))+1, nchar(txt[start])))
  
  # Where the block ends
  end <- c((start-1),length(txt))[-1]
  
  # Create the list
  spec <- lapply(seq_along(start), function(i) {
    y <- txt[start[i]:end[i]]
  })
  
  if(drop_blank) {
    spec <- lapply(spec,function(y) y[y!=""]) 
  }
  
  names(spec) <- labs
  
  return(spec)
  
}

## ----------------------------------------------------------------------------
## New function set for finding double / bool / int
## and moving to global
move_global_re_find <- "\\b(double|int|bool|capture)\\s+\\w+\\s*="
move_global_rcpp_re_find <- "\\bRcpp::(NumericVector|NumericMatrix|CharacterVector)\\s+\\w+\\s*="
move_global_re_sub <-  "\\b(double|int|bool|capture)\\s+(\\w+\\s*=)"
move_global_rcpp_re_sub <-  "\\bRcpp::(NumericVector|NumericMatrix|CharacterVector)\\s+(\\w+\\s*=)"
local_var_typedef <- c("typedef double localdouble;","typedef int localint;","typedef bool localbool;")

move_global <- function(x,env) {
  
  what <- intersect(c("PREAMBLE","MAIN", "ODE", "TABLE"),names(x))
  
  if(length(what)==0) return(x)
  
  # Keep names in here for later
  l <- lapply(x[what], get_c_vars)
  ll <- unlist(l, use.names=FALSE)
  
  env[["global"]] <- c("typedef double capture;",
                       "namespace {",
                       paste0("  ",ll),
                       "}",
                       local_var_typedef)
  
  ll <- cvec_cs(unlist(ll,use.names=FALSE))
  ll <- gsub(";","",ll,fixed=TRUE)
  ll <- setdiff(ll, c("double", "int", "bool", "capture"))
  env[["move_global"]] <- ll
  
  cap <- vector("list")
  
  for(w in what) {
    
    x[[w]] <- gsub(move_global_re_sub,
                   "\\2",x[[w]],perl=TRUE)
    
    # **************************
    # Search for capture 
    wcap <- grepl("capture ", l[[w]], fixed=TRUE)
    
    if(any(wcap)) {
      
      if(w=="ODE") {
        stop("Found capture typed variables in $ODE.\n", 
             "The type should be changed to double.\n",
             call.=FALSE)
      }
      
      ll <- l[[w]][wcap]
      ll <- ll[substr(ll,1,8) == "capture "]
      cap[[w]] <- substr(ll,9,nchar(ll)-1)
    }
    # **************************
    
  } # <-- End for(w in what)
  
  if(length(cap) > 0) {
    # must trim this
    x <- c(x,list(CAPTURE=mytrim(unlist(cap, use.names=FALSE))))
  }
  # **************************
  
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

get_rcpp_vars <- function(y) {
  m <- gregexpr(move_global_rcpp_re_find,y,perl=TRUE)
  regmatches(y,m) %>%
    unlist %>%
    gsub(pattern="\\s*=$",
         replacement=";",
         perl=TRUE)
}

check_block_data <- function(x,env,pos) {
  if(length(x)==0) {
    warning("Block no. ", pos, ": no data was found.", call.=FALSE)
  }
  return(NULL)
}

parse_ats <- function(x) {
  
  if(length(x)==0) return(list())
  
  # Require that line starts with @
  # ls are lists of boolean options on a single ine
  x <- mytrim(unlist(strsplit(x,"@",fixed=TRUE)))
  x <- x[x!=""]
  
  # Name/value lines will have spaces but not lists
  nv <- grepl(" ", x, fixed=TRUE) 
  
  # Boolean are not Name/value
  if(any(!nv)) {
    x[!nv] <- paste0(cvec_cs(x[!nv]), " TRUE")
  }
  
  # find the first space
  sp <- regexpr(" ", x, fixed=TRUE)
  
  # The names
  a <- substr(x, 1,sp-1)
  
  # The values
  b <- substr(x,sp+1,nchar(x))
  
  # Warn if quotes
  if(any(charthere(b,"\"") | charthere(b,"'"))) {
    warning("Found quotation mark in option value.",call.=FALSE) 
  }
  # Convert type
  b <- setNames(lapply(b,type.convert,as.is=TRUE),a)
  
  b
}

##' Scrape options from a code block.
##' 
##' @param x data
##' @param def default values
##' @param all return all options, even those that are not in \code{def}
##' @param marker assignment operator; used to locate lines with options
##' @param narrow logical; if \code{TRUE}, only get options on lines starting with \code{>>}
##' @param envir environment from \code{$ENV}
##' 
##' @return list with elements \code{x} (the data without options) and named options 
##' as specified in the block.
scrape_opts <- function(x,envir=list(),def=list(),all=TRUE,marker="=",narrow=TRUE) {
  
  x <- unlist(strsplit(x, "\n",fixed=TRUE))
  
  ## Get lines starting with >>
  opts <- grepl("^\\s*>>",x,perl=TRUE)
  
  if(!narrow) {
    opts <- opts | grepl(marker,x,fixed=TRUE)
  }
  
  has_at <- grepl("^\\s*@", x, perl=TRUE) 
  at <- parse_ats(x[has_at])
  
  data <- x[!(opts | has_at)]
  
  opts <- c(gsub(">>","", x[opts], fixed=TRUE))
  
  opts <- merge(def, tolist(opts,envir=envir),
                open=all,warn=FALSE,context="opts")
  
  opts <- c(opts,at)
  
  if(any(duplicated(names(opts)))) {
    stop("Found duplicated block option names.", call.=FALSE) 
  }
  
  opts$x <- NULL
  
  c(list(x=data), opts)
}

##' Scrape options and pass to function.
##' 
##' @param x data
##' @param env parse environment
##' @param pass function to call
##' @param ... dots
##' 
##' @details Attributes of \code{x} are also scraped and merged with options.
##' 
scrape_and_call <- function(x,env,pass,...) {
  o <- scrape_opts(x,envir=env$ENV,...)
  o$pos <- o$env <- o$class <- NULL
  o <- c(o,attributes(x),list(env=env))
  do.call(pass,o)
}

dump_opts <- function(x,env,block,...) {
  hasopt <- unique(c(grep(">>", x, fixed=TRUE),grep("@", x, fixed=TRUE))) 
  if(length(hasopt)==0) return(x)
  hasopt <- grep("^\\s*(>>|@)", x[hasopt], perl=TRUE)
  x[-hasopt]
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

parseLIST <- function(x,where,env,...) {
  env[[where]][[attr(x,"pos")]] <- tolist(x)
  return(NULL)
}

## Used to parse OMEGA and SIGMA matrix blocks
specMATRIX <- function(x,
                       oclass,type, annotated = FALSE,
                       env, pos=1,
                       name="...", prefix="", labels=NULL,
                       object=NULL,unlinked=FALSE,...) {
  
  if(is.null(object)) check_block_data(x,env$ENV,pos)
  
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
      stop("Ambigious or mixed annotations in ",paste0("$",toupper(type)),call.=FALSE) 
    }
    
    l <- parse_annot(x[anl],name_value=FALSE,
                     block=toupper(type),
                     envir=env$ENV,novalue=novalue)
    
    if(unlinked) {
      l[["v"]] <- as.numeric(cvec_cs(x[!anl])) 
    }
    
    d <- modMATRIX(l[["v"]],context=oclass,...)
    labels <- l[["an"]][["name"]]
    env[["annot"]][[pos]] <- l[["an"]]
    
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

##' @export
handle_spec_block.specOMEGA <- function(x,...) {
  scrape_and_call(x,
                  pass="specMATRIX",
                  def=list(oclass="omegalist",type="omega"),
                  narrow=FALSE,...)
}

##' @export
handle_spec_block.specSIGMA <- function(x,...) {
  scrape_and_call(x,
                  pass="specMATRIX",
                  def=list(oclass="sigmalist",type="sigma"),
                  narrow=FALSE,...)
  
}

eval_ENV_block <- function(x,where,envir=new.env(),...) {
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(where)
  if(is.null(x)) return(envir)
  .x <- try(eval(parse(text=x),envir=envir))
  if(inherits(.x,"try-error")) {
    stop("Failed to parse code in $ENV",call.=FALSE) 
  }
  envir$.code <- x
  return(envir)
}  

## S3 methods for processing code blocks
## All of these need to be exported
handle_spec_block <- function(x,...) UseMethod("handle_spec_block")
##' @export
handle_spec_block.default <- function(x,...) {
  return(dump_opts(x))
}

##' @export 
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

##' Functions to parse code blocks.
##' 
##' Most of the basic blocks are listed in this help topic.  
##' But see also \code{\link{PKMODEL}} which has more-involved 
##' options and is documented separately.
##' 
##' @param x data
##' @param env parse environment
##' @param annotated logical
##' @param name block name
##' @param pos block position
##' @param ... passed
##' 
##' @rdname BLOCK_PARSE
##' @name BLOCK_PARSE
##' 
##' @seealso \code{\link{PKMODEL}}
NULL

##' @rdname BLOCK_PARSE
PARAM <- function(x,env,annotated=FALSE,pos=1,...) {
  
  check_block_data(x,env$ENV,pos)
  
  if(annotated) {
    l <- parse_annot(x,block="PARAM",envir=env$ENV)
    env[["param"]][[pos]] <- l[["v"]]
    env[["annot"]][[pos]] <- l[["an"]]
    
  } else {
    x <- tolist(x,envir=env$ENV) 
    env[["param"]][[pos]] <- x
  }
  
  return(NULL)
}

##' @export
handle_spec_block.specPARAM <- function(x,...) {
  scrape_and_call(x,pass="PARAM",...)
}

##' @rdname BLOCK_PARSE
FIXED <- function(x,env,annotated=FALSE,pos=1,...) {
  
  check_block_data(x,env$ENV,pos)
  
  if(annotated) {
    l <- parse_annot(x,block="FIXED",envir=env$ENV)
    env[["fixed"]][[pos]] <- l[["v"]]
    env[["annot"]][[pos]] <- l[["an"]]
  } else {
    x <- tolist(x,envir=env$ENV) 
    env[["fixed"]][[pos]] <- x
  }
  
  return(NULL)
}

##' @export
handle_spec_block.specFIXED <- function(x,...) {
  scrape_and_call(x,pass="FIXED",...)
}


##' @rdname BLOCK_PARSE
THETA <- function(x,env,annotated=FALSE,pos=1,
                  name="THETA",...) {
  
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

##' @export
handle_spec_block.specTHETA <- function(x,...) {
  scrape_and_call(x,pass="THETA",narrow=FALSE,...)
}

##' @rdname BLOCK_PARSE
INIT <- function(x,env,annotated=FALSE,pos=1,...) {
  
  check_block_data(x,env$ENV,pos)
  
  if(annotated) {
    l <- parse_annot(x,block="INIT",envir=env$ENV)
    env[["init"]][[pos]] <- l[["v"]]
    env[["annot"]][[pos]] <- l[["an"]]
  } else {
    x <- tolist(x,envir=env$ENV) 
    env[["init"]][[pos]] <- x
  }
  
  return(NULL)
}

##' @export
handle_spec_block.specINIT <- function(x,...) {
  scrape_and_call(x,pass="INIT",...)
}

##' @rdname BLOCK_PARSE
CMT <- function(x,env,annotated=FALSE,pos=1,...) {
  
  check_block_data(x,env$ENV,pos)
  
  if(annotated) {
    l <- parse_annot(x,novalue=TRUE,block="CMT",envir=env$ENV)
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

##' @export
handle_spec_block.specCMT <- function(x,...) {
  scrape_and_call(x,pass="CMT",narrow=FALSE,...)
}
##' @export
handle_spec_block.specVCMT <- handle_spec_block.specCMT

##' @export
handle_spec_block.specSET <- function(x,...) {
  tolist(dump_opts(x))
}

##' @export
handle_spec_block.specNMXML <- function(x,...) {
  parseNMXML(dump_opts(x),...)
}

##' @export
handle_spec_block.specCMTN <- function(x,...) {
  cvec_cs(dump_opts(x))
}

##' @rdname BLOCK_PARSE
CAPTURE <- function(x,env,annotated=FALSE,pos=1,...) {
  
  if(annotated) {
    l <- parse_annot(x,novalue=TRUE,block="CAPTURE",envir=env$ENV)
    env[["annot"]][[pos]] <- l[["an"]]
    x <- names(l[["v"]])
  } else {
    x <- cvec_cs(x)
  }
  
  check_block_data(x,env$ENV,pos)
  
  env[["capture"]][[pos]] <- x
  
  return(NULL)
}

##' @export
handle_spec_block.specCAPTURE <- function(x,...) {
  scrape_and_call(x,pass="CAPTURE",narrow=FALSE,...)
}

##' @export
handle_spec_block.specPKMODEL <- function(x,env,...) {
  x <- scrape_opts(x, narrow=FALSE)
  x$env <- env
  x$pos <-  attr(x,"pos")
  do.call("PKMODEL",x)
}

##' @export
handle_spec_block.specINCLUDE <- function(x,env,...) {
  
  x <- cvec_c_tr(dump_opts(x))
  
  pos <- attr(x, "pos")
  
  check_block_data(x,env$ENV,pos)
  
  if(any(grepl("[\"\']",x,perl=TRUE))) {
    stop("Items in $INCLUDE should not contain quotation marks.",call.=FALSE) 
  }
  
  if(any(!grepl("^.*\\.h$",x,perl=TRUE))) {
    warning("$INCLUDE expects file names ending with .h",call.=FALSE) 
  }
  
  return(x)
}

form_includes <- function(x,where) {
  if(is.null(x)) return("// No includes found.")
  files <- file.path(where,x)
  if(!all(file_exists(files))) {
    stop("All header files in $INCLUDE must exist in the project directory",call.=FALSE) 
  }
  md <- tools::md5sum(file.path(where,x))
  paste0("#include \"", x, "\" // ", md)
}

##' @export
handle_spec_block.specPLUGIN <- function(x,env,...) {
  
  x <- unique(cvec_c_tr(x))
  
  pos <- attr(x,"pos")
  
  check_block_data(x,env$ENV,pos)
  
  if(length(x) ==0) return(list())
  return(x)
}

##' Parse PKMODEL BLOCK data.
##' @param cmt compartment names as comma-delimited character
##' @param ncmt number of compartments; must be 1 (one-compartment, not including a depot dosing compartment) or 2 (two-compartment model, not including a depot dosing compartment)
##' @param depot logical indicating whether to add depot compartment
##' @param trans the parameterization for the PK model; must be 1, 2, 4, or 11
##' @param env parse environment
##' @param pos block position number
##' @param ... not used
##'
##' @details
##' When using \code{$PKMODEL}, certain symbols must be defined in the model specification depending
##' on the value of \code{ncmt}, \code{depot} and \code{trans}.
##'
##' \itemize{
##' \item \code{ncmt} 1, \code{depot FALSE}, trans 2: \code{CL}, \code{V}
##' \item \code{ncmt} 1, \code{depot TRUE} , trans 2: \code{CL}, \code{V},  \code{KA}
##' \item \code{ncmt} 2, \code{depot FALSE}, trans 4: \code{CL}, \code{V1}, \code{Q}, \code{V2}
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
##' @seealso \code{\link{BLOCK_PARSE}}
PKMODEL <- function(ncmt=1,depot=FALSE,cmt=NULL, trans = pick_trans(ncmt,depot),env=list(),pos=1,...) {
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


NAMESPACE <- function(x,env,name,unnamed=FALSE,pos=1,...) {
  if(unnamed) name <-  NULL
  env[["namespace"]][[pos]] <- wrap_namespace(x,name)
  return(NULL)
}

##' @export
handle_spec_block.specNAMESPACE <- function(x,...) {
  scrape_and_call(x,pass="NAMESPACE",narrow=FALSE,...)
}


## Collect PKMODEL information; hopefully will be deprecating ADVAN2 and ADVAN4 soon
collect_subr <- function(x,what=c("PKMODEL")) {
  
  ans <- list(advan=13,trans=1)
  
  y <- x[names(x) %in% what]
  
  if(length(y) >  1) stop("Only one $PKMODEL block is allowed.",call.=FALSE)
  if(length(y) == 0) return(ans)
  ## Get rid of this once ADVANn are deprecated
  if(names(y) %in% c("PKMODEL")) {
    ans <- y[[1]]
  }
  
  if(ans[["advan"]] != 13) {
    if(any(is.element(c("VCMT"),names(x)))) stop("Found $VCMT and $PKMODEL in the same control stream.")
    if(any(is.element("ODE", names(x)))) stop("Found $ODE and $PKMODEL in the same control stream.")
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


parse_env <- function(n,ENV=new.env()) {
  mread.env <- new.env()
  mread.env$param <- vector("list",n)
  mread.env$fixed <- vector("list",n)
  mread.env$init  <- vector("list",n)
  mread.env$omega <- vector("list",n)
  mread.env$sigma <- vector("list",n)
  mread.env$annot <- vector("list",n)
  mread.env$namespace <- vector("list", n)
  mread.env$capture <- vector("list",n)
  mread.env$error <- character(0)
  mread.env$ENV <- ENV 
  mread.env
}


expand_seq <- function(ex){
  matches <- unlist(regmatches(ex,
                               regexec("(\\w+?)(\\d+):(\\d+):(\\d+)",
                                       ex)
  ), use.names = F)
  return(paste0(matches[[2]],
                seq(as.numeric(matches[[3]]),
                    as.numeric(matches[[4]]),
                    as.numeric(matches[[5]])
                )
  )
  )
}

expand <- function(ex){
  matches <- unlist(regmatches(ex,
                               regexec("(\\w+?)(\\d+):(\\d+)",
                                       ex)
  ), use.names = F)
  return(paste0(matches[[2]],
                as.numeric(matches[[3]]):as.numeric(matches[[4]])
  )
  )
}

expand_maybe <- function(ex){
  colons <- charcount(ex, ":")
  if (colons) {
    if(colons == 2) {
      return(expand_seq(ex))
    } else {
      return(expand(ex) )
    }
  }
  return(ex)
  
}

deparens <- function(x,what=c(")", "(")) {
  for(w in what) {
    x <- gsub(w,"",x,fixed=TRUE) 
  }
  return(x)
}

wrap_namespace <- function(x,name) {
  paste(c(paste0("namespace ", name, " {"),paste("  ",x),"}"), collapse="\n")
}

# For captured items, copy annotation
capture_param <- function(annot,.capture) {
  
  .capture <- as.character(.capture)
  
  if(nrow(annot)==0 | length(.capture)==0) {
    return(annot) 
  }
  
  # only if we didn't already include an annotation
  .capture <- setdiff(.capture,annot[annot[,"block"]=="CAPTURE","name"])
  
  # captured parameters
  what <- dplyr::filter(annot, name %in% .capture & block=="PARAM")
  .capture <- intersect(.capture,what[,"name"])
  what <- dplyr::mutate(what, block="CAPTURE")
  annot <- dplyr::filter(annot, !(block=="CAPTURE" & name %in% .capture))
  bind_rows(annot,what)
}

##' @export
handle_spec_block.specCOVSET <- function(x,...) {
  stop("$COVSET is not a valid block.")
  # require_covset()
  # return(x)
}

handle_cov <- function(spec,envir) {
  return(invisible(NULL))
  # where <- which(names(spec)=="COVSET")
  # value <- vector(mode="list",length=length(where))
  # x <- vector(mode="character",length=length(where))
  # 
  # for(i in seq_along(where)) {
  #   y <- scrape_opts(spec[[where[i]]])
  #   xx <- lapply(y$x,dmutate::new_covobj)
  #   value[[i]] <- do.call(dmutate::covset,xx)
  #   if(is.null(y$name)) {
  #     stop("All $COVSET blocks must have name block option set (e.g. $COVSET @name cov1)",call.=FALSE) 
  #   }
  #   x[i] <- y$name
  # }
  # 
  # for(i in seq_along(x)) {
  #   if(exists(x[i],envir)) {
  #     stop("Can't assign covset ", 
  #          x[i], 
  #          ": an object already exists with that name",call.=FALSE) 
  #   }
  #   assign(x[i],value[[i]],envir=envir)  
  # }
  # return(invisible(NULL))
}


