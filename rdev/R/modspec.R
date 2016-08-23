## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

##' @include utils.R complog.R nmxml.R matrix.R annot.R

globalre2 <- "^\\s*(predpk|double|bool|int)\\s+\\w+"
block_re <-  "^\\s*(\\$([A-Z]\\w*)|\\[\\s*([A-Z]\\w*)\\s*])(.*)"

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
  if(length(x)==0) return("// No fixed parameters.")
  if(is.null(fixed_type))  fixed_type <-  "define"
  if(!(fixed_type %in% c("define", "const"))) stop("fixed_type must be either const or define.", call.=FALSE)
  switch(fixed_type,
         `const` =  paste0("const double ", paste0(names(x) ,"= " ,unlist(x), ";")),
         `define` = paste0("#define ", names(x), "  (", define_digits(unlist(x)),")")
  )
}

## A random file name
so_stem <- function(x) paste0(x,"-so-")

rfile <- function(pattern="",tmpdir=normalizePath(getwd(),winslash="/")){
  basename(tempfile(pattern=so_stem(pattern),tmpdir='.'))
}

## Form a file name / path for the file that is actually compiled
comppart <- "-mread-source"
compbase <- function(model) paste0(model, comppart)
compfile <- function(model) paste0(model, comppart,".cpp")
compout  <- function(model) paste0(model, comppart, .Platform$dynlib.ext)
compdir <- function() {
  paste(c("mrgsolve",
          "so",
          as.character(GLOBALS[["version"]]),
          R.version$platform),
        collapse="-")
}

setup_soloc <- function(loc,model) {
  soloc <- file.path(loc,compdir(),model)
  if(!file.exists(soloc)) dir.create(soloc,recursive=TRUE)
  return(soloc)
}



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



## These only really work in code blocks
opts_only <- function(x,def=list(),all=FALSE) {
  opts <- scrape_opts(x)
  merge(def,opts, strict=!all,warn=FALSE,context="opts")
}
scrape_opts <- function(x,def=list(),all=FALSE,marker="=>?",split=TRUE) {
  x <- unlist(strsplit(x, "\n",perl=TRUE))
  opts <- grepl(marker,x,perl=TRUE)
  if(split) {
    data <- unlist(strsplit(x[!opts],"\\s+",perl=TRUE))
  } else {
    data <- x[!opts] 
  }
  if(any(grepl("=>",x[opts],fixed=TRUE))) {
    x[opts] <- gsub("=>", "=",x[opts])
  }
  opts <- merge(def, tolist(x[opts]),strict=!all,warn=FALSE,context="opts")
  c(list(x=data), opts)
}
scrape_and_pass <- function(x,pass,...) {
  o <- scrape_opts(x,...)
  ret <- do.call(pass,o)
  list(opts=o,data=ret)
}

## Functions for handling code blocks
parseNMXML <- function(x,env,...) {
  pos <- attr(x,"pos")
  x <- tolist(x)
  xml <- do.call(nmxml,x)
  env[["param"]][[pos]] <- xml$theta
  env[["omega"]][[pos]] <- xml$omega
  env[["sigma"]][[pos]] <- xml$sigma
  return(NULL)
}

parseTHETA <- function(x,env,...) {
  pos <- attr(x,"pos")
  opts <- scrape_opts(x,def=list(annotated=FALSE),all=TRUE,split=FALSE)
  if(opts[["annotated"]]) {
    l <- parse_annot(opts[["x"]],noname=TRUE)
    x <- as.numeric(l[["v"]])
  } else {
    x <- as.numeric(as.cvec(opts$x))
  }
  
  x <- x[!is.na(x)]
  if(!exists("name", opts)) opts$name <- "THETA"
  names(x) <- paste0(opts$name, 1:length(x))
  env[["param"]][[pos]] <- x
  return(NULL)
}

parseLIST <- function(x,where,env,...) {
  env[[where]][[attr(x,"pos")]] <- tolist(x)
  return(NULL)
}

parseINIT <- function(x,env,...) {
  y <- scrape_opts(x,
                   def=list(annotated=FALSE),
                   marker="=>", all=TRUE,split=FALSE) 
  if(y[["annotated"]]) {
    l <- parse_annot(y[["x"]])
    env[["init"]][[attr(x,"pos")]] <- l[["v"]]
    return(NULL)
  } 
  parseLIST(x,"init",env,...)  
  
}


parsePARAM <- function(x,env,...) {
  
  y <- scrape_opts(x,
                   def=list(annotated=FALSE),
                   marker="=>", all=TRUE,split=FALSE) 
  if(y[["annotated"]]) {
    l <- parse_annot(y[["x"]])
    env[["param"]][[attr(x,"pos")]] <- l[["v"]]
    return(NULL)
  } 
  parseLIST(x,"param",env,...)
}

parseCMT <- function(x,env,...) {
  
  pos <- attr(x,"pos")
  y <- scrape_opts(x,def=list(annotated=FALSE),
                   marker="=>", all=TRUE,split=FALSE)
  if(y[["annotated"]]) {
    l <- parse_annot(y[["x"]],novalue=TRUE)
    x <- names(l[["v"]])
  } 
  x <- tovec(x)
  l <- rep(0,length(x))
  names(l) <- x
  env[["init"]][[pos]] <- as.list(l)
  return(NULL)
}


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

specOMEGA <- function(x,env,...) {
  m <- specMATRIX(x,"omega_block")
  env[["omega"]][[attr(x,"pos")]] <- m
  return(NULL)
}
specSIGMA <- function(x,env,...) {
  m <- specMATRIX(x,"sigma_block")
  env[["sigma"]][[attr(x,"pos")]] <- m
  return(NULL)
}


## S3 methods for processing code blocks
## All of these need to be exported
handle_spec_block <- function(x,...) UseMethod("handle_spec_block")
##' @export
handle_spec_block.default <- function(x,...) return(x)
##' @export
handle_spec_block.specPARAM <- function(x,...) parsePARAM(x,...)
##' @export
handle_spec_block.specINIT <- function(x,...) parseINIT(x,...)
##' @export
handle_spec_block.specCMT <- function(x,...) parseCMT(x,...)
##' @export
handle_spec_block.specSET <- function(x,...) tolist(x)
##' @export
handle_spec_block.specENV <- function(x,...) tolist(x)
##' @export
handle_spec_block.specOMEGA <- function(x,...) specOMEGA(x,...)
##' @export
handle_spec_block.specSIGMA <- function(x,...) specSIGMA(x,...)
##' @export
handle_spec_block.specNMXML <- function(x,...) parseNMXML(x,...)
##' @export
handle_spec_block.specTHETA <- function(x,...) parseTHETA(x,...)
##' @export
handle_spec_block.specCMTN <- function(x,...) as.cvec(x)
##' @export
handle_spec_block.specFIXED <- function(x,...) parseLIST(x,"fixed",...)
##' @export
handle_spec_block.specCAPTURE <- function(x,...) as.cvec(x)
##' @export
handle_spec_block.specVCMT <- function(x,...) parseCMT(x,...)
##' @export
handle_spec_block.specPKMODEL <- function(x,...) {
  x <- scrape_opts(x,all=TRUE)
  do.call("PKMODEL",x)
}

##' @export
handle_spec_block.specINCLUDE <- function(x,...) {
  
  x <- as.cvec2(x)
  if(any(grepl("[\"\']",x,perl=TRUE))) {
    stop("Items in $INCLUDE should not contain quotation marks.",call.=FALSE) 
  }
  if(any(!grepl("^.*\\.h$",x,perl=TRUE))) {
    warning("$INCLUDE expects file names ending with .h",call.=FALSE) 
  }
  return(x)
}

form_includes <- function(x,where) {
  if(is.null(x)) return(NULL)
  files <- file.path(where,x)
  if(!all(file.exists(files))) {
    stop("All header files in $INCLUDE must exist in the project directory",call.=FALSE) 
  }
  md <- tools::md5sum(file.path(where,x))
  paste0("#include \"", x, "\" // ", md)
}


##' @export
handle_spec_block.specPLUGIN <- function(x,...) {
  x <- unique(as.cvec(x))
  if("mrgx" %in% x) {
    warning("There are currently no functions provided by the mrgx plugin. All functions previously provided by mrgx can be called from the R namespace (e.g. R::rnorm(10,2)).", call.=FALSE)
  }
  if(length(x) ==0) return(list())
  return(x)
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
# collect_matrix <- function(x,what,class,xmlname) {
#   what <- c(what, "NMXMLDATA")
#   
#   x <- x[sapply(x,inherits,what)]
#   
#   xmli <- unlist(sapply(x,inherits, "NMXMLDATA"))
#   
#   x[xmli] <- unname(lapply(x[xmli], function(x) x[[xmlname]]))
#   
#   if(length(x)==0) return(create_matlist(class=class))
#   
#   nr <- sapply(x,function(y) nrow(y[["data"]]))
#   x <- x[nr > 0]
#   
#   d <- lapply(x,function(y) y[["data"]])
#   l <- lapply(x,function(y) y[["labels"]])
#   NAMES <- lapply(x,function(y) y[["name"]]) %>% unlist %>% unname
#   names(d) <- NAMES
#   names(l) <- NAMES
#   x <- create_matlist(x=d, class=class, labels=l)
#   return(x)
#   
# }
# collect_omat <- function(x,what=c("omega_block")) collect_matrix(x,what,"omegalist", "omega")
# collect_smat <- function(x,what=c("sigma_block")) collect_matrix(x,what,"sigmalist","sigma")

collect_matlist <- function(x, class) {
  x <- x[!sapply(x,is.null)]
  if(length(x)==0) return(create_matlist(class=class))
  d <- lapply(x,"[[","data")
  l <- lapply(x, "[[", "labels")
  NAMES <- lapply(x,"[[", "name") %>% unlist %>% unname
  names(d) <- names(l) <- NAMES
  create_matlist(x=d,class=class,labels=l)
}


## May have multiple $FIXED
# collect_fixed <- function(x, what=c("fixed_list")) {
#   
#   x <- x[grepl("FIXED",names(x),perl=TRUE)]
#   
#   names(x) <- NULL
#   
#   x <- do.call("c",x)
#   
#   if(length(x)>0) return(x)
#   
#   return(list())
# }

## May have multiple $PARAM blocks; also needs to collect from $NMXML
# collect_param <- function(x, what=c("parameter_list")) {
#   
#   what <- c(what, "NMXMLDATA")
#   
#   x <- x[sapply(x,inherits,what)]
#   
#   xmli <- unlist(sapply(x,inherits,"NMXMLDATA"))
#   
#   x[xmli] <- lapply(x[xmli], function(x) x$theta)
#   
#   names(x) <- NULL
#   
#   x <- unlist(lapply(x, as.numeric))
#   
#   if(length(x)>0) return(as.param(x))
#   
#   return(as.param(list()))
# }
# 
## Merges code from $TABLE and $CAPTURE
# collect_table <- function(x,what=c("TABLE")) {
#   x <- x[names(x) %in% what]
#   unname(unlist(x))
# }

## Look for initial conditions in $INIT, $CMT, and $VCMT
# collect_init <- function(x,what=c("INIT", "CMT", "VCMT")) {
#   x <- x[names(x) %in% what]
#   if(length(x)==0) return(as.init(list()))
#   names(x) <- NULL
#   x <- do.call("c",x)
#   return(as.init(x))
# }

## Collect PKMODEL information; hopefully will be deprecating ADVAN2 and ADVAN4 soon
collect_subr <- function(x,what=c("PKMODEL")) {
  
  ans <- list(advan=13,trans=1,strict=FALSE)
  
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


