# Copyright (C) 2013 - 2023  Metrum Research Group
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

#globalre2 <- "^\\s*(predpk|double|bool|int)\\s+\\w+"
block_re <-  "^\\s*\\$[A-Za-z]\\w*|^\\s*\\[+\\s*[a-zA-Z]\\w*\\s*\\]+"

## Generate an advan/trans directive
advtr <- function(advan,trans) {
  if(advan==13 | trans %in% c(0,1)) return(NULL)
  if((advan %in% c(1,2)) & !(trans %in% c(2,11))) {
    stop("ADVAN 1 and 2 can only use trans 1, 2, or 11", call. = FALSE)
  }
  if((advan %in% c(3,4)) & !(trans %in% c(4,11))) {
    stop("ADVAN 3 and 4 can only use trans 1, 4, or 11", call. = FALSE)
  }
  return(paste0("__ADVAN", advan, "_TRANS", trans, "__"))
}

write_capture <- function(x) {
  if(length(x) == 0) return(NULL)
  i <- seq_along(x)
  paste0("_capture_[",i-1,"] = ", x[i], ";") 
}

#' This function adds ETA values to the capture list when the `@etas` 
#' option is used in `$CAPTURE` .
#' 
#' @param x the model object
#' @param env the mread parse environment
#' 
#' @details
#' We look at env$capture_etas to see if there were any expression text passed
#' through the `@etas` option. This text will be parsed and evaluated in the 
#' model environment after adding `LAST` and `last` to represent the last
#' ETA (or the [total] number of rows in `$OMEGA`).
#' 
#' An error is generated in case the expression can't be parsed and evaluated.
#' 
#' `@etas` must resolve to an integer-like object. Expecting most usage to be
#' `1:last` which will be integer, but we want to support `c(1,2,5)` as well
#' which will not be integer. 
#'
#' @return The model object, possibly updated.   
#' 
#' @noRd
capture_etas <- function(x, env) {
  if(!is.character(env[["capture_etas"]])) return(x)
  last <- sum(nrow(omat(x)))
  if(last==0) return(x)
  parse_env <- as.list(env$ENV)
  parse_env$last <- parse_env$LAST <- last
  for(eta_txt in env[["capture_etas"]]) {
    etan <- try(eval(parse(text = eta_txt), envir = parse_env), silent = TRUE)
    if(inherits(etan, "try-error")) {
      msg <- c(
        glue("could not parse this expression for `etas`: {eta_txt}."), 
        x = etan
      )
      abort(msg)
    }
    resolves_int <- is.numeric(etan) && all.equal(etan, round(etan))
    if(!resolves_int) {
      abort("`etas` must resolve to an integer value.")    
    }
    if(length(etan)==0) {
      abort("`etas` has length 0.")  
    }
    etan <- unique(as.integer(round(etan)))
    if(any(etan < 1 | etan > last)) {
      abort(
        message = c(
          glue("`etas` must be integers between 1 and {last}."),
          i = glue("minimum value in `etas`: {min(etan)}"),
          i = glue("maximum value in `etas`: {max(etan)}")
        )
      )
    }
    old <- paste0("ETA(", etan, ")")
    new <- paste0("ETA", etan)
    x <- update_capture(x, .ren.chr(.ren.create(old, new)))
  }
  x
}

## These are arguments to mrgsim that
## can be stated in $SET and then passed to mrgsim
set_args <- c(
  "Req", "obsonly", "recsort",
  "carry.out","Trequest","trequest", 
  "carry_out", "Request"
)

set_simargs <- function(x, SET) {
  simargs <- SET[is.element(names(SET), set_args)]
  if(length(simargs) > 0) {
    x@args <- combine_list(x@args, simargs)
  }   
  x
}

check_pkmodel <- function(x, subr, spec) {
  # ADVAN 13 is the ODEs
  # Two compartments for ADVAN 2, 3 compartments for ADVAN 4
  # Check $MAIN for the proper symbols
  if(x@advan %in% c(1,2,3,4)) {
    if(subr[["n"]] != neq(x)) {
      stop("$PKMODEL requires  ", subr[["n"]] , 
           " compartments in $CMT or $INIT.", call. = FALSE)
    }
    check_pred_symbols(x, spec[["MAIN"]])
  }
  return(invisible(NULL))
}

check_sim_eta_eps_n <- function(x, spec) {
  if(isFALSE(env_get_env(x)$MRGSOLVE_RESIM_N_WARN)) {
    return(invisible(NULL))  
  }
  main <- spec[["MAIN"]]
  tab <- spec[["TABLE"]]
  simeta_n <- grep("\\bsimeta\\(\\s*[0-9]+\\s*\\)", main, perl = TRUE)
  simeps_n <- grep("\\bsimeps\\(\\s*[0-9]+\\s*\\)", tab,  perl = TRUE)
  if(length(simeta_n) > 0) {
    warning(
      "simeta(n) was requested; ", 
      "resimulating single ETA values is now discouraged and will soon be deprecated; ", 
      "use simeta() to resimulate all ETA; ",
      "silence this warning by setting MRGSOLVE_RESIM_N_WARN to FALSE in $ENV."
    )
  }
  if(length(simeps_n) > 0) {
    warning(
      "simeps(n) was requested; ", 
      "resimulating single EPS values is now discouraged and will soon be deprecated; ", 
      "use simeps() to resimulate all EPS; ",
      "silence this warning by setting MRGSOLVE_RESIM_N_WARN to FALSE in $ENV."
    )
  }
  return(invisible(NULL))
}

check_spec_contents <- function(x, crump = TRUE, warn = TRUE, ...) {
  invalid <- setdiff(x,block_list)
  valid <- intersect(x,block_list)
  
  if(sum("MAIN"  == x) > 1){
    stop("Only one $MAIN block allowed in the model.",call.=FALSE)
  }
  
  if(sum("SET" == x) > 1) {
    stop("Only one $SET block allowed in the model.", call.=FALSE)
  }
  
  if(warn) {
    warn_cmt <- length(intersect(c("INIT", "CMT", "VCMT"),x)) == 0
    warn_cmt <- warn_cmt & is.element("ODE",x)
    
    if(warn_cmt)  {
      warning(
        "Could not find a $INIT or $CMT block.", 
        call.=FALSE, immediate. = TRUE
      )
    }
    
    if(length(invalid) > 0) {
      warning(
        paste0(
          "invalid blocks found: ", 
          paste(invalid, collapse = " ")
        ), 
        call. = FALSE, immediate. = TRUE
      )
    }
  }
  
  if(length(valid)==0) stop("No valid blocks found.", call.=FALSE)
  
  return(invisible(NULL))
}

audit_spec <- function(x, spec, warn = TRUE) {
  cmt <- names(init(x))
  if(!has_name("ODE", spec) | !warn | length(cmt) ==0) {
    return(invisible(NULL))
  }
  z <- sapply(paste0("dxdt_",cmt), function(dx) {
    !any(grepl(dx, spec[["ODE"]], fixed = TRUE))
  })
  if(any(z)) {
    bad <- cmt[z]
    err <- "Missing differential equation(s):"
    for(b in bad) {
      err <- c(err, paste0("--| missing: ", b))  
    }
    err <- c(err, "--| suppress with @!audit block option")
    warning(paste0(err, collapse = "\n"), call.=FALSE)
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
  if(length(x)==0) {
    return("// No fixed parameters.")
  }
  if(is.null(fixed_type)) {
    fixed_type <-  "define"
  }
  if(!(fixed_type %in% c("define", "const"))) {
    stop("fixed_type must be either const or define.", call.=FALSE)
  }
  switch(fixed_type,
         `const` =  paste0("const double ", paste0(names(x) ,"= " ,unlist(x), ";")),
         `define` = paste0("#define ", names(x), "  (", define_digits(unlist(x)),")")
  )
}


##' Parse model specification text
##' @param txt model specification text
##' @param split logical
##' @param drop_blank logical; \code{TRUE} if blank lines are to be dropped
##' @param comment_re regular expression for comments
##' @examples
##' file <- file.path(modlib(), "pk1.cpp")
##' 
##' modelparse(readLines(file))
##' 
##' @export
##' @keywords internal
modelparse <- function(txt, split=FALSE, drop_blank = TRUE, 
                       comment_re=c("//", "##")) {
  
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
  
  for(i in which(toupper(names(spec)) %in% c("PARAM", "CMT", "INIT", "CAPTURE"))) {
    spec[[i]] <- gsub("; *$", "", spec[[i]])  
  }
  
  return(spec)
  
}

#' @rdname modelparse
#' @keywords internal
#' @export
modelparse_rmd <- function(txt, split=FALSE, drop_blank=TRUE, 
                           comment_re = "//") {
  
  if(split) txt <- strsplit(txt,"\n",perl=TRUE)[[1]]
  
  if(drop_blank) txt <- txt[!grepl("^\\s*$",txt)]
  
  for(comment in comment_re) {
    m <- as.integer(regexpr(comment,txt,fixed=TRUE))
    w <- m > 0
    txt[w] <- substr(txt[w],1,m[w]-1)
  }
  start_re <- "^```\\{.*\\}\\s*"
  end_re <- "^\\s*```\\s*$"
  start <- grep(start_re,txt)
  end <- grep(end_re,txt)
  ans <- vector("list", length(start))
  for(i in seq_along(start)) {
    ans[[i]] <- txt[seq(start[i],end[i])]
    ans[[i]] <- gsub("^```\\{\\s*(r|c) +", "\\{", ans[[i]])
    ans[[i]] <- sub("^```\\{", "\\{", ans[[i]])
    ans[[i]] <- sub("```", "", ans[[i]])
  }
  chunk <- sapply(ans, "[[", 1)
  lab <- gsub("\\{|\\}", "",chunk) %>% trimws
  sp <- strsplit(lab, "\\s+|\\,")
  label <- sapply(sp, "[", 1L)
  label <- strsplit(label, "-", fixed = TRUE)
  label <- sapply(label, "[",1L)
  #opts <- lapply(sp, "[", -1L)
  for(i in seq_along(label)) {
    ans[[i]] <- ans[[i]][-length(ans[[i]])]
    ans[[i]] <- ans[[i]][-1]
  }
  names(ans) <- toupper(label)
  dropR <- names(ans)=="R"
  if(any(dropR)) {
    ans <- ans[!dropR]  
  }
  return(ans)
}



## ----------------------------------------------------------------------------
## New function set for finding double / bool / int
## and moving to global
move_global_re_find <- "\\b(double|int|bool|capture)\\s+\\w+\\s*="
move_global_rcpp_re_find <- "\\bRcpp::(NumericVector|NumericMatrix|CharacterVector)\\s+\\w+\\s*="
move_global_re_sub <-  "\\b(double|int|bool|capture)\\s+(\\w+\\s*=)"
move_global_rcpp_re_sub <-  "\\bRcpp::(NumericVector|NumericMatrix|CharacterVector)\\s+(\\w+\\s*=)"
#local_var_typedef <- c("typedef double localdouble;","typedef int localint;","typedef bool localbool;")
param_re_find <- "\\bparam\\s+\\w+\\s*="

# please-deprecate
move_global <- function(x,env) {
  
  what <- intersect(c("PREAMBLE","MAIN", "ODE", "TABLE", "PRED"),names(x))
  
  if(length(what)==0) return(x)
  
  # Keep names in here for later
  l <- lapply(x[what], get_c_vars)
  ll <- unlist(l, use.names=FALSE)
  
  env[["global"]] <- c("typedef double capture;",
                       "namespace {",
                       paste0("  ",ll),
                       "}")
  
  ll <- cvec_cs(unlist(ll,use.names=FALSE))
  ll <- gsub(";","",ll,fixed=TRUE)
  ll <- setdiff(ll, c("double", "int", "bool", "capture"))
  env[["move_global"]] <- ll
  
  cap <- vector("list")
  
  for(w in what) {
    
    x[[w]] <- gsub(move_global_re_sub, "\\2",x[[w]],perl=TRUE)
    
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
  regm <- unlist(regmatches(y,m))
  gsub(pattern="\\s*=$", replacement = ";", x = regm, perl=TRUE)
}

token_space <- function(x) {
  x <- strsplit(x, " ",fixed=TRUE)
  lapply(x, function(xx) xx[xx!=""])
}

get_c_vars2 <- function(y,context) {
  m <- gregexpr(move_global_re_find,y,perl=TRUE)
  regm <- unlist(regmatches(y,m))
  if(length(regm)==0) return(data.frame())
  vars <- gsub(pattern="\\s*=$", replacement = "", x = regm, perl=TRUE)
  vars <- token_space(vars)
  ans <- as.data.frame(do.call(rbind,vars), stringsAsFactors = FALSE)
  names(ans) <- c("type", "var")
  if(nrow(ans) > 0) ans$context <- context
  ans
}

scrub_c_vars <- function(x) {
  gsub(pattern = move_global_re_sub, replacement = "\\2", x = x)
}

c_vars <- function(x,context) {
  if(is.null(x)) return(list(vars = data.frame(), code = NULL))
  list(vars = get_c_vars2(x,context), code = scrub_c_vars(x))
}

pp_defs <- function(x,context) {
  w <- grep("#define ", x, fixed = TRUE)
  if(length(w)==0) {
    return(list(vars = NULL, code = NULL, n = 0, tab = data.frame()))  
  }
  x <- trimws(x[w])
  x <- my_str_split(x, " +", n = 3, collapse = " ")
  vars <- s_pick(x, 2)
  code <- s_pick(x, 3)
  list(
    vars = vars, code = code, n = length(x), 
    tab = data.frame(
      type = "define", 
      var = vars, 
      context = "global", 
      stringsAsFactors = FALSE
    )
  )
}

move_global2 <- function(spec, env, build) {
  pream <- c_vars(spec$PREAMBLE, context = "preamble")
  if(!is.null(pream$code)) {
    spec$PREAMBLE <- pream$code
  }
  pred <- c_vars(spec$PRED, context = "pred")
  if(!is.null(pred$code)) {
    spec$PRED <- pred$code  
  }
  glob <- c_vars(spec$GLOBAL, context = "global")
  main <- c_vars(spec$MAIN, context = "main")
  if(!is.null(main$code)) {
    spec$MAIN <- main$code  
  }
  ode   <- c_vars(spec[["ODE"]], context = "ode")
  if(!is.null(ode$code)) {
    spec$ODE <- ode$code
  }
  table <- c_vars(spec[["TABLE"]], context = "table")  
  if(!is.null(table$code)) {
    spec$TABLE <- table$code
  }
  to_ns <- bind_rows(
    pream$vars,
    pred$vars,
    main$vars,
    ode$vars,
    table$vars
  )
  vars <- bind_rows(glob$vars, to_ns)
  if(any(cap <- to_ns$type=="capture")) {
    captures <- to_ns[cap,"var"]
    spec <- c(spec,list(CAPTURE=mytrim(unlist(captures, use.names=FALSE))))
  }
  to_global <- "typedef double capture;"
  if(nrow(to_ns)  > 0) {
    to_global <- c(
      to_global,
      "namespace {",
      paste0("  ", to_ns$type, " ", to_ns$var, ";"),
      "}"
    ) 
  }
  build$global_vars <- vars
  defines <- pp_defs(spec[["GLOBAL"]], context = "global")
  build$defines <- defines$vars
  build$cpp_variables <- bind_rows(defines$tab, vars)
  
  env[["global"]] <- to_global
  if(nrow(to_ns)  > 0) {
    env[["move_global"]] <- to_ns$var
  } else {
    env[["move_global"]] <- character(0)  
  }
  if(defines$n > 0) {
    env[["defines"]] <- defines$vars  
  } else {
    env[["defines"]] <- character(0)  
  }
  spec
}

# nocov start
get_rcpp_vars <- function(y) {
  m <- gregexpr(move_global_rcpp_re_find,y,perl=TRUE)
  regm <- regmatches(y,m)
  regm <- unlist(regm)
  gsub(pattern = "\\s*=$", replacement = ';', x = regm, perl = TRUE)
}
# nocov end

check_block_data <- function(x,env,pos) {
  if(length(x)==0) {
    name <- env$incoming_names[pos]
    warning("Block no. ", pos, " [", name, "]: no data was found.", call.=FALSE)
  }
  return(NULL)
}

# Code for relocating Rcpp objects in PREAMBLE
get_rcpp_globals <- function(x) {
  global_rcpp_reg <- "\\s*global\\s+(Rcpp::)?(Logical|Integer|Character|Numeric)(Vector|Matrix)\\s+(\\w+?)\\s*(=.*;)"
  global_rcpp_sub <- "\\s*global\\s+(Rcpp::)?(Logical|Integer|Character|Numeric)(Vector|Matrix)\\s+"
  declare_rcpp_globals <- function(x) {
    paste0("Rcpp::", x[3], x[4], " ", x[5], ";")
  }
  m <- regmatches(x,regexec(global_rcpp_reg, x, perl = TRUE))
  w <- which(sapply(m,length) > 0)
  vars <- declare <-  character(0)
  if(length(w) > 0) {
    vars <- sapply(m[w],  "[", 5L)
    x[w] <- gsub(global_rcpp_sub, "", x[w])
    declare <- sapply(m[w], declare_rcpp_globals)
  }
  list(x = x, m = m, w = w, vars = vars, declare = declare) 
}

global_rcpp <- function(spec) {
  x <- spec[["PREAMBLE"]]
  globals <- get_rcpp_globals(x)
  if(length(globals[["vars"]]) > 0) {
    spec[["PREAMBLE"]] <- globals[["x"]]
    spec[["GLOBAL"]] <- c(wrap_namespace(globals[["declare"]],NULL), spec[["GLOBAL"]])
  }
  spec
}

parse_ats <- function(x) {
  
  if(length(x)==0) return(list())
  
  # Require that line starts with @
  # ls are lists of boolean options on a single ine
  x <- mytrim(unlist(strsplit(x,"@",fixed=TRUE)))
  x <- x[x!=""]
  
  # Name/value lines will have spaces but not lists
  nv <- grepl(" ", x, fixed = TRUE)
  
  # Boolean are not Name/value
  if(any(!nv)) {
    negate <- substr(x[!nv], 1, 1) == "!"
    x[!nv] <- paste0(cvec_cs(x[!nv]), " ",  !negate)
  }
  
  # find the first space
  sp <- regexpr(" ", x, fixed=TRUE)
  
  # The names
  a <- substr(x, 1,sp-1)
  
  # drop ! from names globally
  a <- gsub("!", "", a, fixed = TRUE)
  
  # The values
  b <- substr(x,sp+1,nchar(x))
  
  # Warn if quotes
  #if(any(charthere(b,"\"") | charthere(b,"'"))) {
  if(any(substr(b,1,1) %in% c("\"","\'"))) {
    warning("Found quotation mark in option value.",call.=FALSE) 
  }
  
  # Convert type
  b <- setNames(lapply(b,type.convert,as.is=TRUE),a)
  b
}

##' Scrape options from a code block
##' 
##' @param x data
##' @param def default values
##' @param all return all options, even those that are not in \code{def}
##' @param marker assignment operator; used to locate lines with options
##' @param narrow logical; if \code{TRUE}, only get options on lines starting 
##' with \code{>>}
##' @param envir environment from \code{$ENV}
##' @param allow_multiple if \code{TRUE}, the list with replicate names
##' will be reduced
##' @return list with elements \code{x} (the data without options) and named 
##' options  as specified in the block.
##' @keywords internal
scrape_opts <- function(x,envir=list(),def=list(),all=TRUE,marker="=",
                        allow_multiple = FALSE, narrow=TRUE) {
  
  x <- unlist(strsplit(x, "\n",fixed=TRUE))
  
  ## Get lines starting with >>
  opts <- grepl("^\\s*>>",x,perl=TRUE)
  
  has_at <- grepl("^\\s*@", x, perl=TRUE) 
  
  if((!narrow) && (!any(has_at))) {
    opts <- opts | grepl(marker,x,fixed=TRUE)
  }
  
  at <- parse_ats(x[has_at])
  
  data <- x[!(opts | has_at)]
  
  opts <- c(gsub(">>","", x[opts], fixed=TRUE))
  
  opts <- tolist(opts,envir=envir)
  
  opts <- c(opts,at)
  
  if(allow_multiple) {
    opts <- collect_opts(opts)  
  }
  
  opts <- merge.list(def, opts, open=all,warn=FALSE,context="opts")
  
  if(any(duplicated(names(opts)))) {
    stop("Found duplicated block option names.", call.=FALSE) 
  }
  
  opts$x <- NULL
  
  c(list(x=data), opts)
}

##' Scrape options and pass to function
##' 
##' @param x data
##' @param env parse environment
##' @param pass function to call
##' @param ... arguments passed to \code{\link{scrape_opts}}
##' 
##' @details Attributes of \code{x} are also scraped and merged with options.
##' @keywords internal
scrape_and_call <- function(x,env,pass,...) {
  o <- scrape_opts(x,envir=env$ENV,...)
  o$pos <- o$env <- o$class <- NULL
  o <- c(o,attributes(x),list(env=env))
  do.call(pass,o)
}

dump_opts <- function(x, env, block, ...) {
  hasopt <- grep("^\\s*(>>|@)", x, perl = TRUE)
  if(length(hasopt)==0) return(x)
  x[-hasopt]
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

parse_env <- function(spec, incoming_names = names(spec),build,ENV=new.env()) {
  n <- length(spec)
  mread.env <- new.env()
  mread.env$project <- build[["project"]]
  mread.env$root <- build[["root"]]
  mread.env$ext <- build[["ext"]]
  mread.env$param <- vector("list", n)
  mread.env$fixed <- vector("list", n)
  mread.env$init  <- vector("list", n)
  mread.env$omega <- vector("list", n)
  mread.env$sigma <- vector("list", n)
  mread.env$annot <- vector("list", n)
  mread.env$ode   <- vector("list", n)
  mread.env$audit_dadt <- FALSE
  mread.env$`using_nm-vars` <- FALSE
  # TODO:
  #mread.env$using_autodec <- FALSE
  mread.env$namespace <- vector("list", n)
  mread.env$capture <- vector("list", n)
  mread.env$error <- character(0)
  mread.env$covariates <- character(0)
  mread.env$nm_import <- character(0)
  mread.env$ENV <- ENV 
  mread.env$blocks <- names(spec)
  mread.env$incoming_names <- incoming_names
  mread.env$capture_etas <- NULL
  mread.env
}

wrap_namespace <- function(x,name) {
  if(length(x)==0) return(character(0))
  paste0(c(paste0("namespace ", name, " {"), paste0("  ", x),"}"), collapse="\n")
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
  if(nrow(what) > 0) {
    .capture <- intersect(.capture,what[,"name"])
    what[["block"]] <- "CAPTURE"
  }
  
  annot <- dplyr::filter(annot, !(block=="CAPTURE" & name %in% .capture))
  bind_rows(annot,what)
}

include_rfile <- function(rfile) {
  rfile <- normalizePath(rfile)
  if(!file.exists(rfile)) {
    msg <- c(
      basename(rfile), 
      " is required to compile this model, but cound not be found ", 
      "in the directory",
      dirname(rfile)
    )
    stop(msg, call.=FALSE)
  }
  source(rfile, local = parent.frame())
}

evaluate_at_code <- function(x, cl, block, pos, env = list(), named = FALSE) {
  x <- try(eval(parse(text = x), envir = env))
  if(inherits(x, "try-error")) {
    message("Block no: ", pos)
    message("Block type: ", block)
    stop("failed to parse block code.", call.=FALSE)
  }
  right_type <- inherits(x, cl)
  names_missing <- isTRUE(named) && !is_named(x)
  if(!right_type | names_missing) {
    message("Block no: ", pos)
    message(" Block type: ", block)
    if(names_missing) {
      message(" Expected names: yes") 
      message(" Returned named object: no")
      msg <- "the returned object must have names"
    }
    if(!right_type) {
      message(" Expected class: ", paste0(cl, collapse = " or "))
      message(" Returned class: ", paste0(class(x), collapse = ", "))
      msg <- "the returned object was the wrong type"
    }
    stop(msg, call.=FALSE) 
  }
  x
}

get_valid_capture <- function(param, omega, sigma, build, mread.env) {
  n_omega <- sum(nrow(omega))
  if(n_omega > 0) {
    .eta <- paste0("ETA(",seq_len(n_omega),")")  
  } else {
    .eta <- NULL  
  }
  n_sigma <- sum(nrow(sigma))
  if(n_sigma > 0) {
    .eps <- paste0("EPS(",seq_len(n_sigma),")")
  } else {
    .eps <- NULL  
  }
  ans <- c(
    names(param), 
    unlist(labels(omega)), 
    unlist(labels(sigma)),
    .eta,
    .eps,
    build[["cpp_variables"]][["var"]], 
    mread.env[["autov"]]
  )
  if(isTRUE(mread.env[["using_nm-vars"]])) {
    ans <- c(ans, build[["nm-vars"]][["match"]][["match"]])  
  }
  unique(ans)
}

#' Find assignments and capture lhs
#' 
#' @keywords internal
#' @noRd
autodec_find <- function(code) {
  keep <- grep("=", code, fixed = TRUE)
  code <- code[keep]
  if(length(code)==0) {
    return(NULL)
  }
  ans <- regmatches(code, regexpr("[._[:alnum:]]+ *=([^=]|$)", code, perl = TRUE))
  ans <- unique(sub(" *=.?$", "", ans, perl = TRUE))
  ans[!grepl(".", ans, fixed = TRUE)]
}

#' Call `autodec_find` ona list of code chunks
#' 
#' @keywords internal
#' @noRd
autodec_vars <- function(code, blocks = NULL) {
  if(is.null(code)) return(NULL)
  if(is.list(code)) {
    code <- unlist(code[blocks], use.names = FALSE)  
  }
  autodec_find(code)
}

#' Clean up `autodec` candidates
#' 
#' @param vars candidates
#' @param rdefs compartments and parameters that will be implemented with 
#' C++ pre-processor defines
#' @param build the model `build` object; this contains variables that will 
#' be globally declared
#' @param skip additional names to scrub
#' 
#' @details 
#' 
#' Remove
#' 
#' - Anything already showing up as a pre-processor definition
#' - Anyting that is in a reserved word list
#' - Anything that already was declared with a type
#' 
#' @md
#' @keywords internal
#' @noRd
autodec_clean <- function(vars, rdefs, build, skip = NULL) {
  rdefs <- strsplit(rdefs, " ", fixed = TRUE)
  rdefs <- s_pick(rdefs, 2)
  cpp <- build[["cpp_variables"]][["var"]]
  vars <- setdiff(vars, c(Reserved, rdefs, cpp))
  # We are not cleaning Reserved_nm here; this will be checked in  
  # autodec_nm_vars
  vars <- setdiff(vars, skip)
  vars
}

#' Format and save `autodec` to be used later
#' 
#' @keywords internal
#' @noRd
autodec_save <- function(vars, build, env) {
  if(length(vars) ==0) {
    env[["autov"]] <- NULL
    return(invisible(NULL))
  }
  ans <- data.frame(
    type = "double", 
    var = vars, 
    context = "auto", 
    stringsAsFactors = FALSE
  )
  build[["cpp_variables"]] <- rbind(build[["cpp_variables"]], ans)
  env[["autov"]] <- vars
  return(invisible(NULL))
}

#' Format `autodec` as an unnamed namespace 
#' 
#' @keywords internal
#' @noRd
autodec_namespace <- function(build, env) {
  if(length(env[["autov"]])==0) {
    return(NULL)      
  }
  ans <- wrap_namespace(paste0("double ", env[["autov"]], ";"), "")
  ans
}
