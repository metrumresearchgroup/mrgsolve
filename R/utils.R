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

dllfile <- function(x) paste0(dllname(x),.Platform$dynlib.ext)
pathfun <- function(...) path.expand(...) #,mustWork=FALSE,winslash=.Platform$file.sep

# Used in mrgsim
bind_col <- function(x,y,z) {
  cbind(x,matrix(z,ncol=1, nrow=nrow(x), byrow=TRUE, dimnames=list(NULL, y)))
}

is.mt <- function(x) {return(is.null(x) | length(x)==0)}

##' Merge two lists.
##'
##' @param x the original list
##' @param y the new list for merging
##' @param wild wild-card name; see details
##' @param warn issue warning if nothing found to update
##' @param context description of usage context
##' @param ... not used
##' @param open logical indicating whether or not new items should be allowed in the list upon merging.
##' @rdname merge
##' @details
##' Wild-card names (\code{wild}) are always retained in \code{x} and are brought along from \code{y} only when \code{open}.
##' @export
merge.list <- function(x,y,...,open=FALSE,
                       warn=TRUE,context="object",wild="...") {
  
  y <- as.list(y)
  
  if(!open) {
    y <- y[names(y)!=wild | is.null(names(y))]
  }
  
  ## Merge two lists
  common <- intersect(names(x), names(y))
  common <- common[common != wild]
  
  x[common] <- y[common]
  
  if(open)  {
    nw <- !is.element(names(y),names(x)) | names(y) == wild
    x <- c(x,y[nw])
  } else {
    if(length(common)==0 & warn) {
      warning(paste0("Found nothing to update: ", context), call.=FALSE)
    }
  }
  x
}

render_time <- function(x) {
  add <- times <- numeric(0)
  if(!is.mt(x@add)){add <- x@add}
  if(x@end >=0){times <-seq(x@start,x@end,x@delta)}
  times <- invisible(as.numeric(c(times,add)))
  if(is.mt(times)) {return(0)}
  sort(unique(times[times>=0]))
}


##' Simulate from a multivariate normal distribution with mean zero.
##'
##' @param mat a positive-definite matrix
##' @param n number of variates to simulate
##' @param seed if not null, passed to set.seed
##' @export
mvgauss <- function(mat, n=10, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  .Call(mrgsolve_MVGAUSS, mat, n)
}

# simulateres <- function(n1,omega,n2,sigma) {
#   .Call("mrgsolve_SIMRE", PACKAGE="mrgsolve", n1, omega,n2,sigma,-1)
# }


pfile <- function(package,dir,file,ext=NULL) {
  ans <- file.path(path.package(package),dir,file)
  if(is.character(ext)) {
    ans <- paste0(ans, ".", ext)
  }
  return(ans)
}

cropstr <- function(string, prefix, suffix, bump= "...") {
  nc <- nchar(string)
  total <- prefix+suffix
  if(all(nc <= total)) return(string)
  paste0(substr(string,1,prefix) , bump, substr(string,(nc-suffix+nchar(bump)+1),nc))
}

mytrim <- function(x) {
  gsub("^\\s+|\\s+$", "",x,perl=TRUE) 
}

mytriml <- function(x) {
  gsub("^\\s+", "",x,perl=TRUE) 
}

mytrimr <- function(x) {
  gsub("\\s$", "",x,perl=TRUE) 
}


## Create character vector 
## Split on comma or space 
cvec_cs <- function(x) {
  if(is.null(x) | length(x)==0) return(character(0))
  x <- unlist(strsplit(as.character(x),",",fixed=TRUE),use.names=FALSE)
  x <- unlist(strsplit(x," ",fixed=TRUE),use.names=FALSE)
  x <- x[x!=""]
  if(length(x)==0) {
    return(character(0))
  } else {
    return(x) 
  }
}

## Create a character vector
## Split on comma and trim
cvec_c_tr <- function(x) {
  if(is.null(x) | length(x)==0) return(character(0))
  x <- unlist(strsplit(as.character(x),",",fixed=TRUE),use.names=FALSE)
  x <- gsub("^\\s+|\\s+$", "",x, perl=TRUE)
  x <- x[x!=""]
  if(length(x)==0) {
    return(character(0))
  } else {
    return(x) 
  }
}

## Create a character vector 
## Split on comma and rm whitespace
cvec_c_nws <- function(x) {
  if(is.null(x) | length(x)==0) return(character(0))
  x <- unlist(strsplit(as.character(x),",",fixed=TRUE),use.names=FALSE)
  x <- gsub(" ", "",x, fixed=TRUE)
  x <- x[x!=""]
  if(length(x)==0) {
    return(character(0))
  } else {
    return(x) 
  }
}


## Old
as.cvec <- function(x) {
  if(is.null(x)) return(character(0))
  x <- gsub("^\\s+|\\s+$", "", x, perl=TRUE)
  unlist(strsplit(as.character(x),"\\s*(\n|,|\\s+)\\s*",perl=TRUE))
}


##' Create template data sets for simulation.
##'
##' @param ... passed to \code{\link{expand.grid}}
##' 
##' @details
##' An ID column is added as \code{1:nrow(ans)} if not supplied by the user.  
##' For \code{expand.ev}, defaults
##' also added: \code{cmt = 1}, \code{time = 0}, \code{evid = 1}.
##'
##' @examples
##' idata <- expand.idata(CL=c(1,2,3), VC=c(10,20,30))
##'
##' doses <- expand.ev(amt=c(300,100), ii=c(12,24), cmt=1)
##' @export
expand.idata <- function(...) {
  ans <- expand.grid(...,stringsAsFactors=FALSE)
  ans$ID <- 1:nrow(ans)
  shuffle(ans,"ID")
}

##' @export
##' @rdname expand.idata
expand.ev <- function(...) {
  ans <- expand.grid(...,stringsAsFactors=FALSE)
  ans$ID <- 1:nrow(ans)
  if(!has_name("evid", ans)) ans$evid <- 1
  if(!has_name("cmt", ans)) ans$cmt <- 1
  if(!has_name("time", ans)) ans$time <- 0
  shuffle(ans,"ID")
}


is.numeric.data.frame <- function(x) sapply(x, is.numeric)




tolist <- function(x,concat=TRUE,envir=list()) {
  if(is.null(x)) return(list())
  x <- gsub("(,|\\s)+$", "",x,perl=TRUE)
  x <- x[!(grepl("^\\s*$",x,perl=TRUE))]
  x <- x[x!=""]  ## waste?
  if(length(x)>1) x <- paste(x, collapse=',')
  return(eval(parse(text=paste0("list(", x, ")")),envir=envir))
}


tovec <- function(x,concat=TRUE) {
  if(is.null(x)) return(numeric(0))
  ##x <- gsub(eol.comment, "\\1", x)
  x <- gsub("(,|\\s)+$", "", x)
  if(concat) {
    x <- x[!(grepl("^\\s*$",x,perl=TRUE))]
    x <- x[x!=""] # waste?
    if(length(x)>1) x <- paste(x, collapse=',')
  }
  x <- type.convert(unlist(strsplit(x,split="\\,|\n|\\s+",perl=TRUE)), as.is=TRUE)
  x[nchar(x)>0]
}


##' Create create character vectors.
##'
##' @param x comma-separated quoted string (for \code{cvec})
##' @param ... unquoted strings (for \code{ch})
##' @export
##' @examples
##'
##' cvec("A,B,C")
##' ch(A,B,C)
##' s(A,B,C)
##'
setGeneric("cvec", function(x,...) standardGeneric("cvec"))

##' @export
##' @rdname cvec
setMethod("cvec", "character", as.cvec)

##' @export
##' @rdname cvec
ch <- function(...) as.character(match.call(expand.dots=TRUE))[-1]

##' @export
##' @rdname cvec
s <- ch

##' Access or clear arguments for calls to mrgsim.
##'
##' @param x model object
##' @param clear logical indicating whether or not clear args from the model object
##' @param ... passed along
##' 
##' @return If \code{clear} is \code{TRUE}, the argument list is cleared and the model object is returned.  Otherwise, the argument list is returned.
##' 
##' @examples
##' mod <- mrgsolve:::house()
##' mod %>% Req(CP,RESP) %>% carry_out(evid,WT,FLAG) %>% simargs
##' 
##' @export
simargs <- function(x,...) UseMethod("simargs")

##' @rdname simargs
##' @export
simargs.mrgmod <- function(x,clear=FALSE,...) {
  
  if(clear) {
    x@args <- list()
    return(x)
  }
  x@args
}


## https://github.com/RcppCore/Rcpp/commit/59d3bf2e22dafb853c32d82b5e42899152f85c20
build_path <- function(x) {
  if(.Platform$OS.type != "windows") return(x)
  x <- normalizePath(x)
  if (grepl(' ', x, fixed=TRUE)) x <- utils::shortPathName(x)
  x <- gsub("\\\\", "/", x)
  return(x)
}


##' Set RNG to use L'Ecuyer-CMRG.
##'
##' @export
mcRNG <- function() base::RNGkind("L'Ecuyer-CMRG")


if.file.remove <- function(x) {
  if(file_exists(x)) file.remove(x)
}

#' rename columns from vector for new names
#' @param .df dataframe to rename
#' @param new_names vector of names using syntax "<newname>" = "<oldname>"
#' @examples
#' rename_cols(Theoph, c("dv" = "conc", "ID" = "Subject"))
#' @export
rename_cols <- function(.df, new_names) {
  if (!all(new_names %in% names(.df))) {
    missing <- new_names[which(!new_names %in% names(.df))]
    stop(paste("the following columns do not exist in the dataset: ", paste(missing, collapse = ", ")))
  }
  matches <- match(new_names, names(.df))
  names(.df)[matches] <- names(new_names)
  return(.df)
}

as_character_args <- function(x) {
  x <- deparse(x,width.cutoff=500)
  x <- gsub("^.*\\(|\\)$", "", x)
  x
}


get_tokens <- function(x,unlist=FALSE) {
  if(!is.character(x)) return(character(0))
  if(unlist) return(.Call(mrgsolve_get_tokens, x)[["tokens"]])
  .Call(mrgsolve_get_tokens, x)
}

grepn <- function(x,pat,warn=FALSE) {
  if(is.null(names(x))) {
    if(warn) warning("grepn: pattern was specified, but names are NULL.", call.=FALSE)
    return(x)
  }
  if(pat=="*") return(x)
  x[grepl(pat,names(x),perl=TRUE)]
}


nonull <- function(x,...) UseMethod("nonull")
##' @export
nonull.default <- function(x,...) x[!is.null(x)]
##' @export
nonull.list <- function(x,...) x[!sapply(x,is.null)]

s_pick <- function(x,name) {
  stopifnot(is.list(x))
  nonull(unlist(sapply(x,"[[",name)))
}

ll_pick <- function(x,name) {
  stopifnot(is.list(x))
  lapply(x,"[[",name)
}

l_pick <- function(x,name) {
  stopifnot(is.list(x))
  lapply(x,"[",name)
}
s_quote <- function(x) paste0("\'",x,"\'")
d_quote <- function(x) paste0("\"",x,"\"")

mapvalues <- function (x, from, to, warn_missing = FALSE) {
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x)) {
    stop("`x` must be an atomic vector.")
  }
  if (is.factor(x)) {
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  mapidx <- match(x, from)
  mapidxNA <- is.na(mapidx)
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
  }
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}


shuffle <- function (x, who, after = NA)  {
  names(x) <- make.unique(names(x))
  who <- names(x[, who, drop = FALSE])
  nms <- names(x)[!names(x) %in% who]
  if (is.null(after))
    after <- length(nms)
  if (is.na(after))
    after <- 0
  if (length(after) == 0)
    after <- length(nms)
  if (is.character(after))
    after <- match(after, nms, nomatch = 0)
  if (after < 0)
    after <- length(nms)
  if (after > length(nms))
    after <- length(nms)
  nms <- append(nms, who, after = after)
  x[nms]
}

filename <-  function (dir, run = NULL, ext = NULL,short=FALSE) {
  if(short) dir <- build_path(dir)
  file.path(dir, paste0(run, ext))
}

charcount <- function(x,w,fx=TRUE) {
  nchar(x) - nchar(gsub(w,"",x,fixed=fx)) 
}

charthere <- function(x,w,fx=TRUE) {
  grepl(w,x,fixed=fx)
}


null_list <- setNames(list(), character(0))
single.number <- function(x) length(x)==1 & is.numeric(x)


# 15 sept 2016
installed_models <- function() {
  file.path(system.file(package="mrgsolve"), "inst", "models")
}

get_option <- function(what,opt,default=FALSE) {
  if(is.element(what,names(opt))) {
    opt[[what]]
  } else {
    return(default) 
  }
}

has_name <- function(a,b) {
  is.element(a,names(b))
}

file_exists <- function(x) {
  file.exists(x)
  #file.access(x)==0
}

file_writeable <- function(x) {
  file.access(x,2)==0 
}

file_readable <- function(x) {
  file.access(x,4)==0 
}

mrgnorm <- function(n,sigma) {
  ncols <- ncol(sigma)
  matrix(rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
}

where_is <- function(what,x) {
  as.integer(unlist(gregexpr(what,x,fixed=TRUE)))
}

where_first <- function(what,x) {
  as.integer(unlist(regexpr(what,x,fixed=TRUE)))
}

object_exists <- function(name,envir,mode="any",inherits=FALSE) {
  if(!exists(name,envir=envir,mode=mode,inherits=inherits)) {
    stop("Couldn't find object ", name, call.=FALSE) 
  }
}

tparse <- function(x,...) parse(text=x,...)

mt_fun <- function(){}

is.covset <- function(x) {
  class(x)[1] =="covset" 
}

require_covset <- function() {
  stop("covset features are not available in this version of mrgsolve.") 
}

call_system <- function(args) {
  suppressWarnings(do.call(system,args))
}

build_error <- function(args,compfile) {
  if(.Platform$OS.type=="windows" & args$ignore.stdout) {
    
    args$show.output.on.console <- FALSE 
    args$intern <- TRUE
    err <- call_system(args)

    errors <- grepl(paste0("^",compfile),err)
    
    for(i in seq_along(errors)) {
      if(errors[i]) {
        message(err[i]) 
      } else {
        cat(err[i],"\n")
      }
    }
  }
  cat("-------------\n")
  stop("there was a problem building the model.",call.=FALSE)
}


# covset <- function(...) {
#   dmutate::covset(...)
# }