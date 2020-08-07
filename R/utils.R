# Copyright (C) 2013 - 2020  Metrum Research Group
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

dllfile <- function(x) paste0(dllname(x),.Platform$dynlib.ext)
pathfun <- function(...) path.expand(...) #,mustWork=FALSE,winslash=.Platform$file.sep

# Used in mrgsim
bind_col <- function(x,y,z) {
  cbind(x,matrix(z,ncol=1, nrow=nrow(x), byrow=TRUE, dimnames=list(NULL, y)))
}

is.mt <- function(x) {return(is.null(x) | length(x)==0)}

##' Merge two lists
##'
##' @param x the original list
##' @param y the new list for merging
##' @param open logical indicating whether or not new items should 
##' be allowed in the list upon merging
##' @param wild wild-card name; see details
##' @param warn issue warning if nothing found to update
##' @param context description of usage context
##' @param ... not used
##' 
##' @rdname merge
##' 
##' @details
##' Wild-card names (\code{wild}) are always retained in \code{x} and
##' are brought along from \code{y} only when \code{open}.
##' 
##' @export
##' @keywords internal
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
      warning(paste0("found nothing to update: ", context), call.=FALSE)
    }
  }
  x
}

combine_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  left[names(right)] <-  right
  left
}

update_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  common <- intersect(names(left), names(right))
  left[common] <-  right[common]
  left
}

##' Simulate from a multivariate normal distribution with mean zero
##'
##' @param mat a positive-definite matrix
##' @param n number of variates to simulate
##' @param seed if not null, passed to set.seed
##' @export
##' @keywords internal
mvgauss <- function(mat, n=10, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  .Call(`_mrgsolve_MVGAUSS`, mat, n)
}

# nocov start
pfile <- function(package,dir,file,ext=NULL) {
  ans <- file.path(system.file(package=package),dir,file)
  if(is.character(ext)) {
    ans <- paste0(ans, ".", ext)
  }
  return(ans)
}

mrgsolve_file <- function(..., package="mrgsolve") {
  system.file(..., package = package)
}
# nocov end

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
# cvec_c_nws <- function(x) {
#   if(is.null(x) | length(x)==0) return(character(0))
#   x <- unlist(strsplit(as.character(x),",",fixed=TRUE),use.names=FALSE)
#   x <- gsub(" ", "",x, fixed=TRUE)
#   x <- x[x!=""]
#   if(length(x)==0) {
#     return(character(0))
#   } else {
#     return(x) 
#   }
# }


## Old
as.cvec <- function(x) {
  if(is.null(x)) return(character(0))
  x <- gsub("^\\s+|\\s+$", "", x, perl=TRUE)
  unlist(strsplit(as.character(x),"\\s*(\n|,|\\s+)\\s*",perl=TRUE))
}

# collapse a character vector back to length n (undo strsplit)
collapsen <- function(string,pattern,n=3) {
    if(length(string) <= n) return(string)
    if(n==1) return(paste0(string, collapse = pattern))
    ans <- string[seq(1,(n-1))]
    if(n >= 2) {
      remainder <- paste0(string[seq(n,length(string))],collapse=pattern)
      ans <- c(ans, remainder)  
    }
    ans
}

# replica str_split; to be replace if / when we take up stringr
my_str_split <- function(string,pattern,n=3,fixed=FALSE) {
  m <- strsplit(string, pattern, fixed = fixed)
  lapply(m,collapsen,pattern=pattern,n=n)
}

##' Create template data sets for simulation
##'
##' @param ... passed to [expand.grid]
##' 
##' @details
##' An ID column is added as `seq(nrow(ans))` if not supplied by the user.  
##' For `expand.ev`, defaults also added include `cmt = 1`, 
##' `time = 0`, `evid = 1`.  If `total` is included, 
##' then `addl` is derived as `total` - 1. If `tinf` is included, then 
##' an infusion rate is derived for row where `tinf` is greater than 
##' zero.
##'
##' @examples
##' idata <- expand.idata(CL = c(1,2,3), VC = c(10,20,30))
##'
##' doses <- expand.ev(amt = c(300,100), ii = c(12,24), cmt = 1)
##' 
##' infusion <- expand.ev(amt = 100, tinf = 2)
##' 
##' @md
##' @export
expand.idata <- function(...) {
  ans <- expand.grid(...,stringsAsFactors=FALSE)
  ans$ID <- seq_len(nrow(ans))
  dplyr::select(ans, "ID", everything())
}

#' @export
#' @rdname expand.idata
expand.ev <- function(...) {
  ans <- expand.grid(...,stringsAsFactors=FALSE)
  ans[["ID"]] <- seq_len(nrow(ans))
  if(!has_name("evid", ans)) ans[["evid"]] <- 1
  if(!has_name("cmt", ans)) ans[["cmt"]] <- 1
  if(!has_name("time", ans)) ans[["time"]] <- 0
  if(!has_name("amt", ans)) ans[["amt"]] <- 0
  finalize_ev(ans)
}

#' @export
#' @rdname expand.idata
ev_expand <- expand.ev

#' Expand an event data frame across multiple ID
#' 
#' @noRd
expand_event_object <- function(event,ID) {
  event <- as.data.frame(event)
  out_names <- unique(c("ID", names(event)))
  ind <- rep(seq(nrow(event)), times=length(ID))
  big <- dplyr::slice(event, ind)
  big[["ID"]] <- rep(ID, each=nrow(event))
  big[,out_names]
} 

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
  x <- gsub("(,|\\s)+$", "", x)
  if(concat) {
    x <- x[!(grepl("^\\s*$",x,perl=TRUE))]
    x <- x[x!=""] # waste?
    if(length(x)>1) x <- paste(x, collapse=',')
  }
  x <- type.convert(unlist(strsplit(x,split="\\,|\n|\\s+",perl=TRUE)), as.is=TRUE)
  x[nchar(x)>0]
}

##' Create create character vectors
##'
##' @param x comma-separated quoted string (for \code{cvec})
##' @param ... unquoted strings (for \code{ch})
##' @examples
##'
##' cvec("A,B,C")
##' s_(A,B,C)
##' @export
##' @keywords internal
cvec <- function(x) UseMethod("cvec")

##' @export
##' @rdname cvec
##' @keywords internal
cvec.character <- as.cvec

##' @export
##' @rdname cvec
##' @keywords internal
s_ <- function(...) as.character(match.call(expand.dots=TRUE))[-1] #nocov

##' Access or clear arguments for calls to mrgsim
##'
##' @param x model object
##' @param clear logical indicating whether or not to clear `args` from 
##' the model object
##' @param which character with length 1 naming a single arg to get
##' @param ... passed along
##' 
##' @return If `clear` is `TRUE`, the argument list is 
##' cleared and the model object is returned.  Otherwise, the argument 
##' list is returned.
##' 
##' @examples
##' mod <- mrgsolve::house()
##' mod %>% Req(CP,RESP) %>% carry_out(evid,WT,FLAG) %>% simargs
##' 
##' @md
##' @export
simargs <- function(x, which = NULL, clear=FALSE,...) {
  
  if(clear) {
    x@args <- list()
    return(x)
  }
  if(!is.character(which)) {
    return(x@args) 
  }
  return(x@args[[which]])
}


## https://github.com/RcppCore/Rcpp/commit/59d3bf2e22dafb853c32d82b5e42899152f85c20
build_path <- function(x) {
  if(.Platform$OS.type != "windows") return(x)
  x <- normalizePath(x)
  if (grepl(' ', x, fixed=TRUE)) x <- utils::shortPathName(x)
  x <- gsub("\\\\", "/", x)
  return(x)
}

##' Set RNG to use L'Ecuyer-CMRG
##'
##' @export
mcRNG <- function() base::RNGkind("L'Ecuyer-CMRG")

as_character_args <- function(x) {
  x <- deparse(x,width.cutoff=500)
  x <- gsub("^.*\\(|\\)$", "", x)
  x
}

get_tokens <- function(x,unlist=FALSE) {
  if(!is.character(x)) return(character(0))
  if(unlist) return(.Call(`_mrgsolve_get_tokens`, x)[["tokens"]])
  .Call(`_mrgsolve_get_tokens`, x)
}

grepn <- function(x,pat,warn=FALSE) {
  if(is.null(names(x))) {
    if(warn) {
      warning("grepn: pattern was specified, but names are NULL.", 
              call.=FALSE)
    }
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

# ll_pick <- function(x,name) {
#   stopifnot(is.list(x))
#   lapply(x,"[[",name)
# }
# 
# l_pick <- function(x,name) {
#   stopifnot(is.list(x))
#   lapply(x,"[",name)
# }

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

has_name <- function(name,object) {
  is.element(name,names(object))
}

has_ID <- function(object) {
  is.element("ID", names(object)) 
}

file_exists <- function(x) {
  file.exists(x)
}

file_writeable <- function(x) {
  file.access(x,2)==0 
}

file_readable <- function(x) {
  file.access(x,4)==0 
}

where_is <- function(what,x) {
  as.integer(unlist(gregexpr(what,x,fixed=TRUE)))
}

object_exists <- function(name,envir,mode="any",inherits=FALSE) {
  if(!exists(name,envir=envir,mode=mode,inherits=inherits)) {
    wstop("couldn't find object ", name) 
  }
}

tparse <- function(x,...) parse(text=x,...)

na2zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

sanitize_capture <- function(x) {
  for(i in seq_along(x)) {
    x[i] <- gsub("\\[|\\(", "_", x[i])
    x[i] <- gsub("\\]|\\)", "",  x[i])
  }
  x
}

# from metrumrg package
locf <- function(x){
  good <- !is.na(x)
  positions <- seq(length(x))
  good.positions <- good * positions
  last.good.position <- cummax(good.positions)
  last.good.position[last.good.position==0] <- NA
  x[last.good.position]
}
forbak <- function(x)nocb(locf(x))
bakfor <- function(x)locf(nocb(x))
nocb <- function(x)rev(locf(rev(x)))

locf_tibble <- function(x) {
  mutate_all(x, .funs = ~ locf(.))
}

locf_ev <- function(x) {
  if(!is.ev(x)) {
    wstop("x is not an event object") 
  }
  x@data <- mutate_all(x@data, .funs = ~locf(.))
  x
}

arrange__ <- function(df, .dots) {
  arrange(df, `!!!`(syms(.dots)))
}

select__ <- function(df, .dots) {
  select(df, `!!!`(syms(.dots)))
}

group_by__ <- function(df,.dots) {
  group_by(df, `!!!`(syms(.dots)))
}

distinct__ <- function(df, .dots, .keep_all = FALSE) {
  dplyr::distinct(df, `!!!`(syms(.dots)), .keep_all = .keep_all)  
}

divider_msg <- function(msg = "", width = 60) {
  if(nchar(msg) > 0) {
    msg <- paste0("---:: ", msg, " ::")    
  }
  rem <- width - nchar(msg)
  msg <- paste0(
    msg, 
    paste0(rep("-", rem),collapse="")
  )
  return(msg)
}

reg_exec_match <- function(x, pattern) {
  m <- regexec(pattern,x)
  regmatches(x,m)
}

collect_opts <- function(x) {
  un <- unique(names(x))
  ans <- lapply(un, function(y) {
    unlist(x[names(x)==y],use.names=FALSE)
  })
  names(ans) <- un
  ans
}

make_matrix_labels <- function(mat,lab,diag=TRUE) {
  n <- nrow(mat)
  cmat <- matrix(NA_character_, n, n)
  for(i in seq(n)) {
    for(j in seq(n)) {
      if(i > j) next
      if(i==j) {
        val <- lab[i] 
      } else {
        val <- paste0(c(lab[i],lab[j]),collapse='-')
      }
      cmat[i,j] <- val
    }
  }
  ans <- mat[upper.tri(mat,diag=diag)]
  lab <- cmat[upper.tri(cmat,diag = diag)]
  names(ans) <- lab
  ans
}



# nocov start
is.numeric.data.frame <- function(x) vapply(x,is.numeric,TRUE)

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
} # nocov end


system4 <- function(cmd, args=character(0), pattern, path) {
  files <- file.path(path, paste0("system4__",c("stdout","stderr"),"__", pattern))
  x <- list(status=system2(cmd, args, stdout = files[1], stderr = files[2]))
  x[["stdout"]] <- readLines(files[1])
  x[["stderr"]] <- readLines(files[2])
  x
}

wstop <- function(..., width = getOption("width", 60), call.=FALSE) {
  x <- unlist(list(...))
  x <- paste0(x,collapse="")
  x <- sub(" *\n *", " ", x)
  x <- strwrap(x, exdent = 7)
  x <- paste0(x,collapse="\n")
  stop(x, call.=call.)
}

mod_first <- function(cl) {
  fun <- deparse(match.call(sys.function(-1),sys.call(-1))[1])
  fun <- substr(fun,1,(nchar(fun)-2L))
  msg <- sprintf("the first argument to %s must be a model object",fun)
  wstop(msg)
}


