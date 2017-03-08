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

render_annot <- function(x,block,...) {
  dplyr::bind_rows(lapply(x,tibble::as_data_frame)) %>%
    dplyr::mutate(block=block) %>% 
    dplyr::select(block,dplyr::everything()) %>%
    as.data.frame
}


parse_annot <- function(x,noname=FALSE,novalue=FALSE,block='.',name_value=TRUE,...) {
  ## x is a list
  if(is.null(x)) return(NULL)
  x <- x[nchar(x)>0]
  x <- lapply(x,parse_annot_line,novalue=novalue,noname=noname)
  nm <- s_pick(x,"name")
  v <-  s_pick(x,"value")
  
  if(name_value) {
    v <- setNames(tolist(paste(v,collapse=","),...),nm)
  } else {
    v <- as.numeric(tovec(v)) 
  }
  an <- lapply(x,"[", c("name","descr", "unit","options"))
  an <-  render_annot(an,block)
  list(v=v,an=an,nm=nm)
}

## Convenience; keep around for a little bot
gmatch <- function(what,x) as.integer(gregexpr(what,x,fixed=TRUE)[[1]])

parse_annot_line <- function(x, novalue=FALSE, noname=FALSE) {
  
  if(nchar(x)==0) return(NULL)
  
  x <- mytriml(x)
  
  col <- charcount(x,":")[1]
  
  if(col != (2-noname-novalue)) {
    stop("Improper model annotation: ", x, call.=FALSE) 
  }
  
  ## Fix up line if not name : value : other
  if(noname) x <- paste0(". :",x)
  if(novalue) x <- gsub(":",": 0 :",x,fixed=TRUE)
  
  a <- strsplit(x,"\\s*:\\s*",perl=TRUE)[[1]]
  
  b <- a[3]
  
  ## grep out units and options
  units <- regmatches(b,gregexpr("\\s*\\(.*?\\)",b))[[1]]
  options <- regmatches(b,gregexpr("\\s*\\[.*?\\]",b))[[1]]
  
  if(length(units)   > 1) units   <- units[length(units)]
  if(length(options) > 1) options <- options[length(options)]
  
  ## Drop matches
  for(what in c(units,options)) b <- sub(what,"",b,fixed=TRUE)  
  
  ## Clean up matches
  units <-   gsub("\\s*\\(\\s*|\\s*\\)", "", units,   perl=TRUE)
  options <- gsub("\\s*\\[\\s*|\\s*\\]", "", options, perl=TRUE)
  
  ## This is the "description"
  b <- mytrim(b)
  
  if(length(units)==0) units <- '.'
  if(length(options)==0) options <- '.'
  
  list(name=a[1],value=a[2],unit=units,options=options,descr=b)
}

##' Extract model details.
##' 
##' @param x a model object
##' @param complete logical; if \code{TRUE}, un-annotated parameters and compartments will be added to the output
##' @param values logical; if \code{TRUE}, a values column will be added to the output
##' @param ... not used
##' 
details <- function(x,complete=FALSE,values=FALSE,...) {
  
  stopifnot(is.mrgmod(x))
  
  ans <- x@annot[["data"]]
  
  if(nrow(ans)==0) {
    ans <- cobble_details(x)
  } else {
    if(complete) ans <- complete_details(ans,x)
  }
  if(values) {
    ans <- add_detail_values(ans,x) 
  }
  return(ans)
}

store_annot <- function(x,what,loc=soloc(x),...) {
  stopifnot(is.mrgmod(x))
  x@annot <- list(data=what,embedded=TRUE)
  x
}

cobble_details <- function(x) {
  
  ans <- list()
  
  par <- as.numeric(param(x))
  if(length(par) > 0) {
    ans[[1]] <- dplyr::data_frame(block="PARAM",name=names(par))  
  }
  fx <- as.numeric(x@fixed)
  if(length(fx)>0) {
    ans[[3]] <- dplyr::data_frame(block="FIXED", name=names(fx)) 
  }
  cmt <- as.numeric(init(x))
  if(length(cmt)>0) {
    ans[[3]] <- dplyr::data_frame(block="CMT", name=names(cmt)) 
  }
  
  ans <- dplyr::bind_rows(ans)
  ans <- mutate(ans,descr='.',units='.',options='.')
  as.data.frame(dplyr::select(ans,block,name,descr,units,options))
  
}

complete_details <- function(annot,x) {
  
  par <- as.numeric(param(x))
  cmt <- as.numeric(init(x))
  fx <- as.numeric(x@fixed)
  name <- unique(annot[annot$block %in% c("PARAM", "CMT", "FIXED"),"name"])
  dum <- annot[0,]
  
  if(length(par) > 0) {
    miss <- setdiff(names(par),name)
    if(length(miss) > 0) {
      par <- dplyr::data_frame(block="PARAM",name=miss,descr='.',unit='.',options='.')
    } else {
      par <- dum 
    }
    annot <- dplyr::bind_rows(annot,par)
  }
  
  if(length(cmt) > 0) {
    miss <- setdiff(names(cmt),name)
    if(length(miss) > 0) {
      cmt <- dplyr::data_frame(block="CMT",name=miss,descr='.',unit='.',options='.')
    } else {
      cmt <- dum 
    }
    annot <- dplyr::bind_rows(annot,cmt)
  }
  
  if(length(fx) > 0) {
    miss <- setdiff(names(fx),name)
    if(length(miss) > 0) {
      fx <- dplyr::data_frame(block="FIXED",name=miss,descr='.',unit='.',options='.')
    }  else {
      annot <- dplyr::bind_rows(annot,fx) 
    }
  }
  return(annot)
}

add_detail_values <- function(annot,x) {
  x <- c(as.numeric(allparam(x)),as.numeric(init(x)))
  x <- dplyr::data_frame(name=names(x),value=x)
  annot <- dplyr::left_join(annot,x,by="name")
  return(annot)
  
}


