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


##' Event objects for simulating PK and other interventions
##' 
##' An event object specifies dosing or other interventions that get implemented
##' during simulation. Event objects do similar things as \code{\link{data_set}}, 
##' but simpler and quicker.
##'
##' @param x a model object
##' @param time event time
##' @param amt dose amount
##' @param evid event ID
##' @param cmt compartment
##' @param ID subject ID
##' @param replicate logical; if \code{TRUE}, events will be replicated for 
##' each individual in \code{ID}
##' @param until the expected maximum \bold{observation} time for this regimen
##' @param tinf infusion time; if greater than zero, then the \code{rate} item 
##' will be derived as \code{amt/tinf}
##' @param realize_addl if \code{FALSE} (default), no change to \code{addl} 
##' doses.  If \code{TRUE}, \code{addl} doses are made explicit with 
##' \code{\link{realize_addl}}
##' @param object passed to show
##' @param ... other items to be incorporated into the event object; see 
##' details
##' 
##' @details
##' \itemize{
##' \item Required items in events objects include 
##' \code{time}, \code{amt}, \code{evid} and \code{cmt}.
##' \item If not supplied, \code{evid} is assumed to be 1.
##' \item If not supplied, \code{cmt}  is assumed to be 1.
##' \item If not supplied, \code{time} is assumed to be 0.
##' \item If \code{amt} is not supplied, an error will be generated.
##' \item If \code{total} is supplied, then \code{addl} will be set 
##' to \code{total} - 1.
##' \item Other items can include \code{ii}, \code{ss}, and \code{addl}
##' (see \code{\link{data_set}} for details on all of these items).
##' \item \code{ID} may be specified as a vector.
##' \item If replicate is \code{TRUE} (default), then the events 
##' regimen is replicated for each \code{ID}; otherwise, the number of
##' event rows must match the number of \code{ID}s entered
##' }
##' @return events object
##' 
##' @seealso \code{\link{ev_rep}}, \code{\link{ev_days}}, 
##' \code{\link{ev_repeat}}, \code{\link{ev_assign}},
##' \code{\link{ev_seq}}, \code{\link{mutate.ev}},
##' \code{\link{as.ev}}, \code{\link{ev_methods}}
##' 
##' @examples
##' mod <- mrgsolve:::house()
##' 
##' mod <- mod %>% ev(amt=1000, time=0, cmt=1)
##'
##' loading <- ev(time=0, cmt=1, amt=1000)
##' 
##' maint <- ev(time=12, cmt=1, amt=500, ii=12, addl=10)
##' 
##' c(loading, maint)
##' 
##' loading$time
##' 
##' @export
setGeneric("ev", function(x,...) {
  standardGeneric("ev")
})

##' @rdname ev
##' @export
setMethod("ev", "mrgmod", function(x,object=NULL,...) {
  if(is.null(object)) {
    x@args[["events"]] <- ev(...)
    return(x)
  } 
  if(is.character(object)) {
    object <- eval(parse(text = object),envir = x@envir)
  }
  x@args[["events"]] <- object
  x
})

##' @rdname ev
##' @export
setMethod("ev", "missing", function(time=0, amt=0, evid=1, cmt=1, ID=numeric(0), 
                                    replicate=TRUE, until=NULL, tinf=NULL,
                                    realize_addl=FALSE, ...) {
  
  if(length(match.call())==1) { 
    return(new("ev", data=data.frame()[0,]))
  }
  
  if(any(evid==0)) {
    wstop("evid cannot be 0 (observation)")
  }
  
  if(missing(amt)) {
    wstop("argument \"amt\" is missing")
  }
  
  l <- list(time=time, cmt=cmt, amt=amt, evid=evid)
  if(is.numeric(tinf) && length(tinf) > 0) l[["tinf"]] <- tinf
  if(is.numeric(until) && length(until) > 0) l[["until"]] <- until
  
  qu <- quos(...)
  if(length(qu) > 0) {
    na1 <- names(l)
    na2 <- names(qu)
    j <- length(l)
    for(i in seq_along(qu)) {
      l[[j+i]] <- eval_tidy(qu[[i]], l)
    }
    names(l) <- c(na1,na2)
  }
  
  data <- as.data.frame(as_tibble(l))
  
  if(all(c("rate", "tinf") %in% names(data))) {
    wstop("input can include either rate or tinf, not both")
  }
  if(all(c("addl", "until") %in% names(data))) {
    wstop("input can include either addl or until, not both")
  }
  if(all(c("addl", "total") %in% names(data))) {
    wstop("input can include either addl or total, not both")
  }
  
  data <- finalize_ev(data)
  
  if(length(ID) > 0) {
    
    ID <- unique(ID)
    
    if(!is.numeric(ID)) {
      stop("ID must be numeric", call.=FALSE)
    }
    
    if(replicate) {
      if(any(!is.numeric(data))) {
        data <- as.list(data)
        data <- lapply(data, unique)
        data <- do.call("expand.grid", 
                        c(list(ID=ID,stringsAsFactors=FALSE),data))
        data <- arrange__(data,.dots=c("ID", "time"))
        rownames(data) <- NULL
      } else {
        data <- data.frame(.Call(`_mrgsolve_EXPAND_EVENTS`, 
                                 match("ID", colnames(data),0), 
                                 data.matrix(data), 
                                 ID, PACKAGE="mrgsolve" ))
      }
      
    } else {
      if(length(ID)!=nrow(data)) { 
        stop("length of ID does not match number of events while replicate = FALSE",
             call.=FALSE)
      }
      data[["ID"]] <- ID
    }
  }
  
  if(realize_addl) data <- realize_addl(data)
  return(new("ev", data=data))
})

##' @rdname ev
##' @export
setMethod("ev", "ev", function(x, realize_addl=FALSE,...) {
  if(realize_addl) {
    return(realize_addl(x))
  } else {
    return(x)
  }
})


##' Coerce an object to class ev
##' 
##' @param x an object to coerce
##' @param keep_id if \code{TRUE}, \code{ID} column is retained if it exists
##' @param clean if \code{TRUE}, only dosing or ID information is retained in
##' the result
##' @param ... not used
##' 
##' @examples
##' data <- data.frame(amt = 100) 
##' 
##' as.ev(data)
##' 
##' @export
setGeneric("as.ev", function(x,...) {
  standardGeneric("as.ev")
})

##' @rdname as.ev
##' @export
setMethod("as.ev", "data.frame", function(x,keep_id=TRUE,clean = FALSE,...) {
  
  if(nrow(x)==0) {
    return(new("ev",data=data.frame()))
  }
  
  x <- as.data.frame(x)
  
  convert <- c("TIME", GLOBALS[["CARRY_TRAN_UC"]])
  upper <- intersect(convert,names(x))
  
  if(length(upper) > 0) {
    where <- match(upper, names(x))
    names(x)[where] <- tolower(names(x)[where])
  }
  
  if(!has_name("cmt",x)) {
    x[["cmt"]] <- 1 
  }
  
  if(!has_name("time", x)) {
    x[["time"]] <- 0 
  }
  
  if(!has_name("evid", x)) {
    x[["evid"]] <- 1 
  } else {
    x[["evid"]] <- na2zero(x[["evid"]])
    x <- x[x[["evid"]] != 0,] 
    if(nrow(x)==0) {
      wstop("no dosing events found; could not coerce to ev object") 
    }
  }
  
  if(has_ID(x) & !keep_id) x[,"ID"] <- NULL
  
  if(clean) {
    keep <- c("ID", GLOBALS[["CARRY_TRAN_LC"]])
    keep <- intersect(keep, names(x))
    x <- x[,keep]
  }
  
  x <- finalize_ev(x)
  
  new("ev", data=x)
})

##' @rdname as.ev
##' @export
setMethod("as.ev", "ev", function(x,...) {
  x
})

check_ev <- function(x) {
  x <- as.data.frame(x)
  if(!has_name("ID", x)) x[["ID"]] <- 1
  return(x)
}

collect_ev <- function(...) {
  x <- list(...)
  tran <- c("ID","time", "cmt", "evid",  
            "amt", "ii", "addl", "rate", "ss")
  x <- lapply(x,check_ev)
  y <- lapply(x, "[[","ID")
  mx <- sapply(y,function(xx) length(unique(xx)))
  mx <- cumsum(c(0,mx[-length(mx)]))
  y <- mapply(y,mx, FUN=function(yi,mxi) return(yi+mxi), SIMPLIFY=FALSE)
  x <- bind_rows(x)
  x[["ID"]] <- unlist(y,use.names=FALSE)
  tran <- intersect(tran,names(x))
  what <- names(x) %in% tran
  
  x <- mutate_at(x,which(what),list(~na2zero(.)))
  
  na.check <- which(!what)
  
  if(length(na.check) > 0) {
    if(any(is.na(unlist(x[,na.check])))) {
      warning("missing values in some columns",call.=FALSE)
    }
  }
  x <- dplyr::select(x,c(match(tran,names(x)),seq_along(names(x))))
  
  if(!any(c("time", "TIME") %in% names(x))) {
    wstop("no time or TIME column in the data set") 
  }
  
  if(!any(c("cmt", "CMT") %in% names(x))) {
    wstop("no cmt or CMT column in the data set") 
  }
  
  if(!has_ID(x)) {
    wstop("no ID column in the data set")
  }
  return(x)
}


##' Operations for ev objects
##'
##' @param e1 object on left hand side of operator (lhs)
##' @param e2 object on right hand side of operator (rhs)
##' @name ev_ops
##' 
##' @aliases +,ev,ev-method
##' @docType methods
##'
##' @details
##' All operations involving \code{\link[=mrgmod-class]{mrgmod}} 
##' objects have been deprecated.
##'
##' @rdname ev_ops
##' @keywords internal
setMethod("+", signature(e1="ev", e2="ev"), function(e1,e2) {
  return(add.ev(e1,e2))
})

##' @rdname ev_ops
##' @export
##' @keywords internal
setGeneric("%then%", function(e1,e2) standardGeneric("%then%"))

##' @rdname ev_ops
##' @export
##' @keywords internal
setMethod("%then%",c("ev", "ev"), function(e1,e2) {
  left <- as.data.frame(e1)
  if(!has_name("ii",left) | !has_name("addl",left)) {
    stop("both ii and addl are required in lhs",call.=FALSE)
  }
  y <- max(with(left, time + ii*addl + ii))
  e2@data$time <- y
  e1 + e2
})

##' @rdname ev_ops
##' @export
##' @keywords internal
setMethod("+", c("ev", "numeric"), function(e1, e2) {
  e1@data$time <- e1@data$time + e2
  e1
})

##' @param x an ev object
##' @param recursive not used
##' @param ... other ev objects to collect
##' 
##' @rdname ev_ops
##' @export
setMethod("c", "ev", function(x,...,recursive=TRUE) {
  y <- list(...)
  if(length(y)==0) return(x)
  for(i in seq_along(y)) {
    x <- add.ev(x,y[[i]])
  }
  return(x)
})

add.ev <- function(e1,e2) {
  
  short <- setdiff(names(e1@data), names(e2@data))
  long <- setdiff(names(e2@data), names(e1@data))
  
  if(any(short=="ID") | any(long=="ID")) {
    stop("ID found in one ev object but not the other")
  }
  
  if(length(short)>0) {
    add <- as.list(rep(0,length(short)))
    names(add) <- short
    e2@data <- cbind(e2@data, add)
  }
  
  if(length(long) > 0) {
    add <- as.list(rep(0, length(long)))
    names(add) <- long
    e1@data <- cbind(e1@data, add)
  }
  e1@data <- as.data.frame(bind_rows(e1@data, e2@data))
  
  if(has_name("ID", e1@data)) {
    e1@data<- e1@data[order(e1@data$ID, e1@data$time),]
  } else {
    e1@data<- e1@data[order(e1@data$time),]
  }
  return(e1)
}



##' Replicate an event object
##' 
##' An event sequence can be replicated a certain number of
##' times in a certain number of IDs.
##' 
##' @param x event object
##' @param ID numeric vector if IDs
##' @param n passed to \code{\link{ev_repeat}}
##' @param wait passed to \code{\link{ev_repeat}}
##' @param as.ev if \code{TRUE} an event object is returned
##' @param id deprecated; use \code{ID} instead
##' 
##' @seealso \code{\link{ev_repeat}}
##' 
##' @examples
##' 
##' e1 <- c(ev(amt=100), ev(amt=200, ii=24, addl=2, time=72))
##' 
##' ev_rep(e1, 1:5)
##' 
##' @return
##' A single data.frame or event object as 
##' determined by the value of \code{as.ev}.
##' 
##' @export
ev_rep <- function(x, ID = 1, n = NULL, wait = 0, as.ev = FALSE, id = NULL) {
  if(!missing(id)) {
    warning("id argument is deprecated; use ID instead")
    ID <- id
  }
  x <- as.data.frame(x) 
  x <- EXPAND_EVENTS(0,numeric_data_matrix(x),as.numeric(ID))
  x <- as.data.frame(x)
  if(!is.null(n)) {
    if(n  > 1) {
      x <- ev_repeat(x,n=n,wait=wait)
    }
  }
  if(as.ev) return(as.ev(x))
  return(x)
} 

##' Repeat a block of dosing events
##' 
##' @param x event object or dosing data frame
##' @param n number of times to repeat
##' @param wait time to wait between repeats
##' @param as.ev if \code{TRUE}, an event object is
##' returned; otherwise a data.frame is returned
##' 
##' @return 
##' See \code{as.ev} argument.
##' 
##' @export
ev_repeat <- function(x,n,wait=0,as.ev=FALSE) {
  x <- as.data.frame(x)
  if(!exists("ii", x)) {
    x["ii"] <- 0
  }
  if(!exists("addl", x)) {
    x["addl"] <- 0
  }
  start <- x[1,"time"]
  end <- x$time + x$ii*x$addl + x$ii
  end <- max(end) + wait
  out <- vector("list", n)
  for(i in seq_len(n)) {
    nxt <- x
    nxt$time <- start + nxt$time + end*(i-1)
    out[[i]] <- nxt
  }
  out <- bind_rows(out)
  if(exists("ID", out)) {
    out <- arrange__(out, c("ID", "time")) 
  }
  if(as.ev) {
    return(as.ev(out))
  } else {
    return(out) 
  }
}



##' Schedule a series of event objects
##' 
##' @param ... event objects or numeric arguments named \code{wait}
##' @param ID numeric vector of subject IDs
##' @param .dots a list of event objects that replaces \code{...}
##' @param id deprecated; use \code{ID}
##' 
##' @details
##' The doses for the next event line start after 
##' all of the doses from the previous event line plus 
##' one dosing interval from the previous event line (see
##' examples).  
##' 
##' When numerics named \code{wait} are mixed in with the 
##' event objects, a period with no dosing activity is 
##' incorporated into the sequence, between the adjacent 
##' dosing event objects.  Values for \code{wait} can
##' be negative.
##' 
##' Values for \code{time} in any event object act like
##' a prefix time spacer wherever that event 
##' occurs in the event sequence (see examples).
##' 
##' @examples
##' 
##' e1 <- ev(amt=100, ii=12, addl=1)
##' 
##' e2 <- ev(amt=200)
##' 
##' seq(e1, e2)
##' 
##' seq(e1, .ii = 8, e2)
##' 
##' seq(e1, wait = 8, e2)
##' 
##' seq(e1, .ii = 8, e2, ID = 1:10)
##' 
##' ev_seq(.ii = 12, e1, .ii = 120, e2, .ii = 120, e1)
##' 
##' seq(ev(amt=100, ii=12), ev(time=8, amt=200))
##'
##' @details
##' Use the generic \code{\link{seq}} when the first argument 
##' is an event object.  If a waiting period is the 
##' first event, you will need to use \code{ev_seq}.  When 
##' an event object has multiple rows, the end time for 
##' that sequence is taken to be one dosing interval 
##' after the event that takes place on the last 
##' row of the event object. 
##' 
##' @return
##' A single event object.
##' 
##' @export
ev_seq <- function(..., ID = NULL, .dots = NULL, id = NULL) {
  
  if(!missing(id)) {
    warning("id argument is deprecated; using ID instead.")
    ID <- id
  }
  
  evs <- list(...)
  
  if(is.list(.dots)) {
    evs <- .dots 
  }
  out <- vector("list", length(evs))
  if(is.null(names(evs))) {
    names(evs) <- rep(".", length(evs))
  }
  start <- 0
  .ii <- 0
  ii <- 0
  for(i in seq_along(out)) {
    if(names(evs)[i]==".ii") {
      .ii <- evs[[i]]
      evs[[i]] <- list()
      next
    }
    if(names(evs)[i]=="wait") {
      start <- start + eval(evs[[i]])
      evs[[i]] <- list()
      next
    }
    e <- as.data.frame(evs[[i]])
    if(is.null(e[["ii"]])) {
      e[["ii"]] <- 0
    }
    if(is.null(e[["addl"]])) {
      e[["addl"]] <- 0
    }
    after <-  ifelse(is.null(e[[".after"]]), 0, e[[".after"]])
    e[["time"]] <- e[["time"]] + start + ifelse(.ii > 0, .ii, ii)
    elast <- slice(e, nrow(e))
    start <- 
      elast[["time"]] + 
      after + 
      elast[["ii"]]*elast[["addl"]] 
    out[[i]] <- e
    .ii <- 0
    ii <- elast[["ii"]]
  }
  out <- bind_rows(out) 
  out[[".after"]] <- NULL
  if(exists("rate", out)) {
    out[["rate"]] <- na2zero(out[["rate"]])
  }
  if(exists("ss",out)) {
    out[["ss"]] <- na2zero(out[["ss"]]) 
  }
  if(is.numeric(ID)) {
    out <- ev_rep(out,ID)
  }
  as.ev(as.data.frame(out))
}

##' @export
##' @rdname ev_seq
seq.ev <- function(...) {
  ev_seq(...) 
}
