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


##' @rdname events
##' @param evid event ID
##' @param ID subject ID
##' @param time event time
##' @param replicate logical; if \code{TRUE}, events will be replicated for each individual in \code{ID}
##' @param cmt compartment
##' @param until the expected maximum \bold{observation} time for this regimen
##' @param realize_addl if \code{FALSE} (default), no change to \code{addl} doses.  If \code{TRUE}, 
##' \code{addl} doses are made explicit with \code{\link{realize_addl}}.
##' @export
setMethod("ev", "missing", function(time=0, evid=1, ID=numeric(0), 
                                    cmt=1, replicate=TRUE, until=NULL,
                                    realize_addl=FALSE,...) {
  
  if(length(match.call())==1) { 
    return(new("ev", data=data.frame()[0,]))
  }
  
  if(any(evid==0)) stop("evid cannot be 0 (observation)")
  
  data <-
    as.data.frame(list(...)) %>%
    dplyr::mutate(time=time,cmt=cmt,evid=evid) %>%
    as.data.frame
  
  if(!has_name("amt",data)) stop("amt is required input.", call.=FALSE)
  
  if(!missing(until)) {
    if(!has_name("ii", data)) stop("ii is required when until is specified", call.=FALSE)
    data["addl"] <- ceiling((data["time"] + until)/data["ii"])-1
  }
  
  if(length(ID) > 0) {
    ID <- seq_along(unique(ID))
    if(!is.numeric(ID)) stop("ID must be numeric")
    
    if(replicate) {
      if(any(!is.numeric(data))) {
        data <- as.list(data)
        data <- lapply(data, unique)
        data <- do.call("expand.grid", 
                        c(list(ID=ID,stringsAsFactors=FALSE),data))
        data <- data %>% dplyr::arrange(ID,time)
        rownames(data) <- NULL
      } else {
        data <- data.frame(.Call(mrgsolve_EXPAND_EVENTS, 
                                 PACKAGE="mrgsolve", 
                                 match("ID", colnames(data),0), 
                                 data.matrix(data), 
                                 ID))
      }
      
    } else {
      if(length(ID)!=nrow(data)) { 
        stop("Length of ID does not match number of events while replicate = FALSE", call.=FALSE)
      }
      data["ID"] <- ID
    }
    data <- data %>% shuffle(c("ID", "time", "cmt"))
  } else {
    data <- data %>% shuffle(c("time", "cmt"))
  }
  if(realize_addl) data <- realize_addl(data)
  return(new("ev", data=data))
})

##' @rdname events
##' @export
setMethod("ev", "ev", function(x, realize_addl=FALSE,...) {
  if(realize_addl) {
    return(realize_addl(x))
  } else {
    return(x)
  }
})

##' @param nid if greater than 1, will expand to the appropriate 
##' number of individuals
##' @rdname events
##' @export
setMethod("as.ev", "data.frame", function(x,nid=1,...) {
  if(nrow(x)==0) return(new("ev",data=data.frame()))
  if(!all(c("cmt", "time") %in% names(x))) stop("cmt, time are required data items for events.")
  if(nid > 1) x <- data.frame(.Call(mrgsolve_EXPAND_EVENTS, 
                                    match("ID", colnames(x),0), 
                                    data.matrix(x),
                                    c(1:nid)))
  new("ev",data=x)
})

##' @rdname events
##' @export
setMethod("as.matrix", "ev", function(x,...) as.matrix(as.data.frame(x,stringsAsFactors=FALSE),...))

##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
##' 
##' @rdname events
##' @export
setMethod("as.data.frame", "ev", function(x,row.names=NULL,optional=FALSE,...) {
  as.data.frame(x@data,row.names,optional,stringsAsFactors=FALSE,...)
})

##' Create a simulatinon data set from ev objects.
##'
##'
##' @param x ev objects
##' @param ... more ev objects
##' 
##' @details
##' The goal is to take a series of event objects and combine them 
##' into a single data set that can be passed to \code{\link{data_set}}.  
##' Each event object is added to the data frame as an \code{ID} 
##' or set of \code{ID}s  that are distinct from the  \code{ID}s 
##' in the other event objects. Note that including \code{ID} 
##' argument to the \code{\link{ev}} call where \code{length(ID)} 
##' is greater than one will render that set of 
##' events for all of \code{ID}s that are requested.
##'
##' To get a data frame with one row (event) per \code{ID} 
##' look at \code{\link{expand.ev}}.
##' 
##' @return a data frame suitable for passing into \code{\link{data_set}}
##'
##' @examples
##'
##' as_data_set(ev(amt=c(100,200), cmt=1, ID=1:3),
##'             ev(amt=300, time=24, ID=1:2),
##'             ev(amt=1000, ii=8, addl=10, ID=1:3))
##'
##' # Instead of this, use expand.ev
##' as_data_set(ev(amt=100), ev(amt=200),ev(amt=300))
##'
##' @rdname as_data_set
##' @export
setGeneric("as_data_set", function(x,...) standardGeneric("as_data_set"))

##' @rdname as_data_set
setMethod("as_data_set","ev", function(x,...) {
  do.call(collect_ev,c(list(x),list(...)))
})

##' @param object passed to show
##' @rdname events
##' @export
setMethod("show", "ev", function(object) {
  cat("Events:\n")
  print(as.data.frame(object))
  return(invisible(NULL))
})

check_ev <- function(x) {
  x <- as.data.frame(x)
  if(!has_name("ID", x)) x[["ID"]] <- 1
  return(x)
}
na2zero <- function(x) {
  x[is.na(x)] <- 0
  x
}

collect_ev <- function(...) {
  tran <- c("ID","time", "cmt", "evid",  "amt", "ii", "addl", "rate", "ss")
  x <- lapply(list(...),check_ev)
  y <- lapply(x, "[[","ID")
  mx <- sapply(y,function(xx) length(unique(xx)))
  mx <- cumsum(c(0,mx[-length(mx)]))
  y <- mapply(y,mx, FUN=function(yi,mx) return(yi+mx), SIMPLIFY=FALSE)
  x <- dplyr::bind_rows(x) %>% dplyr::mutate(ID = unlist(y,use.names=FALSE))
  tran <- intersect(tran,names(x))
  what <- names(x) %in% tran
  x <- dplyr::mutate_each(x,dplyr::funs(na2zero),which(what))
  na.check <- which(!what)
  if(length(na.check) > 0) {
    if(any(is.na(unlist(x[,na.check])))) {
      warning("Missing values in some columns.",call.=FALSE)
    }
  }
  x <- x %>% dplyr::select(c(match(tran,names(x)),seq_along(names(x))))
  return(x)
}




##' Operations for ev objects.
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
setMethod("+", signature(e1="ev", e2="ev"), function(e1,e2) {
  return(add.ev(e1,e2))
})

##' @rdname ev_ops
##' @export
setGeneric("%then%", function(e1,e2) standardGeneric("%then%"))

##' @rdname ev_ops
##' @export
setMethod("%then%",c("ev", "ev"), function(e1,e2) {
  left <- as.data.frame(e1)
  if(!has_name("ii",left) | !has_name("addl",left)) {
    stop("Both ii and addl are required in lhs",call.=FALSE)
  }
  y <- max(with(left, time + ii*addl + ii))
  e2@data$time <- y
  e1 + e2
})

##' @rdname ev_ops
##' @export
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

plus.ev <- function(e1,e2) {
  
  data1 <- as.data.frame(events(e1))
  data2 <- as.data.frame(e2)
  
  if(nrow(data1)==0) {
    e1@events <- e2
    return(e1)
  }
  
  short <- setdiff(names(data1), names(data2))
  long <- setdiff(names(data2), names(data1))
  
  if(any(short=="ID") | any(long=="ID")) {
    stop("ID found in one object but not the other")
  }
  
  if(length(short)>0) {
    add <- as.list(rep(0,length(short)))
    names(add) <- short
    data2 <- cbind(data2, add)
  }
  
  if(length(long) > 0) {
    add <- as.list(rep(0, length(long)))
    names(add) <- long
    data1 <- cbind(data1, add)
  }
  
  data1 <- as.data.frame(dplyr::bind_rows(data1, data2))
  
  if(has_name("ID", data1)) {
    data1<- data1[order(data1$ID, data1$time),]
  } else {
    data1<- data1[order(data1$time),]
  }
  
  e1@events <- as.ev(data1)
  
  return(e1)
}

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
  e1@data <- as.data.frame(dplyr::bind_rows(e1@data, e2@data))
  
  if(has_name("ID", e1@data)) {
    e1@data<- e1@data[order(e1@data$ID, e1@data$time),]
  } else {
    e1@data<- e1@data[order(e1@data$time),]
  }
  return(e1)
}

##' Replicate a list of events into a data set.
##' 
##' @param l list of event objects
##' @param idata an idata set (one ID per row)
##' @param evgroup the character name of the column in \code{idata} that specifies event object to implement
##' @param join if \code{TRUE}, join \code{idata} to the data set before returning.
##' 
##' 
##' @examples
##' ev1 <- ev(amt=100)
##' ev2 <- ev(amt=300, rate=100, ii=12, addl=10)
##' 
##' idata <- data.frame(ID=1:10) 
##' idata$arm <- 1+(idata$ID %%2)
##' 
##' assign_ev(list(ev1,ev2),idata,"arm",join=TRUE)
##' 
##' @export
assign_ev <- function(l,idata,evgroup,join=FALSE) {
  
  idata <- as.data.frame(idata)
  
  if(!("ID" %in% colnames(idata))) {
    stop("ID column missing from idata set.", call.=FALSE) 
  }
  
  cols <- c("ii", "addl", "evid", "rate","ss","cmt", "time", "amt")
  
  zeros <- matrix(rep(0,length(cols)),nrow=1,
                  dimnames=list(NULL,cols))
  
  l <- lapply(l,as.matrix)
  
  ucols <- unique(unlist(sapply(l,colnames,simplify=FALSE,USE.NAMES=FALSE)))
  
  if(!all(ucols %in% cols)) {
    invalid <- setdiff(ucols,cols)
    invalid <- paste(invalid,collapse=", ")
    stop("invalid event data items found: ", invalid, call.=FALSE)
  }
  
  for(i in seq_along(l)) {
    miss <- setdiff(ucols,colnames(l[[i]]))
    if(length(miss) > 0) {
      l[[i]] <- cbind(l[[i]],zeros[rep(1,nrow(l[[i]])),miss,drop=FALSE])
    }
  }
  
  l <- lapply(l,function(x) {
    x[,colnames(l[[1]]),drop=FALSE]
  })
  
  evgroup <- idata[,evgroup]
  uevgroup <- unique(evgroup)
  evgroup <- match(evgroup,uevgroup)
  
  if(length(l) != length(uevgroup)) {
    stop("For this idata set, please provide exactly ", 
         length(uevgroup), 
         " event objects.",call.=FALSE)
  }
  
  x <- do.call(rbind,l[evgroup]) 
  dimnames(x) <- list(NULL,colnames(x))
  x <- as.data.frame(x)
  
  n <- (sapply(l,nrow))[evgroup]
  ID <- rep(idata[["ID"]],times=n)
  x[["ID"]] <- ID
  
  if(join) {
    nu <- sapply(idata,is.numeric)
    x <- dplyr::left_join(x,idata[,nu,drop=FALSE],by="ID") 
  }
  
  return(x)
  
}

##' Schedule dosing events on days of the week.
##' 
##' This function lets you schedule doses on specific 
##' days of the week, allowing you to create dosing 
##' regimens on Monday/Wednesday/Friday, or Tuesday/Thursday,
##' or every other day (however you want to define that) etc.
##' 
##' @param ev an event object
##' @param days comma- or space-separated character string of valid days of the
##' the week (see details)
##' @param addl additional doses to administer
##' @param ii inter-dose interval; intended use is to keep this at the 
##' default value
##' @param unit time unit; the function can only currently handle hours or days
##' @param ... event objects named by one the valid days of the week (see details)
##' 
##' @details
##' Valid names of the week are: 
##' 
##' \itemize{
##' \item \code{m} for Monday
##' \item \code{t} for Tuesday
##' \item \code{w} for Wednesday
##' \item \code{th} for Thursday
##' \item \code{f} for Friday
##' \item \code{sa} for Saturday
##' \item \code{s} for Sunday
##' }
##' 
##' The whole purpose of this function is to schedule doses on specific
##' days of the week, in a repeating weekly schedule.  Please do use caution 
##' when changing \code{ii} from it's default value.
##' 
##' @examples
##' 
##' # Monday, Wednesday, Friday x 4 weeks
##' ev_days(ev(amt=100), days="m,w,f", addl=3)
##' 
##' # 50 mg Tuesdays, 100 mg Thursdays x 6 months
##' ev_days(t=ev(amt=50), th=ev(amt=100), addl=23)
##' 
##' 
##' @export
ev_days <- function(ev=NULL,days="",addl=0,ii=168,unit=c("hours", "days"),...) {
  
  unit <- match.arg(unit)
  
  max.time <- 24
  start <- c(m=0,t=24,w=48,th=72,f=96,sa=120,s=144)
  
  if(unit=="days") {
    max.time <- 1
    start <- c(m=0,t=1,w=2,th=3,f=4,sa=5,s=6)
    if(missing(ii)) ii <- 7
  }
  if(!is.null(ev)) {
    if(missing(days)) {
      stop("days argument must be supplied with ev argument.",call.=FALSE) 
    }
    days <- cvec_cs(days)
    if(!all(days %in% names(start))) {
      valid <- paste(names(start),collapse=",")
      err <- paste0("invalid day; valid days are: ", valid)
      stop(err,call.=FALSE)
    }
    evs <- lapply(days,function(i) {
      return(ev)
    })
    names(evs) <- days
  } else {
    args <- list(...)
    evs <- args[names(args) %in% names(start)]
    days <- names(evs)
  }
  if(length(evs)==0) {
    stop("no events were found.", call.=FALSE) 
  }
  evs <- lapply(evs,as.data.frame)
  for(d in days) {
    if(any(evs[[d]]$time > max.time)) {
      warning("not expecting time values greater than 24 hours or 1 day.",call.=FALSE)  
    }
    evs[[d]]$time <- evs[[d]]$time + start[d] 
  }
  evs <- bind_rows(evs)
  evs$ii <- ii
  if(addl > 0) evs$addl <- addl
  if("ID" %in% names(evs)) {
    return(as.data.frame(dplyr::arrange(evs,ID,time)))
  } else {
    return(as.data.frame(dplyr::arrange(evs,time)))
  }
}

##' Make addl doses explicit in an event object or data set.
##' 
##' @param x a \code{data_set} data frame or an \code{ev} object (see details)
##' @param ... not used
##' @details
##' Required data elements: \code{addl} and \code{ii}.
##' @export
realize_addl <- function(x,...) UseMethod("realize_addl")
##' @rdname realize_addl
##' @export
realize_addl.data.frame <- function(x,...) {
  
  iicol <- which(names(x) %in% c("II", "ii"))[1]
  addlcol <- which(names(x) %in% c("ADDL", "addl"))[1]
  timecol <- which(names(x) %in% c("TIME", "time"))[1]
  if(is.na(iicol)) stop("missing ii/II column.", call.=FALSE)
  if(is.na(addlcol)) stop("missing addl/ADDL column.", call.=FALSE)
  if(is.na(timecol)) stop("missing time/TIME column.", call.=FALSE)
  
  add <- which(x[[addlcol]] > 0)
  addl <- lapply(add, function(i) {
    df <- x[i,,drop=FALSE]
    df <- df[rep(1,df[[addlcol]]),]
    df[[timecol]] <- df[[timecol]] + df[[iicol]]*seq(1,df[[addlcol]][1])
    df
  }) 
  df <- bind_rows(x,bind_rows(addl))
  df[[addlcol]] <- 0
  df[[iicol]] <- 0
  if("ID" %in% names(df)) {
    df <- dplyr::arrange(df,ID,time)
  } else {
    df <- dplyr::arrange(df,time)
  }
  df
}
##' @rdname realize_addl
##' @export
realize_addl.ev <- function(x,...) {
  x@data <- realize_addl(x@data)
  return(x)
}





