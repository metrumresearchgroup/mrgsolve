## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.



##' @rdname events
##' @param evid event ID
##' @param ID subject ID
##' @param time event time
##' @param replicate logical; if \code{TRUE}, events will be replicated for each individual in \code{ID}
##' @param cmt compartment
##' @param until the expected maximum \bold{observation} time for this regimen
##' @export
setMethod("ev", "missing", function(time=0,evid=1, ID=numeric(0), cmt=1, replicate=TRUE, until=NULL, ...) {
  
  if(length(match.call())==1) { return(new("ev", data=data.frame()[0,]))}
  
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
        data <- do.call("expand.grid", c(list(ID=ID,stringsAsFactors=FALSE),data))
        data <- data %>% dplyr::arrange(ID,time)
        rownames(data) <- NULL
      } else {
        data <- data.frame(.Call("mrgsolve_EXPAND_EVENTS", PACKAGE="mrgsolve", 
                                 match("ID", colnames(data),0), 
                                 data.matrix(data), 
                                 ID))
      }
      
    } else {
      if(length(ID)!=nrow(data)) stop("Length of ID does not match number of events while replicate = FALSE", call.=FALSE)
      data["ID"] <- ID
    }
    data <- data %>% shuffle(c("ID", "time", "cmt"))
  } else {
    data <- data %>% shuffle(c("time", "cmt"))
  }
  return(new("ev", data=data))
})


##' @rdname events
##' @export
setMethod("ev", "ev", function(x,...) return(x))

##' @param nid if greater than 1, will expand to the appropriate number of individuals
##' @rdname events
##' @export
setMethod("as.ev", "data.frame", function(x,nid=1,...) {
  if(nrow(x)==0) return(new("ev",data=data.frame()))
  if(!all(c("cmt", "time") %in% names(x))) stop("cmt, time are required data items for events.")
  if(nid > 1) x <- data.frame(.Call("mrgsolve_EXPAND_EVENTS", PACKAGE="mrgsolve", 
                                    match("ID", colnames(x),0), 
                                    data.matrix(x),
                                    c(1:nid)))
  new("ev",data=x)
})

##' @export
##' @rdname events
setMethod("as.matrix", "ev", function(x,...) as.matrix(as.data.frame(x,stringsAsFactors=FALSE),...))

##' @export
##' @rdname events
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
setMethod("as.data.frame", "ev", function(x,row.names=NULL,optional=FALSE,...) {
  as.data.frame(x@data,row.names,optional,stringsAsFactors=FALSE,...)
})


##' Create a simulatinon data set from ev objects.
##'
##'
##' @export
##' @rdname as_data_set
##' @param x ev objects
##' @param ... more ev objects
##' @return a data frame suitable for passing into \code{\link{data_set}}
##'
##' @details
##' The goal is to take a series of event objects and combine them into a single
##' data set that can be passed to \code{\link{data_set}}.  Each event object
##' is added to the data frame as an \code{ID} or set of \code{ID}s  that are
##' distinct from the  \code{ID}s in the other event objects. Note that including
##' \code{ID} argument to the \code{\link{ev}} call where \code{length(ID)} is greater
##' than one will render that set of events for all of \code{ID}s that are requested.
##'
##' To get a data frame with one row (event) per \code{ID} look at \code{\link{expand.ev}}.
##'
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
##'
setGeneric("as_data_set", function(x,...) standardGeneric("as_data_set"))
##' @rdname as_data_set
setMethod("as_data_set","ev", function(x,...) {
  do.call(collect_ev,c(list(x),list(...)))
})



##' @rdname events
##' @param object passed to show
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
  x <- dplyr::bind_rows(x) %>% dplyr::mutate(ID = unlist(y,use.names=TRUE))
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
##' @rdname ev_ops
##' @aliases +,ev,ev-method
##' @docType methods
##'
##' @details
##' All operations involving \code{\link[=mrgmod-class]{mrgmod}} objects have been deprecated.
##'
setMethod("+", signature(e1="ev", e2="ev"), function(e1,e2) {
  return(add.ev(e1,e2))
})


##' @rdname ev_ops
##' @export
setGeneric("%then%", function(e1,e2) standardGeneric("%then%"))
##' @export
##' @rdname ev_ops
setMethod("%then%",c("ev", "ev"), function(e1,e2) {
  left <- as.data.frame(e1)
  if(!has_name("ii",left) | !has_name("addl",left)) stop("Both ii and addl are required in lhs",call.=FALSE)
  
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
##' @param ... other ev objects to collect
##' @param recursive not used
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



# Add an events object to a mrgmod object.
# 
# @param e1 mrgmod object
# @param e2 events object
plus.ev <- function(e1,e2) {
  
  data1 <- as.data.frame(events(e1))
  data2 <- as.data.frame(e2)
  
  if(nrow(data1)==0) {
    e1@events <- e2
    return(e1)
  }
  
  short <- setdiff(names(data1), names(data2))
  long <- setdiff(names(data2), names(data1))
  
  if(any(short=="ID") | any(long=="ID")) stop("ID found in one object but not the other")
  
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


# Add two events objects.
# 
# @param e1 first events object
# @param e2 second events object
add.ev <- function(e1,e2) {
  
  short <- setdiff(names(e1@data), names(e2@data))
  long <- setdiff(names(e2@data), names(e1@data))
  
  if(any(short=="ID") | any(long=="ID")) stop("ID found in one ev object but not the other")
  
  
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







