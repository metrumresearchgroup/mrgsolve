## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

##' @include classes.R


##' @title Get model events
##'
##' @description
##' An accessor function for the \code{events} model attribute.
##'
##' Events can either be specified when the model object is created (with
##' \code{mrgmod}) or by updating an existing model object (with \code{update}).
##'
##' @return Returns a user-defined data frame of events that should be suitable
##' for passing into \code{lsoda}.  If events are stored as a data frame,
##' \code{events} returns the data frame.  If events are stored as a function
##' that generates the data frame, \code{events} calls the function and passes
##' return back to the user.
##'
##' @param x mrgmodel object
##' @param ... passed on
##' @export events
##' @author Kyle Baron
##' @keywords events
setGeneric("events", function(x,...) standardGeneric("events"))
##' @rdname events
##' @export
setMethod("events", "mrgmod", function(x,...) {

    args <- list(...)
    if(length(args)>0) return(update(x,events=ev(...)))

    x@events
})
##' @export
##' @rdname events
setMethod("events", "mrgsims", function(x,...) events(mod(x)))

## REMOVE
#checkevents.mrgmod <- function(x) {return(TRUE)}
#null_ev <- list(time=0,cmt=0,amt=0, ss=0, ii=0, addl=0,rate=0, evid=0)
#null_ev_col <- length(null_ev)
#null_ev_names <- names(null_ev)
#null_ev_df <- as.data.frame(null_ev)[0,]

##' @title Create event object
##' @return an object of class ev
##'
##' @details
##' \itemize{
##' \item Required input for creating events objects include \code{time} and \code{cmt}
##' \item If not supplied, \code{evid} is assumed to be 1
##' \item If not supplied, \code{cmt}  is assumed to be 1
##' \item If not supplied, \code{time} is assumed to be 0
##' \item \code{ID} may be specified as a vector
##' \item if replicate is \code{TRUE} (default), thenthe events regimen is replicated for each \code{ID}; otherwise, the number of
##' event rows must match the number of \code{ID}s entered
##' }
##'
##' @export
##' @rdname events
##' @examples
##' mod <- mrgsolve:::house()
##' mod <- mod %>% ev(amt=1000, time=0, cmt=1)
##' events(mod)
##'
##' loading <- ev(time=0, cmt=1, amt=1000)
##' maint <- ev(time=12, cmt=1, amt=500, ii=12, addl=10)
##' loading + maint
##'
##'
##' ev(ID=1:10, cmt=1, time=0, amt=100)
##'
##'
setGeneric("ev", function(x,...) standardGeneric("ev"))
##' @export
##' @rdname events
##' @param evid event ID
##' @param ID subject ID
##' @param time event time
##' @param replicate logical; if \code{TRUE}, events will be replicated for each individual in \code{ID}
##' @param cmt compartment
##' @param until the expected maximum \bold{observation} time for this regimen
setMethod("ev", "missing", function(time=0,evid=1, ID=numeric(0), cmt=1, replicate=TRUE, until=NULL, ...) {


    if(length(match.call())==1) { return(new("ev", data=data.frame()[0,]))}

    if(any(evid==0)) stop("evid cannot be 0 (observation)")



    data <-
        as.data.frame(list(...)) %>%
            dplyr::mutate(time=time,cmt=cmt,evid=evid) %>%
                as.data.frame

    if(!exists("amt",data)) stop("amt is required input.", call.=FALSE)

    if(!missing(until)) {
        if(!exists("ii", data)) stop("ii is required when until is specified", call.=FALSE)
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

##' Create a regimen as event object.
##'
##' @param amt dose amount
##' @param ii dosing interval
##' @param addl additional doses
##' @param ... passed to \code{\link{ev}}
##' @export
##'
##'
Rx <- function(amt,ii,addl,...) {
    ev(amt=amt, ii=ii,addl=addl,...)
}

##' @export
##' @rdname events
setMethod("ev", "ev", function(x,...) return(x))
##' @export
##' @rdname events
setMethod("ev", "mrgmod", function(x,...) {
    update(x,events=ev(...))

})

##' @rdname events
##' @export
setGeneric("as.ev", function(x,...) standardGeneric("as.ev"))


##' @export
##' @param nid if greater than 1, will expand to the appropriate number of individuals
##' @rdname events
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
##' @export
##' @param object passed to show
setMethod("show", "ev", function(object) {
    cat("Events:\n")
    print(as.data.frame(object))
    return(invisible(NULL))
})


check_ev <- function(x) {
  x <- as.data.frame(x)
  if(!exists("ID", x)) x[["ID"]] <- 1
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
  y <- mapply(y,mx, FUN=function(yi,mx) return(yi+mx))
  x <- dplyr::bind_rows(x) %>% dplyr::mutate(ID = unlist(y))
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



