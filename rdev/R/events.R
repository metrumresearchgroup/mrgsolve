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

checkevents.mrgmod <- function(x) {return(TRUE)}

null_ev <- list(time=0,cmt=0,amt=0, ss=0, ii=0, addl=0,rate=0, evid=0)
null_ev_col <- length(null_ev)
null_ev_names <- names(null_ev)
null_ev_df <- as.data.frame(null_ev)[0,]

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
##' @importFrom dplyr data_frame as_data_frame select   mutate_ arrange
##' @importFrom lazyeval lazy_dots
setMethod("ev", "missing", function(evid=1, time=0, ID=numeric(0), cmt=1, replicate=TRUE, until=NULL, ...) {


    if(length(match.call())==1) { return(new("ev", data=data.frame()[0,]))}

    if(any(evid==0)) stop("evid cannot be 0 (observation)")

    data <-
        as.data.frame(list(...)) %>%
        dplyr::mutate(evid=evid,time=time,cmt=cmt) %>%
        as.data.frame

    if(!missing(until)) {
        if(!exists("ii", data)) stop("ii is required when until is specified", call.=FALSE)
        data["addl"] <- ceiling((data["time"] + until)/data["ii"])-1
    }

    if(length(ID) > 0) {

        if(!is.numeric(ID)) stop("ID must be numeric")

        if(replicate) {
            if(any(!is.numeric(data))) {
                data <- as.list(data)
                data <- lapply(data, unique)
                data <- do.call("expand.grid", c(list(ID=ID,stringsAsFactors=FALSE),data))
                data <- data %>% dplyr::arrange(ID,time)
                rownames(data) <- NULL
            } else {
                data <- data.frame(.Call("mrgsolve_EXPAND_EVENTS", PACKAGE="mrgsolve", list(), data.matrix(data), ID))
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
##' @rdname events
setMethod("as.ev", "data.frame", function(x,...) {
    if(nrow(x)==0) return(new("ev",data=data.frame()))
    if(!all(c("cmt", "time") %in% names(x))) stop("cmt, time are required data items for events.")
    new("ev",data=x)
})


##' @export
##' @rdname events
setMethod("as.matrix", "ev", function(x,...) as.matrix(as.data.frame(x,stringsAsFactors=FALSE),...))

##' @export
##' @rdname events
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
setMethod("as.data.frame", "ev", function(x,row.names=NULL,optional=FALSE,...) as.data.frame(x@data,row.names,optional,stringsAsFactors=FALSE,...))

##' @rdname events
##' @export
##' @param object passed to show
setMethod("show", "ev", function(object) {
    cat("Events:\n")
    print(as.data.frame(object))
    return(invisible(NULL))
})


