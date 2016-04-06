## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

##' @include utils.R
NULL

##'
##' Create a simtime object.
##'
##' simtime objects allow the user to specify simulation start and end times, along with the simulation time step.
##'
##' @rdname tgrid
##' @name tgrid
##'
##'
##' @param start simulation start time
##' @param end simulation end time
##' @param delta simulation time step
##' @param add addition simulation times
##' @param .offset the resulting set of times will be adjusted by this amount
##' @param .scale the resulting set of times will be scaled by this factor
##' @param ... passed on to other methods
##' @export
##' @examples
##'
##' peak <- tgrid(0,6,0.2)
##' sparse <- tgrid(0,24,4)
##'
##' day1 <- c(peak,sparse)
##'
##' design <- c(day1, day1+72, day1+240)
##'
##' mod <- mrgsolve:::house()
##'
##' out <- mod %>% ev(amt=1000, ii=24, addl=10) %>% mrgsim(tgrid=design)
##'
##' plot(out,CP~., type='b')
##'
tgrid <-  function(start=0,end=24,delta=1,add=numeric(0),.offset=0, .scale=1,...) {
    new("tgrid", start=start, end=end, delta=delta, add=add, offset=.offset, scale=.scale)
}


tgrids <- function(...) {
    new("tgrids", data=list(...))
}

##' @export
##' @rdname tgrid
##' @param x mrgmod object
##' @param recursive not used
setMethod("c", "tgrid", function(x,..., recursive=FALSE) {

    x <- c(list(x), list(...))

    singles <- sapply(x, inherits, what="tgrid")

    multis <- sapply(x, inherits, what="tgrids")

    x <- c(x[singles], unlist(lapply(x[multis], function(y) y@data)))

    do.call("tgrids", x)
})

##' @export
##' @rdname tgrid
setMethod("c", "tgrids", function(x,...) {
    do.call("c",c(x@data, list(...)))
})



#' @export
#' @rdname tgrid
setMethod("stime", "tgrid", function(x,...) {
    (render_time(x) + x@offset) * x@scale
})

#' @export
#' @rdname tgrid
setMethod("stime", "tgrids", function(x,...) {
    sort(unique(unlist(lapply(x@data,stime))))
})

##' ##' @export
##' @rdname tgrid
setMethod("stime", "numeric", function(x,...) {
    sort(unique(x))
})



##' @rdname tgrid
##' @name tgrid_+_numeric
##' @docType methods
##' @aliases +,tgrid,numeric-method
##' @param e1 tgrid or tgrids object
##' @param e2 numeric value
setMethod("+", signature(e1="tgrid", e2="numeric"), function(e1,e2) {
    e1@offset <- e1@offset + e2
    e1
})

##' @rdname tgrid
##' @name tgrid_*_numeric
##' @docType methods
##' @aliases *,tgrid,numeric-method
setMethod("*", signature(e1="tgrid", e2="numeric"), function(e1,e2) {
    e1@scale <- e2
    e1
})


##' @rdname tgrid
##' @name tgrids_+_numeric
##' @docType methods
##' @aliases +,tgrids,numeric-method
setMethod("+", signature(e1="tgrids", e2="numeric"), function(e1,e2) {
    e1@data <- lapply(e1@data, function(x) {
        x@offset <- x@offset + e2
        x
    })
    e1
})

##' @rdname tgrid
##' @name tgrids_*_numeric
##' @docType methods
##' @aliases *,tgrids,numeric-method
setMethod("*", signature(e1="tgrids", e2="numeric"), function(e1,e2) {
    e1@data <- lapply(e1@data, function(x) {
        x@scale <- e2
        x
    })
    e1
})


##' @export
##' @rdname tgrid
##' @param object passed to show
setMethod("show", "tgrid", function(object) {
    x <- stime(object)
    min <- min(x)
    max <- max(x)

    cat("start: ", object@start, " ")
    cat("end:   ", object@end, " ")
    cat("delta: ", object@delta, " ")
    cat("offset:", object@offset, " ")
    cat("min:   ", min, "  ")
    cat("max:   ", max, "\n")
})


##' @export
##' @rdname tgrid
setMethod("show", "tgrids", function(object) {

    lapply(object@data, function(x) {

        show(x)
        cat("--------\n")
    })
})






