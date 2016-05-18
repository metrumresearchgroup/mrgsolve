## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.






##' @include events.R
NULL
##' Operations for ev objects.
##'
##' @rdname mrgsolve_Ops
##' @param e1 object on left hand side of operator (lhs)
##' @param e2 object on right hand side of operator (rhs)
##' @name mrgsolve_Ops
##' @aliases +,ev,ev-method
##' @docType methods
##'
##' @details
##' All operations involving \code{\link[=mrgmod-class]{mrgmod}} objects have been deprecated.
##'
setMethod("+", signature(e1="ev", e2="ev"), function(e1,e2) {
    return(add.ev(e1,e2))
})

##' @export
##' @rdname mrgsolve_Ops
setGeneric("%then%", function(e1,e2) standardGeneric("%then%"))
##' @export
##' @rdname mrgsolve_Ops
setMethod("%then%",c("ev", "ev"), function(e1,e2) {
    left <- as.data.frame(e1)
    if(!exists("ii",left) | !exists("addl",left)) stop("Both ii and addl are required in lhs",call.=FALSE)

    y <- max(with(left, time + ii*addl + ii))

    e2@data$time <- y
    e1 + e2
})

##' @export
##' @rdname mrgsolve_Ops
setMethod("+", c("ev", "numeric"), function(e1, e2) {
    e1@data$time <- e1@data$time + e2
    e1
})


##' @export
##' @rdname mrgsolve_Ops
##' @param x an ev object
##' @param ... other ev objects to collect
##' @param recursive not used
setMethod("c", "ev", function(x,...,recursive=TRUE) {
    y <- list(...)
    if(length(y)==0) return(x)
    for(i in seq_along(y)) {
        x <- add.ev(x,y[[i]])
    }
    return(x)
})



##' Add an events object to a mrgmod object
##' @param e1 mrgmod object
##' @param e2 events object
##' @export
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

    data1 <- rbind_fill(data1, data2)

    if(exists("ID", data1)) {
        data1<- data1[order(data1$ID, data1$time),]
    } else {
        data1<- data1[order(data1$time),]
    }
    e1@events <- as.ev(data1)
    return(e1)
}


##' Add two events objects
##' @param e1 first events object
##' @param e2 second events object
##' @export
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
    e1@data <- rbind_fill(e1@data, e2@data)

    if(exists("ID", e1@data)) {
        e1@data<- e1@data[order(e1@data$ID, e1@data$time),]
    } else {
        e1@data<- e1@data[order(e1@data$time),]
    }
    return(e1)
}





