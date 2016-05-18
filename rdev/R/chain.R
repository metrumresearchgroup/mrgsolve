## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


##' Functions for chaining commands together.
##'
##' Use these functions with chaining commands togehter with the %>% operator.
##'
##' @name chain
##' @details
##' Other functions that may be used in the chain of commands include: \code{\link{param}}, \code{\link{init}}, \code{\link[mrgsolve]{update}},\code{\link{ev}},\code{\link{limit}}
##' or any other function that will take the output of the preceeding command as it's first argument.
##'
##'
##' @examples
##'
##' mod <- mrgsolve:::house()
##'
##' data(exidata)
##' data(exTheoph)
##'
##' out <- mod %>% data_set(exTheoph) %>% mrgsim()
##' out <- mod %>% carry.out(evid) %>% ev(amt=100, cmt=1) %>% mrgsim()
##' out <- mod %>% Req(CP,RESP) %>% mrgsim()
##'
NULL


##' Set the \code{Request} argument for \code{mrgsim}.
##'
##' @param x model object
##' @param ... unquoted names of compartments or tabled items
##' @export
##' @examples
##' mod <- mrgsolve:::house()
##'
##' mod %>% Req(CP,RESP) %>% ev(amt=1000) %>%  mrgsim
##'
setGeneric("Req", function(x,...) standardGeneric("Req"))
##' @export
##' @rdname Req
setMethod("Req", "mrgmod", function(x,...) {

    x@args <- merge(x@args, list(Request=as_character_args(match.call()[-1])), strict=FALSE)
    return(x)
})

##' @rdname Req
##' @export
setGeneric("req", function(x,...) standardGeneric("req"))
##' @export
##' @rdname Req
setMethod("req", "mrgmod", function(x,...) {
    x@args <- merge(x@args, list(request=as.character(match.call()[-1])), strict=FALSE)
    return(x)
})

sub_set <- function(e,x) {
    r <- eval(e,x,parent.frame())
    x[r,,drop=FALSE]
}

##' Set the \code{data} argument for \code{mrgsim}.
##'
##'
##' @export
##' @param x model object
##' @param data data set
##' @param ... passed along
setGeneric("data_set", function(x,data,...) standardGeneric("data_set"))
##' @export
##' @rdname data_set
##' @param subset passed to \code{dplyr::filter_}; retain only certain rows in the data set
##' @param select passed to \code{dplyr::select_}; retain only certain columns in the data set
##'
##'
##' @details
##' Input data sets are \code{R} data frames that can include columns with any valid name, however columns with selected names are recognized by \code{mrgsolve} and incorporated into the simulation.
##'
##' \code{ID} specifies the subject ID and is required for every input data set.
##'
##' When columns have the same name as parameters (\code{$PARAM} in the model specification file), the values in those columns will be used to update the corresponding parameter as the simulation progresses.
##'
##' Input data set may include the following columns related to PK dosing events: \code{time}, \code{cmt}, \code{amt}, \code{rate},
##' \code{ii}, \code{addl}, \code{ss}.  \code{time} and \code{cmt} (and \code{ID}) are required columns in the input data set.  \code{time} is the observation or event time, \code{cmt} is the compartment number (see \code{\link{init}}), \code{amt} is the dosing amount, \code{rate} is the infusion rate, \code{ii} is the dosing interval, \code{addl} specifies additional doses to administer, and \code{ss} is a flag for steady state dosing.  These column names operate similarly to other non-linear mixed effects modeling software, but note that (except for \code{ID}) the column names related to PK dosing must be lower case.
##'
##' Only numeric data can be brought in to the problem.  Any non-numeric data columns will be dropped with warning.
##'
##' See \code{\link{exdatasets}} for different example data sets.
##'
##'
##' @examples
##'
##' data <- expand.ev(ID=1:3, amt=c(10,20))
##'
##' data <- expand.ev(amt=c(10,20), rate=c(1,2))
##'
##'
setMethod("data_set",c("mrgmod", "data.frame"), function(x,data,subset=TRUE,select=TRUE,...) {
    if(exists("data", x@args)) stop("data already has been set.")
    if(!missing(subset)) data <- dplyr::filter_(data,.dots=lazy(subset))
    if(!missing(select)) data <- dplyr::select_(data,.dots=lazy(select))
    ## if(!missing(rename)) {
    ##     rename <- set_altname(as.cvec2(substitute(rename)))
    ##     data <- dplyr::rename_(data,.dots=setNames(as.list(rename[["from"]]),rename[["to"]]))
    ## }
    if(nrow(data) ==0) stop("Zero rows in data after filtering.", call.=FALSE)
    data <- mrgindata(m=x,x=data,...)
    x@args <- merge(x@args,list(data=data), strict=FALSE)
    x
})

##' @export
##' @rdname data_set
setMethod("data_set",c("mrgmod", "ANY"), function(x,data,...) {
    data_set(x,as.data.frame(data),...)
})

##' Set the \code{idata} argument for \code{mrgsim}.
##'
##'
##' @param x model object
##' @param data a data set coercable to data.frame
##' @param ... passed along
##' @export
setGeneric("idata_set", function(x,data,...) standardGeneric("idata_set"))
##' @export
##' @param subset passed to \code{dplyr::filter_}
##' @param select passed to \code{dplyr::select_}
##' @rdname idata_set
setMethod("idata_set",c("mrgmod", "data.frame"), function(x,data,subset=TRUE,select=TRUE,...) {
    if(exists("idata", x@args)) stop("idata has already been set.")
    if(!missing(subset)) data <- filter_(data,.dots=lazy(subset))
    if(!missing(select)) data <- select_(data,.dots=lazy(select))
    if(nrow(data) ==0) stop("Zero rows in idata after filtering.", call.=FALSE)
    x@args <- merge(x@args,list(idata=data), strict=FALSE)
    x
})
##' @export
##' @rdname idata_set
setMethod("idata_set",c("mrgmod", "ANY"), function(x,data,...) {
    idata_set(x,as.data.frame(data),...)
})


##' Set the \code{carry.out} argument for \code{mrgsim}.
##'
##'
##' @param x model object
##' @param ... passed along
##' @export
##'
##'
##'
carry.out <- function(x,...) {
    x@args <- merge(x@args, list(carry.out=as_character_args(match.call()[-1])), strict=FALSE)
    return(x)
}
##' Set the \code{tscale} argument for \code{mrgsim}.
##'
##'
##' @param x model object
##' @param value value by which time will be scaled
##' @param ... passed along
##' @export
tscale <- function(x,value=1,...) {
    x@args <- merge(x@args, list(tscale=value), strict=FALSE)
    return(x)
}



##' Set the \code{obsonly} argument for \code{mrgsim}.
##'
##' @param x model object
##' @param value the value for \code{obsonly}
##' @param ... passed along
##' @export
obsonly <- function(x,value=TRUE,...) {
    x@args <- merge(x@args, list(obsonly=value), strict=FALSE)
    x
}
##' Set the \code{obsaug} argument for \code{mrgsim}.
##'
##' @param x model object
##' @param value the value for \code{obsaug}
##' @param ... passed along
##'
##' @export
obsaug <- function(x,value=TRUE,...) {
    x@args <- merge(x@args, list(obsaug=value), strict=FALSE)
    x
}


##' Set observation designs for the simulation.
##'
##' @param x model object
##' @param descol the \code{idata} column name for design assignment
##' @param ... \code{tgrid} or \code{tgrids} objects or \code{numeric} vector
##' @param deslist a list of \code{tgrid} or \code{tgrids} objects or \code{numeric} vector to be used in place of ...

##'
##' @export
design <- function(x,descol=character(0),...,deslist = list()) {

    descol <- as.character(substitute(descol))

    stopifnot(length(descol) <= 1)

    if(length(deslist) > 0) {
        des <- deslist
    } else {
        des <- list(...)
    }

    des <- des[unlist(lapply(des,inherits,c("tgrid", "tgrids", "numeric")))]

    if(length(des) ==0) stop("No valid tgrid objects found.")

    if(length(descol) ==1) {
        if(!exists("idata", x@args)) stop("Please set idata before specifying designs.")
        if(!exists(descol, x@args$idata)) stop(paste0("Column ", descol, " does not exist in idata."))
        x@args$idata[,descol] <- as.integer(as.factor(x@args$idata[,descol]))
    } else {
        if(length(des) > 1)
            warning("Multiple designs specified but no idata key; only the first design will be used.",call.=FALSE)
    }
    x@args <- merge(x@args, list(descol=descol, deslist=des),strict=FALSE)
    x
}







