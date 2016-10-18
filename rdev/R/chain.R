## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


##' Functions for chaining commands together.
##'
##' Use these functions with chaining commands togehter with the %>% operator.
##'
##' @name chain
##' @details
##' Other functions that may be used in the chain of commands include: 
##' \code{\link{param}}, \code{\link{init}}, \code{\link[mrgsolve]{update}},
##' \code{\link{ev}}
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

# SEE ALSO: data_set and idata_set

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
  
  x@args <- merge(x@args, list(Request=as_character_args(match.call()[-1])), open=TRUE)
  return(x)
})

##' @rdname Req
##' @export
setGeneric("req", function(x,...) standardGeneric("req"))

##' @export
##' @rdname Req
setMethod("req", "mrgmod", function(x,...) {
  x@args <- merge(x@args, list(request=as.character(match.call()[-1])), open=TRUE)
  return(x)
})


##' Set the \code{carry.out} argument for \code{mrgsim}.
##'
##'
##' @param x model object
##' @param ... passed along
##' @export
carry.out <- function(x,...) {
  x@args <- merge(x@args, list(carry.out=as_character_args(match.call()[-1])), open=TRUE)
  return(x)
}

##' Set the \code{carry.out} argument for \code{mrgsim}.
##' 
##' @param x model object
##' @param ... passed along
##' @export
##' 
##' @examples
##' data(extran3)
##' 
##' mod <- mrgsolve:::house()
##' 
##' head(extran3)
##' 
##' mod %>% 
##'   data_set(extran3, ID == 1) %>% 
##'   carry_out(CL,KA,amt,ii) %>% 
##'   mrgsim
##' 
carry_out <- function(x,...) {
  x@args <- merge(x@args, list(carry.out=as_character_args(match.call()[-1])), open=TRUE)
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
  x@args <- merge(x@args, list(tscale=value), open=TRUE)
  return(x)
}



##' Set the \code{obsonly} argument for \code{mrgsim}.
##'
##' @param x model object
##' @param value the value for \code{obsonly}
##' @param ... passed along
##' @export
obsonly <- function(x,value=TRUE,...) {
  x@args <- merge(x@args, list(obsonly=value), open=TRUE)
  return(x)
}
##' Set the \code{obsaug} argument for \code{mrgsim}.
##'
##' @param x model object
##' @param value the value for \code{obsaug}
##' @param ... passed along
##'
##' @export
obsaug <- function(x,value=TRUE,...) {
  x@args <- merge(x@args, list(obsaug=value), open=TRUE)
  x
}


##' Set observation designs for the simulation.
##'
##' This function also allows you to assign different designs to different
##' groups or individuals in a popultion.
##'
##' @param x model object
##' @param descol the \code{idata} column name (\code{character}) for design assignment
##' @param ... \code{tgrid} or \code{tgrids} objects or \code{numeric} vector
##' @param deslist a list of \code{tgrid} or \code{tgrids} objects or \code{numeric} vector to be used in place of ...
##' 
##' @details
##' This setup requires the use of an \code{idata_set}, with individual-level data
##' passed in one \code{ID} per row.  For each \code{ID}, specify a grouping variable
##' in \code{idata} (\code{descol}).  For each unique value of the grouping variable, 
##' make one \code{\link{tgrid}} object and pass them in order as \code{...} or 
##' form them into a list and pass as \code{deslist}.
##' 
##' You must assign the \code{idata_set} before assigning the designs in the 
##' command chain (see the example below).
##'
##' @export
##' 
##' @examples 
##' 
##' peak <- tgrid(0,6,0.1)
##' sparse <- tgrid(0,24,6)
##' 
##' des1 <- c(peak,sparse)
##' des2 <- tgrid(0,72,4)
##' 
##' 
##' data <- expand.ev(ID = 1:10, amt=c(100,300))
##' data$GRP <- data$amt/100
##' 
##' idata <- data[,c("ID", "amt")]
##' 
##' mod <- mrgsolve:::house()
##' 
##' mod %>%
##'   omat(dmat(1,1,1,1)) %>%
##'   carry_out(GRP) %>%
##'   idata_set(idata) %>%
##'   design("amt", des1, des2) %>%
##'   data_set(data) %>%
##'   mrgsim %>% 
##'   plot(RESP~time|GRP)
##' 
##' 
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
    
    if(!exists("idata", x@args)) {
      stop("Please set idata before specifying designs.")
    }
    
    if(!exists(descol, x@args$idata)) {
      stop(paste0("Column ", descol, " does not exist in idata."))
    }
    
    x@args$idata[,descol] <- as.integer(as.factor(x@args$idata[,descol]))
    
  } else {
    
    if(length(des) > 1) {
      warning("Multiple designs specified but no idata key; only the first design will be used.",call.=FALSE)
    }
  }
  
  x@args <- merge(x@args, list(descol=descol, deslist=des),open=TRUE)
  
  x

}







