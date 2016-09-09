## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

##' @include check_names.R

null_list <- setNames(list(), character(0))

single.number <- function(x) length(x)==1 & is.numeric(x)

valid_funs <- function(x) {
  x1 <- length(x)==4
  x2 <- identical(names(x), c("main", "ode", "table", "config"))
  if(x1 & x2) return(list(TRUE,""))
  return(list(FALSE, c("Invalid functions specification.", 
                  "This model object is not compatible with the current mrgsolve version.",
                  "Rebuild the model object or upgrade the mrgsolve version.")))
}


valid.numericlist <- function(object) {
  x1 <- all(sapply(object@data,single.number))
  x2 <- all(names(object@data) !="")
  x3 <- !any(grepl("=|\\.",names(object),perl=TRUE))

  x <- x1 & x2 & x3
  if(all(x)) return(TRUE)
  

  out <- c()
  if(!x3) {
    message("Problem with names:")
    cat(paste(names(object), collapse=","))
    out <- c(out, "Invalid names")
  }
  if(!x2) {
    d <- object@data
    d <- d[nchar(names(d))==0]
    message("Parameter values without names:")
    print(d)
    out <- c(out, "All parameters require names")
  }
  if(!x1) {
    out <- c(out, "All parameters must be single numbers") 
  }
  return(out)
  
}

valid.matlist <- function(object) {
  
  labels <- names(object@data)[names(object@data) != "..."]
  
  x1 <- all(sapply(object@data, is.matrix))
  x2 <- all(sapply(object@data, is.numeric))
  
  x3 <- (!any(duplicated(labels))) | length(labels)==0
  
  x4 <- all(sapply(object@data, det)>=0)
  
  x5 <- mapply(object@data, object@labels, FUN=function(x,y) {
    nrow(x) == length(y)
  }) %>% all
  
  
  x <- x1 & x2 & x3 & x4 & x5
  
  if(all(x)) return(TRUE)
  out <- c()
  if(!x1) out <- c(out, "Found objects that are not matrix")
  if(!x2) out <- c(out, "Found matrices that are not numeric")
  if(!x3) {
    y <- labels[duplicated(labels)]
    message("Problem with this/these name(s):")
    cat(paste(y, collapse=","))
    out <- c(out, "Found duplicate names")
  }
  
  if(!x4) {
    y <- which(sapply(object@data, det) < 0)
    message("Problem with this matrix:")
    print(object@data[y])
    out <- c(out, "Invalid matrix: determinant is less than 0")
  }
  if(!x5) {
    out <- c(out, "Length of labels does not match the matrix entered.")
  }
  return(out)
}

dim_matlist <- function(x) {
  if(length(x@data)==0) return(0)
  unname(sapply(x@data,nrow))
}

create_matlist <- function(x=list(),class,labels=list(),signature=NULL,...) {
  x <- x[!sapply(x,nrow)==0]
  if(is.null(names(x))) names(x) <- rep("...", length(x))
  names(x)[nchar(names(x))==0] <- "..."
  if(is.null(unlist(labels))) labels <- lapply(x, function(y) rep('.',nrow(y)))
  x <- new(class, data=x, labels=labels)
  x@n <- dim_matlist(x)
  return(x)
}


##' S4 class matlist.
##'
##' @rdname matlist-class
setClass("matlist", 
         slots=c(
           data="list",
           n="numeric", 
           labels="list"
         ),
         prototype=list(data=list(), labels=list()),
         validity=valid.matlist
)

##' @export
##' @rdname matlist-class
setClass("omegalist", contains="matlist")
##' @export
##' @rdname matlist-class
setClass("sigmalist", contains="matlist")


##' S4 class numeric list.
##'
##' @name numericlist-class
##' @param data list of data
##' @param pattern character of length 1 containing regular expression to be used as a filter when printing data to the console
setClass("numericlist", 
         slots=c(
           data="list", 
           pattern="character"
         ),
         validity=valid.numericlist, 
         prototype=list(data=null_list, pattern="*")
)

##' @title Methods for numericlist
##' @description
##' These methods can be used to corece \code{param} and \code{init} objects into common \code{R} data structures.
##' @name numericlist
NULL


##' @export
##' @rdname numericlist
##' @param x object
##' @param ... passed along to other methods
setMethod("as.list", "numericlist", function(x,...) as.list(x@data))
##' @export
##' @rdname numericlist
setMethod("as.numeric", "numericlist", function(x) {
  ans <- unlist(x@data)
  if(is.null(ans)) return(numeric(0))
  return(ans)
})
##' @export
##' @rdname numericlist
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
setMethod("as.data.frame", "numericlist", function(x,row.names=NULL, optional=FALSE,...) as.data.frame(x@data,row.names,optional,...))
##' @export
##' @rdname numericlist
setMethod("length", "numericlist", function(x) length(x@data))
##' @export
##' @rdname numericlist
setMethod("names", "numericlist", function(x) as.character(names(x@data)))

##' @export
##' @rdname numericlist
##' @param name column to take; used with \code{$}
setMethod("$", "numericlist", function(x,name){unlist(x@data[name],use.names=FALSE)})

##' @export
##' @rdname numericlist
##' @param i elements to keep
##' @param j not used
##' @param drop not used
##' @aliases [,numericlist-method
setMethod("[", "numericlist", function(x,i,j,...){x@data[i,...]})


create_numeric_list <- function(x,class,...) {
  if(length(x) ==0) return(new(class))
  new(class, data=x)
}




##' S4 events class
##' @slot data a data frame of events
##' @export
setClass("ev", slots=c(data="data.frame"))
##' S4 parameter_list class
##' @details
##' parameter_list is a \code{\link{numericlist-class}}
setClass("parameter_list",contains="numericlist")
##' S4 cmt_list class
##' @details
##' cmt_list is a \code{\link{numericlist-class}}
setClass("cmt_list",contains="numericlist")


## mrgmod:
protomod <- list(model=character(0),
                 package=character(0),
                 soloc=tempdir(),
                 project='.',
                 start = 0.0,
                 end=24.0,
                 delta=1.0,
                 add = numeric(0),
                 tscale = as.double(1),
                 digits=-1,
                 quiet = FALSE,
                 verbose = FALSE,
                 debug=FALSE,
                 preclean=FALSE,
                 atol=1E-8,
                 rtol=1E-8,
                 maxsteps=2000,
                 hmin=0,
                 hmax=0,
                 ixpr=0,
                 mxhnil=0,
                 shlib=list(date="",par="", cmt="", compiled=FALSE, version=NULL, source=""),
                 funs = c(main=character(0),
                          ode=character(0),
                          table=character(0),
                          config=character(0)),
                 omega =new("omegalist"),
                 sigma = new("sigmalist"),
                 events=new("ev"),
                 request="(all)",
                 param = new("parameter_list"),
                 init=new("cmt_list"),
                 capture=character(0),
                 args = list(),
                 fixed  = list(),
                 advan=13,
                 trans=1,
                 mindt=10*.Machine$double.eps,
                 code = character(0),
                 annot = list()
)
slot.names <- names(protomod)
slots <- sapply(protomod, class)
names(slots) <- names(protomod)


valid.mrgmod <- function(object) {
  tags <- unlist(names(object), use.names=FALSE)
  x <- check_names(tags,pars(object),cmt(object))
  x1 <- length(x)==0
  x2 <- object@advan %in% c(1,2,3,4,13)
  fun <- valid_funs(object@funs)
  cool <- x1 & x2 & fun[[1]]
  if(cool) return(TRUE)
  x <- c(x,fun[[2]])
  if(!x2) x <- c(x,"Advan must be 1, 2, 3, 4, or 13")
  return(x)
}


##' S4 class for mrgsolve model object
##'
##' @section Notes:
##' \itemize{
##' \item Spaces in paths (\code{project} and \code{soloc}) are prohibited.
##'
##' }
##'
##' @slot model model name \code{<character>}
##' @slot project working directory; must be writeable with no spaces \code{<character>}
##' @slot start simulation start time \code{<numeric>}
##' @slot end simulation end time \code{<numeric>}
##' @slot delta simulation time interval \code{<numeric>}
##' @slot add additional simulation times \code{<numeric-vector>}
##' @slot param parameter_list
##' @slot fixed a parameter_list of fixed value parameters; these are not updatable from \code{R}
##' @slot init cmt_list
##' @slot events \link[=ev]{events} object
##' @slot digits significant digits in simulated output; negative integer means ignore \code{<numeric>}
##' @slot hmin passed to dlsoda  \code{<numeric>}
##' @slot hmax passed to dlsoda \code{<numeric>}
##' @slot mxhnil passed to dlsoda \code{<numeric>}
##' @slot ixpr passed to dlsoda \code{<numeric>}
##' @slot atol passed to dlsoda \code{<numeric>}
##' @slot rtol passed to dlsoda \code{<numeric>}
##' @slot maxsteps passed to dlsoda \code{<numeric>}
##' @slot preclean passed to R CMD SHLIB during compilation \code{<logical>}
##' @slot verbose print run information to screen \code{<logical>}
##' @slot tscale used to scale time in simulated output \code{<numeric>}
##' @slot omega \code{\link{matlist}} for simulating individual-level random effects
##' @slot sigma \code{\link{matlist}} for simulating residual error variates
##' @slot args \code{<list>} of arguments to be passed to \code{\link{mrgsim}}
##' @slot advan either 2, 4, or 13 \code{<numeric>}
##' @slot trans either 1, 2, 4, or 11
##' @slot request  vector of compartments to request \code{<character>}
##' @slot soloc directory path for storing the model shared object \code{<character>}
##' @slot code a character vector of the model code
##' @slot mindt minimum time between simulation records \code{<numeric>}
setClass("mrgmod",slots=slots, validity=valid.mrgmod, prototype=protomod)


##' Check if an object is \code{mrgmod} or \code{packmod}.
##' 
##' 
##' @param x any object
##' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
##' @export
##' 
is.mrgmod <- function(x) inherits(x,c("mrgmod","packmod"))

##' Check if an object is \code{mrgsims}.
##' 
##' 
##' @param x any object
##' @export
##' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
##' 
is.mrgsims <- function(x) inherits(x,"mrgsims")

is.matlist <- function(x) inherits(x,"matlist")
is.mrgindata <- function(x) inherits(x,"mrgindata")
is.valid_idata <- function(x) inherits(x,"valid_idata")

##' S4 class for mrgsolve simulation output
##'
##' @slot request character vector of compartments requested in simulated output
##' @slot outnames character vector of column names in simulated output coming from table step
##' @slot data matrix of simulated data
##' @slot mod the mrgmod model object
setClass("mrgsims", 
         slots=c(
           request="character",
           outnames="character",
           data="data.frame",
           mod="mrgmod",
           seed="integer",
           date="character"
         )
)

setClass("batch_mrgsims",contains="mrgsims",
         slots=c(
           knobs="character", 
           batch="data.frame", 
           request="character",
           moving="character",
           input="list"
         )
)

setClass("lockedmod",
         contains="mrgmod",
         slots=c(
           dllloc="character",
           dllname="character",
           src = "character",
           include="character",
           inpackage="logical"
         )
)

setClass("packmod",
         prototype = list(shlib=list(compiled=TRUE, date="date of package compile"),package="",src="",header=""),
         contains="mrgmod",
         slots=c(
           package="character",
           src="character", 
           header="character"
         )
)

##' @export
##' @rdname stime
setClass("tgrid", slots=c(start="numeric", end="numeric", delta="numeric", add="numeric", offset="numeric", scale="numeric"),
         prototype=list(start=0, end=24, delta=1, offset=0,scale=1))
##' @export
##' @rdname stime
setClass("tgrids", slots=c(data="list"))



