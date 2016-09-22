
valid_funs <- function(x) {
  x1 <- length(x)==4
  x2 <- identical(names(x), c("main", "ode", "table", "config"))
  if(x1 & x2) return(list(TRUE,""))
  return(list(FALSE, c("Invalid functions specification.",
                       "This model object is not compatible with the current mrgsolve version.",
                       "Rebuild the model object or upgrade the mrgsolve version.")))
}



check_names <- function(x,par,cmt) {
  
  x <- x[!is.element(x,c(".", "..."))]
  
  dups <- any(duplicated(x))
  us <- any(grepl("\\_",x,perl=TRUE))
  res <- any(is.element(x,Reserved))
  
  ans <- character(0)
  
  ## Duplicate names are not allowed
  if(dups) {
    tmp <- paste(x[duplicated(x)], collapse=" ")
    ans <- c(ans,paste0("Duplicated model names: ", tmp))
  }
  ## Look for names in the Reserved word list
  if(res) {
    tmp <- paste(x[is.element(x,Reserved)],collapse=" ")
    ans <- c(ans,paste0("Reserved words in model names: ",tmp))
  }
  ## Scan for names with underscores
  ## Leading underscores are not allowed
  ## Also, look for any name that conflicts with
  ##   bioav, lag-time, or infusion duration or ate
  if(us) {
    u <- grep("\\_",x,perl=TRUE,value=TRUE)
    leading <- grep("^\\_",x,perl=TRUE,value=TRUE)
    if(length(leading) > 0) {
      ans <- c(ans, paste0("Leading underscore not allowed: ", paste(leading, collapse=" "))) 
    }
    check <- as.character(sapply(c("F_", "ALAG_", "D_", "R_"),paste0,cmt))
    iv_name <- intersect(x,check)
    if(length(iv_name) > 0) {
      ans <- c(ans, paste0("Reserved symbols in model names: ", iv_name))
    }
  }
  return(ans)
}


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
                 omega=new("omegalist"),
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


##' Return a pre-compiled, PK/PD model.
##' 
##' @param ... passed to update
##' 
##' @return 
##' A \code{packmod} object, ready to simulated.
##' 
##' @examples
##' 
##' mod <- mrgsolve:::house()
##' 
##' see(mod)
##' 
##' mod %>% ev(amt=100) %>% mrgsim %>% plot
##' 
house <- function(...) {
  att <- readRDS(file=pfile("mrgsolve", "project", "housemodel", "RDS"))
  x <- new("packmod",
           att,
           package="mrgsolve",
           model="housemodel"
  )
  x@soloc <- dirname(sodll(x))
  x <- compiled(x,TRUE)
  x <- update(x,...,strict=FALSE)
  x
}



##' Coerce an mrgmod object to packmod
##' @name as.packmod
##' @rdname as.packmod
##' @param x mrgmod model object
##' @param ... passed along
setGeneric("as.packmod", function(x,...) standardGeneric("as.packmod"))


##' @export
##' @rdname as.packmod
setMethod("as.packmod", "mrgmod", function(x,...) {
  x <- new("packmod",...,x)
  x
})


