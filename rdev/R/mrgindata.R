

## see classes.R for is.mrgindata

as.mrgindata <- function(x) {
  class(x) <- c("mrgindata", x)
  return(x)
}

##' Prepare input data.frame or matrix
##'
##' @param x data.frame or matrix
##' @param m object that inherits from mrgmod
##' @param verbose logical
##' @param quiet if \code{TRUE}, messages will be suppressed
##' @param ... additional arguments
##' @return a matrix with non-numeric columns dropped; if x is a data.frame with character \code{cmt} column comprised of valid compartment names and \code{m} is a model object,
##' the \code{cmt} column will be converted to the corresponding compartment number.
##' @export
mrgindata <- function(x,...) UseMethod("mrgindata")
##' @rdname mrgindata
##' @export
mrgindata.default <- function(x,...) mrgindata(as.data.frame(x),...)
##' @rdname mrgindata
##' @export
mrgindata.data.frame <- function(x,m=NULL,verbose=FALSE,quiet=FALSE,...) {
  
  if(verbose) quiet <- FALSE
  
  if(is.mrgindata(x)) return(x)
  
  tcol <- "time"
  
  if(ncol(x) > 1) {
    
    cmtcol <- intersect(c("cmt", "CMT"), names(x))[1]
    if(is.na(cmtcol)) stop("Couldn't find cmt/CMT column in data set.", call.=FALSE)
    
    tcol <- intersect(c("time", "TIME"), names(x))[1]
    if(is.na(tcol)) stop("Couldn't find time/TIME column in data set.", call.=FALSE)
    
    idcol <- intersect("ID", names(x))[1]
    if(is.na(idcol)) stop("Couldn't find ID column in data set.", call.=FALSE)
    
    if(is.mrgmod(m)) {
      if(is.character(x[[cmtcol]])) {
        if(verbose) message("Converting cmt to integer")
        x[[cmtcol]] <- match(x[[cmtcol]], cmt(m),0)
      }
    }
    
    x <- cbind(x, matrix(0,ncol=1,nrow=nrow(x), dimnames=list(NULL, "..zeros..")))
  }

  x <- numeric_data_matrix(x)
  
  uc <- any(colnames(x) %in% GLOBALS[["CARRY_TRAN_UC"]])
  lc <- any(colnames(x) %in% GLOBALS[["CARRY_TRAN_LC"]])
  
  if(uc & lc) {
    warning("Both lower- & upper-case names found in the data set.\n",
            "Please use either:\n",
            "  time,amt,cmt,evid,ii,addl,ss,rate\n",
            "or:\n",
            "  TIME,AMT,CMT,EVID,II,ADDL,SS,RATE\n", call.=FALSE)
  }
  
  structure(x, class="mrgindata")
  
}


valid_idata <- function(x,verbose=FALSE,quiet=TRUE,...) {
  if(verbose) quiet <- FALSE
  
  if(is.valid_idata(x)) return(x) 
  
  if(!any(grepl("ID", colnames(x),perl=TRUE))) {
    stop("idata set must contain ID column.", call.=FALSE) 
  }
  
  x <- numeric_data_matrix(x,quiet)
  
  return(x)
  
}

numeric_data_matrix <- function(x,quiet=TRUE) {
  
  nu <-is.numeric(x)
  if(sum(!nu)>0) {
    if(!quiet) message("Dropping non-numeric columns: ", 
                       paste(names(x)[!nu], collapse=" "))
  }
  data.matrix(x[,nu, drop=FALSE])
}

idcol <- function(x) {
  match("ID", colnames(x)) 
}
timename <- function(x) {
   intersect(c("time", "TIME"), colnames(x))[1]
}
cmtname <- function(x) {
  intersect(c("cmt", "CMT"), colnames(x))[1] 
}


##' @rdname mrgindata
##' @export
mrgindata.matrix <- function(x,verbose=FALSE,...) {
  
  if(is.mrgindata(x)) return(x)
  if(is.numeric(x)) {
    return(mrgindata(as.data.frame(x),...))
  }
  stop("Input data matrix is not numeric")
}

