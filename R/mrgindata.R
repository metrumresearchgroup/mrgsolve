# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
#
# This file is part of mrgsolve.
#
# mrgsolve is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# mrgsolve is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.

as.valid_data_set <- function(x) {
  structure(x,class="valid_data_set")
}
as.valid_idata_set <- function(x) {
  structure(x,class="valid_idata_set")
}
is.valid_data_set <- function(x) {
  inherits(x,"valid_data_set")
}
is.valid_idata_set <- function(x) {
  inherits(x,"valid_idata_set")
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
numeric_data_matrix <- function(x,quiet=FALSE) {
  nu <- is.numeric(x)
  if(!all(nu)) {
    if(!quiet) {
      message("Dropping non-numeric columns: ", 
              paste(names(x)[!nu], collapse=" "))
    }
    x <- x[,nu, drop=FALSE]
  } 
  x <- data.matrix(x) 
  if(ncol(x)==0) stop("invalid data set.",call.=FALSE)
  return(x)
}


##' Validate and prepare data sets for simulation.
##'
##' @name valid_data
##' @rdname valid_data
##'
##' @param x data.frame or matrix
##' @param m a model object
##' @param verbose logical
##' @param quiet if \code{TRUE}, messages will be suppressed
##' @param ... additional arguments
##' 
##' @return a matrix with non-numeric columns dropped; if x is a 
##' data.frame with character \code{cmt} column comprised of valid 
##' compartment names and \code{m} is a model object,
##' the \code{cmt} column will be converted to the corresponding 
##' compartment number.
##' 
##' @export
valid_data_set <- function(x,...) UseMethod("valid_data_set")

##' @rdname valid_data
##' @export
valid_data_set.default <- function(x,...) {
  valid_data_set(as.data.frame(x),...)
}

##' @rdname valid_data
##' @export
valid_data_set.data.frame <- function(x,m=NULL,verbose=FALSE,
                                      quiet=FALSE,...) {
  
  if(verbose) quiet <- FALSE
  
  if(is.valid_data_set(x)) return(x)
  
  tcol <- "time"
  
  # check for ID column
  idcol <- intersect("ID", colnames(x))[1]
  if(is.na(idcol)) {
    stop("Couldn't find ID column in data set.", call.=FALSE)
  }
  
  if(ncol(x) > 1) {
    
    # First, check for compartment
    cmtcol <- intersect(c("cmt", "CMT"), colnames(x))[1]
    if(is.na(cmtcol)) stop("Couldn't find cmt/CMT column in data set.", call.=FALSE)
    
    # Convert cmt/CMT to numeric if it's character and you have the model object
    if(is.mrgmod(m)) {
      if(is.character(x[[cmtcol]])) {
        if(verbose) message("Converting cmt to integer")
        x[[cmtcol]] <- match(x[[cmtcol]], cmt(m),0)
      }
    }
    
    # Drop character columns
    x <- numeric_data_matrix(x,quiet)
    
    # Now, check for time/TIME and ID
    # TODO: look into droping these checks.
    tcol <- intersect(c("time", "TIME"), colnames(x))[1]
    if(is.na(tcol)) stop("Couldn't find time/TIME column in data set.", call.=FALSE)
    
    x <- cbind(x, matrix(0,
                         ncol=1,
                         nrow=nrow(x), 
                         dimnames=list(NULL, "..zeros..")))
    
  } else {
    x <- numeric_data_matrix(x,quiet)  
  }
  
  # Look for both upper and lower case column names
  uc <- any(colnames(x) %in% GLOBALS[["CARRY_TRAN_UC"]])
  lc <- any(colnames(x) %in% GLOBALS[["CARRY_TRAN_LC"]])
  
  if(uc & lc) {
    warning("Both lower- & upper-case names found in the data set.\n",
            "Please use either:\n",
            "  time,amt,cmt,evid,ii,addl,ss,rate\n",
            "or:\n",
            "  TIME,AMT,CMT,EVID,II,ADDL,SS,RATE\n", call.=FALSE)
  }
  
  as.valid_data_set(x)
  
}

##' Validate and prepare idata data sets for simulation.
##' 
##' 
##' @seealso \code{\link{idata_set}}, \code{\link{data_set}},
##' \code{\link{valid_data_set}}
##' 
##' @return A numeric matrix with class \code{valid_idata_set}.
##' 
##' @rdname valid_data
##' 
##' @export
valid_idata_set <- function(x,verbose=FALSE,quiet=FALSE,...) {
  
  if(verbose) quiet <- FALSE
  
  if(is.valid_idata_set(x)) return(x) 
  
  if(!("ID" %in% colnames(x))) {
    stop("ID is a required column for idata_set",call.=FALSE)
  }
  
  if(any(duplicated(x[,"ID"]))) {
    stop("duplicate IDs not allowed in idata_set",call.=FALSE) 
  }
  
  as.valid_idata_set(numeric_data_matrix(as.data.frame(x),quiet))
  
}

##' @rdname valid_data
##' @export
valid_data_set.matrix <- function(x,verbose=FALSE,...) {
  
  if(is.valid_data_set(x)) return(x)
  if(is.numeric(x)) {
    return(valid_data_set(as.data.frame(x),...))
  }
  stop("Input data matrix is not numeric",call.=FALSE)
}
