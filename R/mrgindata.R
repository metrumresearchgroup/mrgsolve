# Copyright (C) 2013 - 2018  Metrum Research Group, LLC
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

convert_character_cmt <- function(data, mod) {
  cmtcol <- intersect(c("cmt", "CMT"), names(data))
  for(cm in cmtcol) {
    if(is.character(data[[cm]])) {
      data[[cm]] <- match(data[[cm]], cmt(mod),0)  
    }
  }
  return(data)
}


##' Validate and prepare data sets for simulation
##'
##' @name valid_data
##' @rdname valid_data
##'
##' @param x data.frame or matrix
##' @param m a model object
##' @param verbose logical
##' @param quiet if \code{TRUE}, messages will be suppressed
##' 
##' @return a matrix with non-numeric columns dropped; if x is a 
##' data.frame with character \code{cmt} column comprised of valid 
##' compartment names and \code{m} is a model object,
##' the \code{cmt} column will be converted to the corresponding 
##' compartment number.
##' 
##' @export
valid_data_set <- function(x, m = NULL, verbose = FALSE,
                           quiet = FALSE) {
  
  if(verbose) quiet <- FALSE
  
  if(is.valid_data_set(x)) return(x)
  
  x <- as.data.frame(x)
  
  if(nrow(x)==0) {
    stop("Input data event object has zero rows", call. = FALSE)  
  }
  
  # check for ID column
  if(!has_ID(x)) {
    stop("Could not find ID column in data set", call. = FALSE)
  }
  
  if(ncol(x) > 1) {
    
    # First, check for compartment
    cmtcol <- intersect(c("cmt", "CMT"), colnames(x))[1]
    if(is.na(cmtcol))  {
      stop("Could not find cmt/CMT column in data set")
    }
    
    if(any(is.na(x[,cmtcol]))) {
      stop("Found missing value in cmt/CMT column")
    }
    
    # Convert cmt/CMT to numeric if it's character and you 
    # have the model object
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
    if(is.na(tcol)) {
      stop("Could not find time/TIME column in data set", call. = FALSE)
    }
    
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
  
  structure(x, class = "valid_data_set")
  
}

##' Validate and prepare idata data sets for simulation
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
valid_idata_set <- function(x,verbose=FALSE,quiet=FALSE) {
  
  if(verbose) quiet <- FALSE
  
  if(is.valid_idata_set(x)) return(x) 
  
  x <- as.data.frame(x)
  
  if(!has_ID(x)) {
    stop("ID is a required column for idata_set",call.=FALSE)
  }
  
  if(any(duplicated(x[,"ID"]))) {
    stop("duplicate IDs not allowed in idata_set",call.=FALSE) 
  }
  
  structure(numeric_data_matrix(as.data.frame(x),quiet),
            class="valid_idata_set")
}

##' @rdname valid_data
##' @export
valid_data_set.matrix <- function(x,verbose=FALSE) {
  
  if(is.valid_data_set(x)) return(x)
  if(is.numeric(x)) {
    return(valid_data_set(as.data.frame(x)))
  }
  stop("input data matrix is not numeric",call.=FALSE)
}
