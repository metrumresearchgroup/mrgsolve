# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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
  x <- data.matrix(numerics_only(x,quiet)) 
  if(ncol(x)==0) stop("invalid data set.",call.=FALSE)
  return(x)
}

##' Prepare data.frame for input to mrgsim
##' 
##' @param x a input data set
##' @param quiet logical indicating whether or not warnings 
##' should be printed
##' @param convert_lgl by default, convert logical 
##' columns with \code{\link{as.integer}}
##' 
##' @export
numerics_only <- function(x,quiet=FALSE,convert_lgl=TRUE) {
  if(convert_lgl) {
    if(any(sapply(x,is.logical))) {
      x <- dplyr::mutate_if(x, is.logical, as.integer)
    }
  }
  nu <- is.numeric(x)
  if(!all(nu)) {
    if(!quiet) {
      message(
        "Dropping non-numeric columns: \n  ", 
        paste(names(x)[!nu], collapse=" ")
      )
    }
    x <- dplyr::select(x,which(nu))
  } 
  x
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


##' Validate and prepare a data sets for simulation
##'
##' This function is called by mrgsim.  Users may also call this function
##' to pre-validate data when the same data set is used for repeated 
##' simulation.
##'
##' @param x data.frame or matrix
##' @param m a model object
##' @param verbose logical
##' @param quiet if \code{TRUE}, messages will be suppressed
##' 
##' @return A matrix with non-numeric columns dropped; if x is a 
##' data.frame with character \code{cmt} column comprised of valid 
##' compartment names and \code{m} is a model object,
##' the \code{cmt} column will be converted to the corresponding 
##' compartment number.
##' 
##' @seealso \code{\link{valid_idata_set}}, \code{\link{idata_set}}, 
##' \code{\link{data_set}}
##' 
##' @examples
##' 
##' mod <- mrgsolve:::house()
##' 
##' data(exTheoph)
##' 
##' d <- valid_data_set(exTheoph,mod)
##' 
##' @export
valid_data_set <- function(x, m = NULL, verbose = FALSE, quiet = FALSE) {

  if(is.valid_data_set(x)) return(x)
    
  if(verbose) quiet <- FALSE
  
  if(!is.mrgmod(m)) {
    stop("A valid model object is required to validate the data set.", 
         call. = FALSE)
  }
  
  x <- as.data.frame(x)
  
  if(nrow(x)==0) {
    stop("Input data event object has zero rows", call. = FALSE)  
  }
  
  # check for ID column
  if(!has_ID(x)) {
    stop("Could not find ID column in data set", call. = FALSE)
  }
  
  # special case
  if(ncol(x)==1) {
    x <- numeric_data_matrix(x,quiet)
    return(structure(x, class = c("valid_data_set", "matrix")))
  }
  
  
  # First, check for compartment
  cmtcol <- cmtname(x)
  if(!is.na(cmtcol)) {
    if(any(is.na(x[,cmtcol]))) {
      stop("Found missing value in cmt/CMT column")
    }
    if(is.character(x[[cmtcol]])) {
      if(verbose) message("Converting cmt to integer")
      x[[cmtcol]] <- match(x[[cmtcol]], cmt(m),0)
    }
  }
  
  tcol <- timename(x)
  if(is.na(tcol)) {
    if(neq(m) > 0) {
      stop(
        "A time or TIME column is required in the data set item with this model.",
        call. = FALSE
      )  
    }
  }
  
  # Drop character columns
  x <- numeric_data_matrix(x,quiet)
  
  check_data_set_na(x,m)
  
  x <- cbind(x, matrix(0,
                       ncol=1,
                       nrow=nrow(x), 
                       dimnames=list(NULL, "..zeros..")))
  
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
  
  structure(x, class = c("valid_data_set", "matrix"))
}

##' Validate and prepare idata data sets for simulation
##' 
##' @return A numeric matrix with class \code{valid_idata_set}.
##' 
##' @inheritParams valid_data_set
##' 
##' @seealso \code{\link{valid_data_set}}, \code{\link{idata_set}}, 
##' \code{\link{data_set}}
##' 
##' @export
valid_idata_set <- function(x, m, verbose=FALSE, quiet=FALSE) {
  
  if(verbose) quiet <- FALSE
  
  if(is.valid_idata_set(x)) return(x) 
  
  x <- as.data.frame(x)
  
  if(!has_ID(x)) {
    stop("ID is a required column for idata_set.",call.=FALSE)
  }
  
  if(any(duplicated(x[["ID"]]))) {
    stop("Duplicate IDs not allowed in idata_set.",call.=FALSE) 
  }
  
  x <- numeric_data_matrix(x,quiet)
  
  check_data_set_na(x,m)
  
  structure(x, class=c("valid_idata_set", "matrix"))
}

##' @rdname valid_data_set
##' @export
valid_data_set.matrix <- function(x,verbose=FALSE) {
  if(is.valid_data_set(x)) return(x)
  if(is.numeric(x)) {
    return(valid_data_set(as.data.frame(x)))
  }
  stop("Input data matrix is not numeric.",call.=FALSE)
}

check_data_set_na <- function(data,m) {
  if(!anyNA(data)) return(invisible(NULL))
  err <- FALSE
  flagged <- check_column_na(
    data,
    Pars(m)
  )
  for(col in flagged) {
    warning(
      "Parameter column ", col, " must not contain missing values.", 
      call.=FALSE, immediate.=TRUE
    ) 
  }
  flagged <- check_column_na(
    data,
    c("ID","TIME", "time", "RATE", "rate")
  )
  for(col in flagged) {
    message(
      col, 
      " column must not contain missing values.", 
      call.=FALSE,immediate.=TRUE
    ) 
    err <- TRUE
  }  
  if(err) stop("Found missing values in input data.", call.=FALSE)
  return(invisible(NULL))
}

check_column_na <- function(data,cols) {
  check <- intersect(colnames(data),cols)
  if(length(check)==0) return(character(0))
  if(!anyNA(data[,check])) return(character(0))
  flagged <- character(0)
  for(col in check) {
    if(anyNA(data[,col])) {
      flagged <- c(flagged,col)
    }
  }
  return(flagged)
}



