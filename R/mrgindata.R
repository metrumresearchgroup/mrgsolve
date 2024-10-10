# Copyright (C) 2013 - 2024  Metrum Research Group
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
  inherits(x, "valid_data_set")
}

is.valid_idata_set <- function(x) {
  inherits(x, "valid_idata_set")
}

idcol <- function(x) {
  match("ID", colnames(x)) 
}

timename <- function(x) {
  y <- c("time", "TIME")
  y[y %in% .colnames(x)][1]
}

cmtname <- function(x) {
  y <- c("cmt", "CMT")
  y[y %in% .colnames(x)][1]
}

numeric_data_matrix <- function(x, quiet = FALSE) {
  x <- do.call(cbind, numerics_only(x, quiet)) 
  if(ncol(x)==0) stop("invalid data set.", call.=FALSE)
  x
}

#' Prepare data.frame for input to mrgsim()
#' 
#' @param x a input data set.
#' @param quiet logical indicating whether or not warnings 
#' should be printed.
#' @param convert_lgl if `TRUE`, convert logical 
#' columns with [as.integer()].
#' 
#' @md
#' @export
numerics_only <- function(x, quiet = FALSE, convert_lgl = FALSE) {
  if(convert_lgl) {
    if(any(vapply(x,is.logical,TRUE))) {
      x <- mutate_if(x, is.logical, as.integer)
    }
  }
  nu <- vapply(x, bare_numeric, TRUE)
  if(!all(nu)) {
    if(!quiet) {
      message(
        "Dropping non-numeric columns: \n  ",
        paste(names(x)[!nu], collapse = " ")
      )
    }
    x <- x[, which(nu), drop = FALSE]
  } 
  x
}

convert_character_cmt <- function(data, mod) {
  cmtcol <- cmtname(data)
  for(cm in cmtcol) {
    if(is.character(data[[cm]])) {
      data[[cm]] <- match(data[[cm]], Cmt(mod), 0L)  
    }
  }
  return(data)
}

check_dropped_cols <- function(dm, x, check, context) {
  new_col_set <- dimnames(dm)[[2]]
  old_col_set <- names(x)
  drop <- old_col_set[!old_col_set %in% new_col_set]
  drop <- drop[drop %in% check]
  if(!length(drop)) return(invisible(NULL))
  body <- vector(mode = "character", length = length(drop))
  names(body) <- rep("x", length(body))
  for(i in seq_along(drop)) {
    d <- drop[i]
    type <- paste0(class(x[[d]]), collapse = ",")
    body[i] <- paste0(context, " column: ", d, " (", type, ")")
  }
  abort(
    message = "Found input data that cannot be used for simulation", 
    body = body, 
    call = caller_env()
  )
}

#' Validate and prepare data sets for simulation
#'
#' This function is called by [mrgsim()] and friends to check and prepare 
#' input data sets for simulation.  Users may also call this function to 
#' pre-validate data when the same data set is used for repeated simulation.
#'
#' @param x data.frame or matrix.
#' @param m a model object.
#' @param verbose logical.
#' @param quiet if `TRUE`, messages will be suppressed.
#' 
#' @details
#' An error will be issued when
#' - non-numeric data is found in columns sharing names with model parameters
#' - non-numeric data is found in reserved data items related to dosing 
#'   (see `mrgsolve:::GLOBALS$CARRY_TRAN`)
#' - a column is found that is "internally classed", including columns that 
#'   inherit from `integer64` (see [is.object()])
#' 
#' @return A matrix with non-numeric columns dropped; if x is a 
#' data.frame with character `cmt` column comprised of valid 
#' compartment names and `m` is a model object,
#' the `cmt` column will be converted to the corresponding 
#' compartment number.
#' 
#' @seealso [valid_idata_set()], [idata_set()], [data_set()]
#' 
#' @examples
#' 
#' mod <- mrgsolve::house()
#' 
#' data(exTheoph)
#' 
#' d <- valid_data_set(exTheoph, mod)
#' 
#' @md
#' @export
valid_data_set <- function(x, m = NULL, verbose = FALSE, quiet = FALSE) {
  
  if(is.valid_data_set(x)) return(x)
  
  if(verbose) quiet <- FALSE
  
  if(!is.mrgmod(m)) {
    stop("a valid model object is required to validate the data set.", 
         call. = FALSE)
  }
  
  x <- as.data.frame(x)
  
  if(nrow(x)==0) {
    stop("input data event object has zero rows", call. = FALSE)  
  }
  
  # check for ID column
  if(!has_ID(x)) {
    stop("could not find ID column in data set", call. = FALSE)
  }
  
  # special case
  if(ncol(x)==1) {
    x <- numeric_data_matrix(x,quiet=TRUE)
    class(x) <- c("valid_data_set", "matrix")
    return(x)
  }
  
  # First, check for compartment
  cmtcol <- cmtname(x)
  if(!is.na(cmtcol)) {
    if(any(is.na(x[,cmtcol]))) {
      stop("Found missing value in cmt/CMT column")
    }
    if(is.character(x[[cmtcol]])) {
      if(verbose) message("Converting cmt to integer")
      x[[cmtcol]] <- match(x[[cmtcol]], Cmt(m), 0)
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
  dm <- numeric_data_matrix(x,quiet=TRUE)
  
  if(ncol(dm) != ncol(x)) {
    check <- c(Pars(m), GLOBALS$CARRY_TRAN)
    check_dropped_cols(dm, x, check, context = "data set")
  }
  
  has_na <- check_data_set_na(dm,m)
  
  if(has_na) {
    dm <- fill_tran_na(dm)  
  }
  
  dm <- cbind(dm, matrix(0,
                         ncol=1,
                         nrow=nrow(dm), 
                         dimnames=list(NULL, "..zeros..")))
  
  # Look for both upper and lower case column names
  uc <- any(dimnames(dm)[[2]] %in% GLOBALS[["TRAN_UPPER"]])
  lc <- any(dimnames(dm)[[2]] %in% GLOBALS[["TRAN_LOWER"]])
  
  if(uc & lc) {
    warning("Both lower- & upper-case names found in the data set.\n",
            "Please use either:\n",
            "  time,amt,cmt,evid,ii,addl,ss,rate\n",
            "or:\n",
            "  TIME,AMT,CMT,EVID,II,ADDL,SS,RATE\n", call.=FALSE)
  }
  class(dm) <- c("valid_data_set", "matrix")
  dm
}

#' Validate and prepare idata data sets for simulation
#' 
#' This function is called by [mrgsim()] and friends to check and prepare 
#' input data sets for simulation.  Users may also call this function to 
#' pre-validate data when the same data set is used for repeated simulation.
#' 
#' @return A numeric matrix with class `valid_idata_set`.
#' 
#' @inheritParams valid_data_set
#' 
#' @seealso [valid_data_set()], [idata_set()], [data_set()]
#' 
#' @details
#' An error will be issued when
#' - non-numeric data is found in columns sharing names with model parameters
#' - a column is found that is internally classed, including columns that 
#'   inherit from `integer64` (see [is.object()])
#' 
#' @md
#' @export
valid_idata_set <- function(x, m, verbose = FALSE, quiet = FALSE) {
  
  if(verbose) quiet <- FALSE
  
  if(is.valid_idata_set(x)) return(x) 
  
  x <- as.data.frame(x)
  
  if(!has_ID(x)) {
    stop("ID is a required column for idata_set.",call.=FALSE)
  }
  
  if(anyDuplicated(x[["ID"]])) {
    stop("Duplicate IDs not allowed in idata_set.",call.=FALSE) 
  }
  
  dm <- numeric_data_matrix(x, quiet = TRUE)
  
  if(ncol(dm) != ncol(x)) {
    check <- Pars(m)
    check_dropped_cols(dm, x, check, context = "idata set")
  }
  
  check_data_set_na(dm, m)
  class(dm) <- c("valid_idata_set", "matrix")
  
  dm
}

#' @rdname valid_data_set
#' @export
valid_data_set.matrix <- function(x,verbose=FALSE) {
  if(is.valid_data_set(x)) return(x)
  if(is.numeric(x)) {
    return(valid_data_set(as.data.frame(x)))
  }
  stop("Input data matrix is not numeric.",call.=FALSE)
}

check_data_set_na <- function(data,m) {
  if(!anyNA(data)) return(invisible(FALSE))
  err <- FALSE
  flagged <- check_column_na(data, Pars(m))
  for(col in flagged) {
    warning(
      "Parameter column ", col, " must not contain missing values.", 
      call.=FALSE, immediate.=TRUE
    ) 
  }
  flagged <- check_column_na(data, c("ID", "TIME", "time"))
  for(col in flagged) {
    message(
      col, 
      " column must not contain missing values.", 
      call.=FALSE, immediate.=TRUE
    ) 
    err <- TRUE
  }  
  if(err) stop("Found missing values in input data.", call.=FALSE)
  return(invisible(TRUE))
}

#' Look for TRAN columns replace NA with 0
#' Columns to scan are found in `GLOBALS$TRAN_FILL_NA`
#' @noRd
fill_tran_na <- function(data) {
  cols_to_zero <- check_column_na(data, GLOBALS[["TRAN_FILL_NA"]])
  data[, cols_to_zero][is.na(data[, cols_to_zero])] <- 0
  data
}

check_column_na <- function(data, cols) {
  to_check <- unique(cols[cols %in% dimnames(data)[[2L]]])
  if(length(to_check)==0L) return(character(0))
  if(!anyNA(data[,to_check])) return(character(0))
  flagged <- character(0)
  for(col in to_check) {
    if(anyNA(data[,col])) {
      flagged <- c(flagged, col)
    }
  }
  flagged
}
