# Copyright (C) 2013 - 2020  Metrum Research Group
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

#' Check whether all required parameters needed in a model are present in an object
#' @param x model object
#' @param obj data.frame to pass to \code{\link{idata_set}} or \code{\link{data_set}}
#' @param ... capture dplyr-style parameter requirements
#' @param .strict whether to stop execution if all requirements are present
#'  (\code{TRUE}) 
#' or just warn (\code{FALSE}); see details
#' @examples \dontrun{
#' inventory(mod, idata, CL:V) # parameters defined, inclusively, CL through Volume 
#' inventory(mod, idata, everything()) # all parameters
#' inventory(mod, idata, contains("OCC")) # all parameters containing OCC
#' inventory(mod, idata, -F) # all parameters except F
#' }
#' @return original mrgmod
#' @details 
#' If parameter requirements are not explicitly stated, the requirement defaults to 
#' all parameter names in \code{x}.  Note that, by default,
#' the inventory is not \code{.strict} unless the user explicitly
#' states the parameter requirement. That is, if parameter requirements are explicitly 
#' stated, \code{.strict} will be set to \code{TRUE} if a value \code{.strict} was not
#' passed in the call.
#' @export
inventory <- function(x, obj, ..., .strict = FALSE) {
  
  oname <- as.character(as.list(match.call())$obj)
  
  need <- vars_select(Pars(x),...)
  
  if(!length(need)) {
    need <- Pars(x)
  } else {
    if(missing(.strict)) .strict <- TRUE 
  }
  
  missing <- setdiff(need,names(obj))
  miss <- length(missing) 
  
  if(!miss) {
    message("Found all required parameters in ", sQuote(oname),".")
    return(invisible(x))
  }
  
  if(.strict) {
    stop("missing parameters in ", sQuote(oname), "\n", 
         paste(paste0(" - ", missing, collapse="\n")), call.=FALSE)
  } else {
    warning("Missing parameters in ", shQuote(oname), "\n", 
            paste(paste0(" - ", missing, collapse="\n")), call.=FALSE)
  }
  
  return(invisible(x))
}

#' Check data set names against model parameters
#' 
#' When parameters are tagged or labeled in the model specification file, use
#' this function to check names of input data sets against the tagged
#' parameters. This is especially useful to alert the user to misspelled or 
#' otherwise misspecified parameter names in input data sets.
#' 
#' @param data a data frame or other object with names to check.
#' @param x a model object.
#' @param check_covariates logical; indicates whether or not to check `data` for 
#' parameter names carrying the `covariates` attribute.
#' @param check_input logical; indicates whether or not to check `data` for
#' parameter names carrying the `input` attribute.
#' @param tags a character vector of parameter tags to require in `data`; this
#' may be a comma- or space-separated string (e.g. `"tag1, tag2"`).
#' @param strict if `TRUE`, then generate an error when `data` is missing some
#' expected column names; otherwise, issue a warning.
#' @param silent silences any warning that might have been issued when `strict`
#' is `FALSE`.
#' 
#' @details
#' By default, `data` will be checked for parameters with the `covariates` or 
#' `input` attributes; these checks can be bypassed with the `check_covariates` 
#' and `check_input` arguments.  
#' 
#' @examples
#' 
#' mod <- mcode("ex-cdn", "$PARAM @input \n CL = 1, KA = 2", compile = FALSE)
#' 
#' data <- expand.evd(amt = 100, cl = 2, KA = 5)
#' 
#' check_data_names(data, mod)
#' 
#' @md
#' @export
check_data_names <- function(data, x, check_covariates = TRUE, 
                             check_input = TRUE, tags = NULL, strict = FALSE, 
                             silent = FALSE) {
  
  if(!is_named(data)) {
    abort("`data` must be a named object.")  
  }
  
  if(!is.mrgmod(x)) {
    abort("`x` must be a model object.")  
  }
  
  tg <- x@shlib$param_tag
  
  need <- character(0)
  need_type <- character(0)

  if(isTRUE(check_input)) {
    input <- tg[tg$tag=="input",,drop = FALSE]
    need_name <- input$name
    need_type <- rep("input", length(need_name))
  }
  
  if(isTRUE(check_covariates)) {
    need_name <- c(need_name, x@shlib$covariates)  
    n_cov <- length(x@shlib$covariates)
    need_type <- c(need_type, rep("covariates", n_cov))
  }
  
  if(is.character(tags) && length(tags) > 0) {
    tags <- cvec_cs(tags)
    tg <- tg[tg$tag %in% tags,,drop = FALSE]
    if(nrow(tg) > 0) {
      need_name <- c(need_name, tg$name)  
      need_type <- c(need_type, tg$tag)
    }
  }
  
  dup <- duplicated(need_name)
  need_name <- need_name[!dup]
  need_type <- need_type[!dup]
  
  if(length(need_name)==0) {
    warn("Did not find any inputs, covariates, or tags to check.")
    return(invisible(NULL))
  }
  
  found <- need_name %in% names(data)
  
  if(!(status <- all(found))) {
    miss <- need_name[!found]
    miss <- formatC(miss, width = max(nchar(miss)), flag = "-")
    miss <- paste0(miss, " (", need_type[!found], ")")
    names(miss) <- rep("*", length(miss))
    foot <- "Please check names in `data` against names in the parameter list."
    msg <- c("Could not find the following parameter names in `data`:", miss)
    if(isTRUE(strict)) {
      abort(msg, footer = c(x = foot), use_cli_format = TRUE)  
    } else {
      if(isFALSE(silent)) warn(msg, footer = c(i = foot), use_cli_format = TRUE) 
    }
  } else {
    if(isFALSE(silent)) {
      message("Found all expected parameter names in `data`.")
    }
  }
  
  return(invisible(status))
}
