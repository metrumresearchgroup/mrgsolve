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

#' Check input data set names against model parameters
#' 
#' When parameters are tagged (labeled) in the model specification file, use
#' this function to check names of input data sets against the tagged
#' parameters. This is especially useful to alert the user to misspelled or 
#' otherwise misspecified parameter names in input data sets. See [param_tags()]
#' for information on associating tags with parameters.
#' 
#' @param data a data frame or other object with names to check.
#' @param x a model object.
#' @param check_covariates logical; indicates whether or not to check `data` for 
#' parameter names carrying the `covariates` attribute.
#' @param check_inputs logical; indicates whether or not to check `data` for
#' parameter names carrying the `input` attribute.
#' @param tags a character vector of user-defined parameter tags to require 
#' in `data`; this may be a comma- or space-separated string (e.g. 
#' `"tag1, 'tag2"`).
#' @param strict if `TRUE`, then generate an error when `data` is missing some
#' expected column names; otherwise, issue a warning when `data` names aren't
#' as expected.
#' @param silent silences any warning that might have been issued when `strict`
#' is `FALSE`.
#' 
#' @details
#' By default, `data` will be checked for parameters with the `covariates` or 
#' `input` tags; these checks can be bypassed with the `check_covariates`
#' and `check_inputs` arguments.
#' 
#' It is an error to request a parameter tag via the `tags` argument when that
#' tag is not found in the model.
#' 
#' @examples
#' 
#' mod <- mcode("ex-cdn", "$PARAM @input \n CL = 1, KA = 2", compile = FALSE)
#' 
#' param(mod)
#' 
#' data <- expand.evd(amt = 100, cl = 2, KA = 5)
#' 
#' check_data_names(data, mod)
#' 
#' @return 
#' A logical value is returned; `TRUE` if all expected parameters were found 
#' and `FALSE` otherwise. 
#' 
#' @seealso [param_tags()]
#' 
#' @md
#' @export
check_data_names <- function(data, x, check_covariates = TRUE, 
                             check_inputs = TRUE, tags = NULL, 
                             strict = FALSE, silent = FALSE) {
  if(!is_named(data)) {
    abort("`data` must be a named object.")  
  }
  
  if(!is.mrgmod(x)) {
    abort("`x` must be a model object.")  
  }
  
  if(!is.null(tags)) {
    if(!is.character(tags)) {
      abort("`tags` must have type 'character'.")  
    }
  }
  
  check_covariates <- isTRUE(check_covariates)
  check_inputs <- isTRUE(check_inputs)
  silent <- isTRUE(silent)
  strict <- isTRUE(strict)
  
  tg <- param_tags(x)
  
  need_name <- character(0)
  need_type <- character(0)
  
  # Check that user-defined tags exist
  if(length(tags) > 0) {
    tags <- cvec_cs(tags)
    if(!all(tags %in% tg$tag)) {
      bad_tag <- setdiff(tags, tg$tag)
      names(bad_tag) <- rep("x", length(bad_tag))
      msg <- c("Unrecognized user tag(s):", bad_tag)
      abort(msg, use_cli_format = TRUE)
    }
  }

  if(check_covariates) {
    tags <- c("covariates", tags)
  }
    
  if(check_inputs) {
    tags <- c("input", tags)
  }
  
  if(length(tags)==0) {
    msg <- "Did not find any inputs, covariates, or tags to check."
    if(strict) {
      abort(msg, use_cli_format = TRUE)
    } else {
      warn(msg, use_cli_format = TRUE)  
    }
    return(invisible(FALSE))
  }
  
  tg <- tg[tg$tag %in% tags,]
  need_name <- tg$name
  need_type <- tg$tag
  
  need_type <- tapply(
    X = need_type, 
    INDEX = need_name,
    FUN = paste0, collapse = ", ",
    simplify = TRUE
  )
  
  # tapply will reorder things; restore order with this:
  need_name <- unique(need_name)
  need_type <- need_type[need_name]
  
  found <- need_name %in% names(data)
  
  if(!(status <- all(found))) {
    miss <- need_name[!found]
    miss <- paste0(miss, " (", need_type[!found], ")")
    names(miss) <- rep("*", length(miss))
    msg <- c("Could not find the following parameter names in `data`:", miss)
    foot <- "Please check names in `data` against names in the parameter list."
    if(strict) {
      abort(msg, footer = c(x = foot), use_cli_format = TRUE)  
    } else {
      if(!silent) {
        warn(msg, footer = c(i = foot), use_cli_format = TRUE) 
      }
    }
  } else {
    if(!silent) {
      msg <- "Found all expected parameter names in `data`."
      message(msg)
    }
  }
  return(invisible(status))
}

#' Return parameter tags
#' 
#' Use this function if you added the `@covariates` or `@input` attributes or 
#' specified a user-defined tag (via `@tag`) in one or more parameter blocks 
#' and need to extract that information. Also, using the `$INPUT` block to 
#' declare parameters will automatically add the `@input` attribute. Once these 
#' attributes / tags are added, you can use [check_data_names()] to test 
#' input data sets for names match up with parameters carrying these attributes.
#' 
#' @param x mrgsolve model object.
#' 
#' @return 
#' A data frame listing parameter names and their tags.
#' 
#' @examples
#' mod <- house()
#' 
#' param_tags(mod)
#' 
#' @seealso [check_data_names()]
#' 
#' @md
#' @export
param_tags <- function(x) {
  return(x@shlib$param_tag)
}
