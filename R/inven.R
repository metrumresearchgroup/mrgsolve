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
#' Use this function to check names of input data sets against parameters that 
#' have been assigned different tags. Assignment is made in the model 
#' specification file. This is useful to alert the user to misspelled or 
#' otherwise misspecified parameter names in input data sets. See [param_tags()]
#' for information on associating tags with parameters.
#' 
#' @param data a data frame or other object with names to check.
#' @param x a model object.
#' @param check_covariates logical; if `TRUE`, check `data` for parameter names 
#' carrying the `covariates` tag.
#' @param check_inputs logical; if `TRUE`, check `data` for parameter names 
#' carrying the `input` tag. 
#' @param tags a character vector of user-defined parameter tags to require 
#' in `data`; this may be a comma- or space-separated string (e.g. 
#' `"tag1,tag2"`).
#' @param mode the default is to `"warn"` the user when `data` is missing 
#' some expected column names; alternatively, use `"error"` to issue an 
#' error or `"inform"` to generate a message when `data` is missing some 
#' expected column names.
#' @param silent silences message on successful check.
#' 
#' @details
#' By default, `data` will be checked for parameters with the `covariates` or 
#' `input` tags; these checks can be bypassed with the `check_covariates`
#' and `check_inputs` arguments. When a parameter name is missing from `data`
#' the user will be warned by default. Use `mode = "error"` to generate an 
#' error instead of a warning and use `mode = "inform"` to simply be informed. 
#' When the user has not tagged any parameters for checking, there will 
#' either be a warning (default) or an error (when `mode = "error"`).
#' 
#' It is an error to request a parameter tag via the `tags` argument when that
#' tag is not found in the model.
#' 
#' It is an error to call `check_data_names` when no parameters have been tagged
#' in the model specification file (see [param_tags()]).
#' 
#' 
#' @examples
#' 
#' mod <- mcode("ex-cdn", "$PARAM @input \n CL = 1, KA = 2", compile = FALSE)
#' 
#' param(mod)
#' 
#' # Coding mistake!
#' data <- expand.evd(amt = 100, cl = 2, KA = 5)
#' 
#' check_data_names(data, mod)
#' 
#' try(check_data_names(data, mod, mode = "error"))
#' 
#' check_data_names(data, mod, mode = "inform")
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
                             mode = c("warn", "error", "inform"),
                             silent = FALSE) {
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
  
  mode <- match.arg(mode)
  check_covariates <- isTRUE(check_covariates)
  check_inputs <- isTRUE(check_inputs)
  silent <- isTRUE(silent)
  err <- mode=="error"
  inform <- mode=="inform"
  
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
  
  # Check the list of what we're looking for against the list of tagged
  # parameters _in the model_
  tg <- tg[tg$tag %in% tags,]
  
  if(nrow(tg)==0) {
    msg <- "Did not find any inputs, covariates, or user tags to check."
    if(err) {
      abort(msg, use_cli_format = TRUE)
    } else {
      warn(msg, use_cli_format = TRUE)  
    } 
    return(invisible(FALSE))
  }
  
  # Now, start checking against what is in the data
  need_name <- tg$name
  need_type <- tg$tag
  
  # This associates parameter with all of their tags
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
    if(err) {
      abort(msg, footer = c(x = foot), use_cli_format = TRUE)  
    } else if(!inform) {
      warn(msg, footer = c(i = foot), use_cli_format = TRUE) 
    } else {
      inform(msg, use_cli_format = TRUE)  
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
#' declare parameters will automatically add the `input` tag (via `@input`). 
#' Once these attributes / tags are added, you can use [check_data_names()] to 
#' reconcile names of input data sets against tagged model parameters.
#' 
#' @param x mrgsolve model object.
#' 
#' @return 
#' A data frame listing parameter names and their tags.
#' 
#' @section Model specification: 
#' 
#' Note: it is good practice to tag parameters where appropriate with `input` 
#' or `covariates` as these will automatically be expected on input data when 
#' you call [check_data_names()]. User-defined tags are also possible, but you 
#' will need to alert [check_data_names()] to look for them.
#' 
#' **Examples**
#' 
#' You can use the `$INPUT` block to add the `input` tag on these parameters
#' 
#' ```
#' $INPUT 
#' STUDY = 101, WT = 70, DVID = 1
#' ```
#' Tag some covariates in the model
#' 
#' ```
#' $PARAM @covariates
#' WT = 70, SEX = 1, EGFR = 110
#' ```
#' 
#' A user-defined tag
#' 
#' ```
#' $PARAM @tag flags
#' FFLAG = 1, DFLAG = 0
#' ```
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
