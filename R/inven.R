# Copyright (C) 2013 - 2022  Metrum Research Group
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
#' 
#' @param x model object.
#' @param obj an object with names which potentially represent model parameters.
#' @param ... capture tidy-select parameter requirements.
#' @param .strict whether to stop execution if all requirements are present
#'  (`TRUE`) or just warn (`FALSE`); see details.
#' 
#' @examples \dontrun{
#' inventory(mod, idata, CL:V)            # parameters defined, inclusively, CL through Volume 
#' inventory(mod, idata, everything())    # all parameters
#' inventory(mod, idata, contains("OCC")) # all parameters containing OCC
#' inventory(mod, idata, -F)              # all parameters except F
#' }
#' 
#' @return
#' `x` is returned invisibly.
#' 
#' @details 
#' If parameter requirements are not explicitly stated, the requirement defaults 
#' to all parameter names in `x`.  Note that, by default, the inventory is not 
#' `.strict` unless the user explicitly states the parameter requirement. That 
#' is, if parameter requirements are explicitly stated, `.strict` will be set 
#' to `TRUE` if a value `.strict` was not passed.
#' 
#' @md
#' @export
inventory <- function(x, obj, ..., .strict = FALSE) {
  
  if(!is.mrgmod(x)) {
    wstop("`x` must be a model object; pass that first.")
  }
  
  if(!is_named(obj)) {
    wstop("`obj` must be named.")  
  }

  need <- names(eval_select(expr(c(...)), as.list(Param(x))))
  
  if(!length(need)) {
    need <- Pars(x)
  } else {
    if(missing(.strict)) .strict <- TRUE 
  }
  
  missing <- setdiff(need, names(obj))
  nmiss <- length(missing) 

  if(nmiss == 0) {
    message("Found all required parameters in candidate obj.")
    return(invisible(x))
  }
  
  msg <- c(
    "missing parameters in candidate obj\n", 
    paste0("   --| ", missing, "\n")
  )

  if(.strict) {
    stop(msg, call.=FALSE)
  } else {
    warning(msg, call.=FALSE)
  }
  
  return(invisible(x))
}
