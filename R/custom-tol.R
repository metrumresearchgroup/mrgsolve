# Copyright (C) 2013 - 2025  Metrum Research Group
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


initialize_tol <- function(x, tol = c("rtol", "atol"), default = NULL) {
  tol <- match.arg(tol)
  sname_vec <- paste0("vec_", tol)
  values_vec <- slot(x, sname_vec)
  cmts <- Cmt(x)
  if(length(values_vec) == neq(x)) {
    check_vec_tol_slots(x, tol)
    return(x)
  }
  if(length(values_vec) > 0) {
    # Call this to generate the error
    check_vec_tol_slots(x, tol)
  }
  if(is.null(default)) default <- slot(x, tol)
  values <- rep(default, length(cmts))
  names(values) <- cmts
  slot(x, sname_vec) <- values
  check_vec_tol_slots(x, tol)
  x
}

customize_tol <- function(x, val = list(), tol = c("rtol", "atol"), default = NULL, ...) {
  tol <- match.arg(tol)
  val <- c(list(...), val)
  if(!length(val)) {
    val <- setNames(rep(default, neq(x)), Cmt(x))
  }
  valid_tol_data(val, x)
  val <- val[!duplicated(names(val))]
  x <- initialize_tol(x, tol = tol, default = default)
  sname_vec <- paste0("vec_", tol)
  values_vec <- slot(x, sname_vec)
  current <- as.list(slot(x, sname_vec))
  updated <- update_list(current, as.list(val))
  slot(x, sname_vec) <- unlist(updated)
  check_vec_tol_slots(x, tol)
  x
}

valid_tol_data <- function(val, x) {
  named <- is_named(val)
  type_list <- is.list(val) && all(vapply(val, single.number, TRUE))
  type_numeric <- is.atomic(val) && is.numeric(val)
  type <- type_list | type_numeric
  if(!named || !type) {
    abort("tolerance data must be a named numeric list or vector.")
  }
  bad <- setdiff(names(val), Cmt(x))
  if(length(bad)) {
    msg <- "Invalid compartment names in tolerance data:"
    names(bad) <- rep("x", length(bad))
    abort(c(msg, bad))
  }
}

check_vec_tol_slots <- function(x, scope = c("both", "rtol", "atol")) {
  scope <- match.arg(scope)
  n <- neq(x)
  cmts <- Cmt(x)
  msg <- NULL
  if(scope %in% c("both", "rtol")) {
    check_rtol <- length(x@vec_rtol) == n
    check_rtol_names <- identical(names(x@vec_rtol), cmts)
    no_names <- !is_named(x@vec_rtol)
    if(!check_rtol) {
      msg <- c(msg, "custom rtol vector is not the correct size.")
    }
    if(check_rtol && !check_rtol_names) {
      if(no_names) {
        msg <- c(msg, "custom rtol vector is unnamed.")
      } else {
        msg <- c(msg, "custom rtol vector is in the wrong order.")  
      }
    }
  }
  if(scope %in% c("both", "atol")) { 
    check_atol <- length(x@vec_atol) == n
    check_atol_names <- identical(names(x@vec_atol), cmts)
    no_names <- !is_named(x@vec_atol)
    if(!check_atol) {
      msg <- c(msg, "custom atol vector is not the correct size.")
    }
    if(check_atol && !check_atol_names) {
      if(no_names) {
        msg <- c(msg, "custom atol vector is unnamed.")
      } else {
        msg <- c(msg, "custom atol vector is in the wrong order.")  
      }
    }
  }
  if(is.null(msg)) return(invisible(TRUE))
  header <- "Problems were found in custom tolerance vectors:"
  footer <- "Consider runing `reset_tolerances()` to start over."
  names(msg) <- "x"
  abort(message = header, body = msg, footer = footer)
}

#' Customize tolerances for specific compartments
#' 
#' These functions update the relative or absolute tolerance values
#' only for the custom tolerance configuration. 
#' 
#' @param .x a model object.
#' @param .rtol a named numeric list or vector, where names reference
#' selected model compartments and relative tolerances for those 
#' compartments; `custom_tol()` also accepts the name of an object in the 
#' model environment to be used. 
#' @param .atol a named numeric list or vector, where names reference
#' selected model compartments and absolute tolerances for those 
#' compartments; `custom_tol()` also accepts the name of an object in the 
#' model environment to be used.  
#' @param .default the default tolerance value to use for compartments not 
#' listed in `.rtol`, `.atol`, or `...`; if not supplied, the current scalar 
#' value in `.x` will be used.
#' @param .use `logical`; if `TRUE`, then a call to [use_custom_tol()] will be 
#' made prior to return; if `FALSE`, a call to [use_scalar_tol()] will be made; 
#' under expected use, the value for this argument is kept `TRUE`, so that 
#' whenever tolerances are customized, they will be used in the next simulation 
#' run. 
#' @param ... `name`/`value` pairs, where `name` references a model compartment
#' and `value` is a new, numeric value to use for `rtol` or `atol`. 
#' 
#' @details
#' New tolerance values can be supplied by either a named, numeric vector or 
#' list via `.rtol` and `.atol` or via `...` or by both. If duplicate 
#' compartment names are found in `...` and either `.rtol` or `.atol`, the value 
#' passed via `...` will take precedence. 
#' 
#' The `custom_tol()` function provides a mechanism for coding customized
#' tolerances into the model file itself. Simply create named numeric lists
#' or vectors for customized `rtol` or `atol` in a `$ENV` block. On loading 
#' the model, call `custom_tol()` and supply the names of those objects as
#' `.rtol` and `.atol`. 
#' 
#' @examples
#' mod <- house()
#' mod <- custom_rtol(mod, GUT = 1e-2, CENT = 1e-3)
#' 
#' new_tolerances <- c(GUT = 1e-4, RESP = 1e-5)
#' mod <- custom_rtol(mod, new_tolerances, RESP = 1e-6)
#' 
#' @name custom_tol
#' @md
#' @export 
custom_tol <- function(.x, .rtol = NULL, .atol = NULL) {
  if(is.character(.rtol) && length(.rtol)==1) {
    .rtol <- get(.rtol, envir = env_get_env(.x), inherits = FALSE, mode = "numeric")
  }
  if(is.character(.atol) && length(.atol)==1) {
    .atol <- get(.atol, envir = env_get_env(.x), inherits = FALSE, mode = "numeric")
  }
  .x <- custom_rtol(.x, .rtol = .rtol, .default = .x@rtol)
  .x <- custom_atol(.x, .atol = .atol, .default = .x@atol)
  .x
}

#' @rdname custom_tol
#' @export
custom_rtol <- function(.x, .rtol = list(), .default = NULL, .use = TRUE, ...) {
  if(!is.mrgmod(.x)) mod_first() 
  .x <- customize_tol(x = .x, val = .rtol, tol = "rtol", default = .default, ...)
  if(isTRUE(.use)) {
    .x <- use_custom_tol(.x)    
  } 
  if(isFALSE(.use)) {
    .x <- use_scalar_tol(.x)
  }
  .x
}

#' @rdname custom_tol
#' @export
custom_atol <- function(.x, .atol = list(), .default = NULL, .use = TRUE, ...) {
  if(!is.mrgmod(.x)) mod_first()
  .x <- customize_tol(x = .x, val = .atol, tol = "atol", default = .default, ...) 
  if(isTRUE(.use)) {
    .x <- use_custom_tol(.x)    
  } 
  if(isFALSE(.use)) {
    .x <- use_scalar_tol(.x)
  }
  .x
}

#' Reset all model tolerances
#' 
#' These functions reset both scalar and customized values for both 
#' relative and absolute tolerances. All functions reset both customized
#' and scalar tolerances to a single, common `rtol` or `atol`.  The functions 
#' do _not_ change the which tolerance configuration is used for 
#' simulation (e.g., scalar or customized); see [use_custom_tol()] and 
#' [use_scalar_tol()] to make that change in the model object.
#' 
#' @param x a model object.
#' @param rtol global relative tolerance for both scalar and customized
#' configurations; if not supplied, the current model scalar `rtol` value is 
#' used. 
#' @param atol global absolute tolerance for both scalar and customized
#' configurations; if not supplied, the current model scalar `atol` value is 
#' used. 
#' 
#' @examples
#' mod <- house()
#' mod <- reset_tol(mod, rtol = 1e-6, atol = 1e-10)
#' mod
#' 
#' @name reset_tol
#' @md
#' @export
reset_tol <- function(x, rtol = NULL, atol = NULL) {
  if(!is.mrgmod(x)) mod_first()
  if(!is.numeric(rtol)) {
    rtol <- x@rtol
  }
  if(!is.numeric(atol)) {
    atol <- x@atol
  }
  x <- reset_rtol(x, rtol = rtol)
  x <- reset_atol(x, atol = atol)
  x
}
#' @rdname reset_tol
#' @export
reset_rtol <- function(x, rtol = NULL) {
  if(!is.mrgmod(x)) mod_first()
  if(!is.numeric(rtol)) {
    rtol <- x@rtol  
  }
  x@vec_rtol <- numeric(0)
  x <- update(x, rtol = rtol)
  x <- initialize_tol(x, default = rtol, tol = "rtol")
  x
}

#' @rdname reset_tol
#' @export
reset_atol <- function(x, atol = NULL) {
  if(!is.mrgmod(x)) mod_first()
  if(!is.numeric(atol)) {
    atol <- x@atol  
  }
  x@vec_atol <- numeric(0)
  x <- update(x, atol = atol)
  x <- initialize_tol(x, default = atol, tol = "atol")
  x
}


#' Extract rtol and atol information from a model object
#' 
#' @param x a model object.
#' 
#' @examples
#' mod <- house()
#' get_tol(mod)
#' get_tol_list(mod)
#' 
#' @name get_tol
#' @md
#' @export
get_tol <- function(x) {
  if(!is.mrgmod(x)) mod_first()
  rtol <- x@vec_rtol
  atol <- x@vec_atol
  if(!length(rtol)) {
    rtol <- rep(NA_real_, neq(x))  
  } else {
    check_vec_tol_slots(x, "rtol")  
  }
  if(!length(atol)) {
    atol <- rep(NA_real_, neq(x))   
  } else {
    check_vec_tol_slots(x, "atol")  
  }
  data.frame(
    cmt = Cmt(x),
    custom_rtol = rtol, 
    custom_atol = atol, 
    scalar_rtol = x@rtol, 
    scalar_atol = x@atol, 
    row.names = NULL, 
    stringsAsFactors = FALSE
  )
}

#' @rdname get_tol
#' @export
get_tol_list <- function(x) {
  if(!is.mrgmod(x)) mod_first()
  data <- get_tol(x)
  cmt <- data$cmt
  data$cmt <- NULL
  l <- as.list(data)
  l <- lapply(as.list(data), function(tol) {
    setNames(as.list(tol), cmt)  
  })
  l
}

#' Set up a model object to use either scalar or custom tolerances
#' 
#' Call `use_custom_tol()` to use custom relative and absolute tolerances in 
#' a model; call `use_scalar_tol()` to revert to the traditional configuration
#' where a single `rtol` and `atol` are applied to all compartments. 
#' 
#' @param x a model object.
#' 
#' @details
#' If customized tolerances have not been initialized yet, they will be,
#' assigning the current `rtol` or `atol` for every compartment. These default
#' values can be updated using [custom_rtol()], [custom_atol()], or 
#' [custom_tol()].
#' 
#' @examples
#' mod <- house()
#' 
#' mod <- use_custom_tol(mod)
#' mod
#' 
#' mod <- use_scalar_tol(mod)
#' mod
#' 
#' 
#' @md
#' @export
use_custom_tol <- function(x) {
  if(!is.mrgmod(x)) mod_first()
  x <- initialize_tol(x, default = x@rtol, tol = "rtol")
  x <- initialize_tol(x, default = x$atol, tol = "atol")
  x@itol <- 4
  x
}

#' @rdname use_custom_tol
#' @export
use_scalar_tol <- function(x) {
  if(!is.mrgmod(x)) mod_first()
  x@itol <- 1  
  x
}
