# Note: `evd` isn't really a separate class; it could be implemented that 
# way, but for not it's just an `ev()` object with a specific `case`
# attribute.


#' Create an event object with data-like names
#' 
#' This function calls [ev()] to create an event object and then sets the 
#' case attribute so that it renders nmtran data names in upper case. An 
#' object created with [evd()] can be used in the same way as an object
#' created with [ev()].
#' 
#' Note that `evd` isn't a separate class; it is just an `ev` object with 
#' a specific `case` attribute. See examples which illustrate the difference.
#' 
#' @param x an mrgmod object.
#' @param ... arguments passed to [ev()].
#' 
#' @examples
#' a <- evd(amt = 100)
#' b <- ev(amt = 300)
#' a
#' as.data.frame(a)
#' as_data_set(a, b)
#' as_data_set(b, a)
#' as.data.frame(seq(a, b))
#' 
#' @seealso [ev()], [lctran()], [uctran()]
#' 
#' @md
#' @export
setGeneric("evd", function(x, ...) standardGeneric("evd"))

#' @rdname evd
#' @export
setMethod("evd", "mrgmod", function(x, ...) {
  x <- ev(x, ...)
  x@args[["events"]] <- set_ev_case(x@args[["events"]], 1L)
  x
})

#' @rdname evd
#' @export
setMethod("evd", "missing", function(...) {
  set_ev_case(ev(...), 1L)
})

#' @rdname evd
#' @export
setMethod("evd", "ev", function(x, ...) {
  set_ev_case(x, 1L)
})

#' @param x an event object.
#' @rdname evd
#' @export
as.evd <- function(x) {
  if(!is.ev(x)) stop("evnt must be an ev object.")
  x@case <- 1L
  x
}

set_ev_case <- function(x, case) {
  if(!is.ev(x)) return(x)
  x@case <- case
  x
}

# This actually changes names of the result
# For now, 0 = as-is; 1 = all uppercase
recase_ev <- function(data, case = 0) {
  if(case==0) return(data)
  convert <- names(data) %in% GLOBALS$TRAN_LOWER
  names(data)[convert] <- toupper(names(data)[convert]) 
  data
}
