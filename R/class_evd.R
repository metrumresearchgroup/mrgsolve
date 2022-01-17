# Note: `evd` isn't really a separate class; it could be implemented that 
# way, but for not it's just an `ev()` object with a specific `case`
# attribute.


#' Create an event object with data-like names
#' 
#' This function calls [ev()] to create an event object and then sets the 
#' case attribute so that it renders as upper case tran names.
#' 
#' @param x An mrgmod object.
#' @param ... Arguments passed to [ev()].
#' 
#' @examples
#' a <- evd(amt = 100)
#' b <- ev(amt = 300)
#' a
#' as.data.frame(a)
#' as_data_set(a,b)
#' as_data_set(b,a)
#' as.data.frame(seq(a,b))
#' 
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
  set_ev_case(ev(...), case = 1L)
})

#' @param x An event object.
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
