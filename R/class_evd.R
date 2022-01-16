set_ev_case <- function(x, case) {
  if(!is.ev(x)) return(x)
  x@case <- case
  x
}

#' @export
setGeneric("evd", function(x, ...) standardGeneric("evd"))

#' @export
setMethod("evd", "mrgmod", function(x, ...) {
  x <- ev(x, ...)
  x@args[["events"]] <- set_ev_case(x@args[["events"]], 1L)
  x
})

#' Create an evdata object
#'
#' @param ... passed to [ev()]
#'
#' @md
#' @export
setMethod("evd", "missing", function(...) {
  set_ev_case(ev(...), case = 1L)
})
