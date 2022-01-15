
#' S4 evdata class
#' @slot data a data frame of events
#' @export
#' @keywords internal
setClass("evd", contains = "ev")

setAs("ev", "evd", def = function(from) {
  new("evd", data = from@data)
})

is.evd <- function(x) {
  inherits(x,"evd")  
}

#' @export
setGeneric("evd", function(x, ...) standardGeneric("evd"))
           
#' @export
setMethod("evd", "mrgmod", function(x, object = NULL, ...) {
  if(is.null(object)) {
    x@args[["events"]] <- evd(...)
    return(x)
  } 
  if(is.character(object)) {
    object <- eval(parse(text = object), envir = x@envir)
  }
  x@args[["events"]] <- object
  x
})

#' Create an evdata object
#' 
#' @param ... passed to [ev()]
#' 
#' @md
#' @export
setMethod("evd", "missing", function(x, ...) {
  as(ev(...), "evd")
})

#' @method as.data.frame evd
#' @keywords internal
#' @export
as.data.frame.evd <- function(x, row.names = NULL, optional = FALSE, 
                              add_ID = NULL, final = FALSE, ...) {
  ans <- x@data
  if(final) {
    convert <- names(ans) %in% GLOBALS$TRAN_LOWER
    names(ans)[convert] <- toupper(names(ans)[convert]) 
  }
  if(is.numeric(add_ID) & !has_ID(ans) & nrow(ans) > 0) {
    ans[["ID"]] <- add_ID[1]
  } 
  return(ans)
}

#' @keywords internal
#' @export
setMethod("show", "evd", function(object) {
  cat("Event Data:\n")
  print(as.data.frame(object))
  return(invisible(NULL))
})
