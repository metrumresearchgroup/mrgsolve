##' S4 events class
##' @slot data a data frame of events
##' @export
setClass("ev", slots=c(data="data.frame"))

is.ev <- function(x) {
  inherits(x,"ev")  
}

