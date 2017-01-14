


##' S4 class for mrgsolve simulation output.
##'
##' @slot request character vector of compartments requested in simulated output
##' @slot outnames character vector of column names in simulated output coming from table step
##' @slot data matrix of simulated data
##' @slot mod the mrgmod model object
setClass("mrgsims",
         slots=c(
           request="character",
           outnames="character",
           data="data.frame",
           mod="mrgmod",
           seed="integer",
           date="character"
         )
)

setClass("batch_mrgsims",contains="mrgsims",
         slots=c(
           knobs="character",
           batch="data.frame",
           request="character",
           moving="character",
           input="list"
         )
)


##' Check if an object is mrgsim output.
##' 
##' @param x any object
##' 
##' @return \code{TRUE} if \code{x} inherits \code{mrgsims}.
##' 
##' @export
is.mrgsims <- function(x) inherits(x,"mrgsims")
