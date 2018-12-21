# Copyright (C) 2013 - 2018  Metrum Research Group, LLC
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

##' @include class_ev.R
##' @include events.R


parse_this_rx <- function(x) {
  dose <- reg_exec_match(x, "^ *([0-9]+)")[[1]]
  amt <- as.numeric(dose[2])
  if(is.na(amt)) {
    stop("A dose amount is required in parse_rx.", call.=FALSE)  
  }
  number <- reg_exec_match(x, " *x *([0-9]+)")[[1]]
  dur <- reg_exec_match(x,"ov[er]* +([0-9]+) *([hdw]*)")[[1]]
  dur <- as.numeric(dur[2])
  rate <- amt/dur
  cmt <- reg_exec_match(x, "in +([0-9]+)")[[1]][2]
  if(is.na(cmt)) cmt <- 1
  inter <- reg_exec_match(x, "[Qq] *([0-9]+) *([hdw]*)")[[1]]
  at <- reg_exec_match(x, "aft[er]* +([0-9]+) *([hdw]*)")[[1]][2]
  if(is.na(at)) at <- 0
  l <- list()
  l[["amt"]] <- as.numeric(dose[2])
  l[["cmt"]] <- as.numeric(cmt)
  l[["addl"]] <- as.numeric(number[2])-1
  l[["rate"]] <- rate
  l[["ii"]] <- as.numeric(inter[2])
  l[["time"]] <- as.numeric(at)
  structure(l, class="sig")
}

##' Parse Rx directions
##' 
##' An alternative to call int [ev] with 
##' named arguments.  
##' 
##' @param x an Rx string; see `details` and `examples`
##' @return An event object
##' 
##' @details
##' 
##' - The dose is found at the start of the string by sequential digits
##' - Use `in` to identify the dosing compartment number
##' - Use `q` to identify the dosing interval
##' - Use `over` to indicate an infusion and its duration
##' - Use `x` to indicate total number of doses
##' - Use `then` or `,` to separate dosing periods
##' - User `after` to insert a lag in the start of a period
##' 
##' @examples
##' # examples("parse_rx")
##' 
##' parse_rx("100")
##' 
##' parse_rx("100 in 2")
##' 
##' parse_rx("100 q12 x 3")
##' 
##' parse_rx("100 over 2")
##' 
##' parse_rx("100 q 24 x 3 then 50 q12 x 2")
##' 
##' parse_rx("100 then 50 q 24 after 12")
##' 
##' @seealso [ev_rx]
##' @md
##' @export
parse_rx <- function(x) {
  x <- strsplit(x, "then|,")[[1]]
  x <- trimws(x)
  ans <- lapply(x, parse_this_rx)
  ans <- lapply(ans,as.ev)
  if(length(ans) > 1) return(do.call(seq,ans))
  return(ans[[1]])
}

setClass("sig")
##' Coerce sig object to event
##' 
##' @param x a sig object
##' @param ... not used
##' @keywords internal
setMethod("as.ev", "sig", function(x,...) {
  data <- data.frame(
    time = x[["time"]], amt = x[["amt"]], cmt = x[["cmt"]],
    rate = x[["rate"]], addl = x[["addl"]], ii = x[["ii"]]
  )
  data <- dplyr::select_if(data, .p = function(x) !is.na(x))
  as.ev(dplyr::select(data,"time","cmt","amt",everything()))
})

##' Create interventions from Rx input
##' 
##' See [parse_rx] for Rx specification.
##' 
##' @param x a model object
##' @param y Rx input; see [parse_rx] for details
##' @param df if `TRUE` then a data frame is returned
##' @param ... not used at this time
##' 
##' @examples
##' 
##' ev_rx("100 q 12 x 3")
##' @seealso [parse_rx]
##' @md
setGeneric("ev_rx", function(x,y,...) {
  standardGeneric("ev_rx")
})

##' @rdname ev_rx
##' @export
setMethod("ev_rx", signature=c("mrgmod", "character"), function(x,y,...) {
  ev(x,parse_rx(y))
})

##' @rdname ev_rx
##' @export
setMethod("ev_rx", signature="character", function(x,df = FALSE,...) {
  x <- ev(parse_rx(x))
  if(df) return(as.data.frame(x))
  return(x)
})
