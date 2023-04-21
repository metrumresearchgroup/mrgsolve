# Copyright (C) 2013 - 2023  Metrum Research Group
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
NULL

##' Create intervention objects from Rx input
##' 
##' See details below for Rx specification. Actual parsing is done
##' by [parse_rx()]; this function can be used to debug Rx inputs.
##' 
##' @param x a model object or `character` Rx input. 
##' @param y `character` Rx input; see details.
##' @param df if `TRUE` then a data frame is returned.
##' @param ... not used at this time.
##' 
##' @section Rx specification:
##' 
##' - The dose is found at the start of the string by sequential digits; this 
##'   may be integer, decimal, or specified in scientific notation
##' - Use `in` to identify the dosing compartment number; must be integer
##' - Use `q` to identify the dosing interval; must be integer or 
##'   decimal number (but not scientific notation)
##' - Use `over` to indicate an infusion and its duration; integer or 
##'   decimal number
##' - Use `x` to indicate total number of doses; must be integer
##' - Use `then` or `,` to separate dosing periods
##' - Use `after` to insert a lag in the start of a period; integer or 
##'   decimal number (but not scientific notation)
##' - Use `&` to implement multiple doses at the same time
##' 
##' @return The method dispatched on model object (`mrgmod`) returns another
##' model object.  The `character` method returns an event object.  The
##' `parse_rx` function return a list named with arguments for the event 
##' object constructor [ev()].
##' 
##' @examples
##' # example("ev_rx")
##' 
##' ev_rx("100")
##' 
##' ev_rx("100 in 2")
##' 
##' ev_rx("100 q12 x 3")
##' 
##' ev_rx("100 over 2")
##' 
##' ev_rx("100 q 24 x 3 then 50 q12 x 2")
##' 
##' ev_rx("100 then 50 q 24 after 12")
##' 
##' ev_rx("100.2E-2 q4")
##' 
##' ev_rx("100 over 2.23")
##' 
##' ev_rx("100 q 12 x 3")
##' 
##' ev_rx("100 in 1 & 200 in 2") 
##' 
##' parse_rx("100 mg q 24 then 200 mg q12")
##' 
##' @md
##' @rdname ev_rx
##' @export
setGeneric("ev_rx", function(x,y,...) {
  standardGeneric("ev_rx")
})

##' @rdname ev_rx
##' @export
setMethod("ev_rx", signature=c("mrgmod", "character"), function(x,y,...) {
  ev(x,ev_rx(y))
})

##' @rdname ev_rx
##' @export
setMethod("ev_rx", signature=c("character","missing"), function(x, df = FALSE, 
                                                                ...) {
  x <- parse_rx(x)
  if(is.list(x[[1]])) {
    x <- lapply(x, do.call, what = ev)  
    x <- do.call(ev_seq, x)
  } else {
    x <- do.call(ev, x)
  }
  if(df) return(as.data.frame(x))
  return(x)
})

##' @rdname ev_rx
##' @export
parse_rx <- function(x) {
  x <- strsplit(x, "then|,", perl = TRUE)[[1]]
  x <- trimws(x)
  x <- lapply(x, parse_this_rx)
  if(length(x)==1) return(x[[1]])
  return(x)
}

parse_this_rx <- function(x) {
  if(charthere(x, "&")) {
    sp <- strsplit(x, "&", fixed = TRUE)[[1]]
    sp <- sapply(sp, trimws, USE.NAMES = FALSE)
    sp <- lapply(sp, parse_this_rx)
    ev <- lapply(sp, do.call, what = ev)
    ev <- do.call("c", ev)
    return(list(ev))
  }
  dose <- reg_exec_match(x, "^ *(\\d+[\\.]*\\d*[Ee\\+\\-]{0,2}\\d*)")[[1]]
  amt <- as.numeric(dose[2])
  if(is.na(amt)) {
    stop("A dose amount is required in parse_rx.", call.=FALSE)  
  }
  number <- reg_exec_match(x, " *x *(\\d+)")[[1]]
  dur <- reg_exec_match(x,"ov[er]* +(\\d+[\\.]*\\d*) *([hdw]*)")[[1]]
  dur <- as.numeric(dur[2])
  rate <- amt/dur
  cmt <- reg_exec_match(x, "in +(\\d+)")[[1]][2]
  if(is.na(cmt)) cmt <- 1
  inter <- reg_exec_match(x, "[Qq] *(\\d+[\\.]*\\d*) *([hdw]*)")[[1]]
  at <- reg_exec_match(x, "aft[er]* +(-*\\d+[\\.]*\\d*) *([hdw]*)")[[1]][2]
  if(is.na(at)) at <- 0
  l <- list()
  l[["time"]] <- as.numeric(at)
  l[["cmt"]] <- as.numeric(cmt)
  l[["amt"]] <- amt
  l[["ii"]] <- as.numeric(inter[2])
  l[["addl"]] <- as.numeric(number[2])-1
  l[["rate"]] <- rate
  l[sapply(l,function(xx) !is.na(xx))]
}
