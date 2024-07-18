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


# A series of functions for dealing with ev/data.frame -------

#' Just get the data.frame part
#' 
#' This is an internal function. 
#' 
#' If an event object, return `data` slot; otherwise, call as.data.frame. This 
#' is supposed to be optimized for handling event objects. 
#' 
#' @param x An R object. 
#' 
#' @noRd
to_data_frame <- function(x) {
  # Just return the data frame
  if(is.ev(x)) {
    x@data
  } else {
    as.data.frame(x)
  }
}

#' Convert an event object to data set
#' 
#' Call this function when `x` is already known to be an event object. 
#' 
#' @param x An event object.
#' @param id The subject ID. 
#' 
#' @noRd
ev_to_ds <- function(x, id = 1) {
  # Specifically for simulating a (known) ev object
  ans <- x@data
  if(nrow(ans)==0) return(ans)
  if(match("ID", names(ans), 0)==0) ans$ID <- id
  recase_ev(ans, x@case)
}

#' Create a data set from and event object or data frame
#' 
#' This is more general applicability. 
#' 
#' @noRd
As_data_set <- function(x, id = 1) {
  # Possibly handle data.frame or 
  if(is.ev(x)) return(ev_to_ds(x, id = id))
  ans <- as.data.frame(x)
  if(nrow(ans)==0) return(ans)
  if(match("ID", names(ans), 0) ==0) ans$ID <- id
  ans
}

ev_proto <- list(data = data.frame(), case = 0L)
ev_slots <- c(data = "data.frame", case = "integer")
ev_initialize <- function(.Object, case = 0L, ...) {
  .Object <- callNextMethod()
  if(!case %in% c(0, 1)) {
    stop("Event object case must be either 0 or 1.")
  }
  .Object@case <- case
  .Object
}

##' S4 events class
##' @slot data a data frame of events
##' @slot case indicates how to handle column naming upon coerce to data.frame
##' @export
##' @keywords internal
setClass("ev", prototype = ev_proto, slots = ev_slots)
setMethod("initialize", "ev", ev_initialize)

is.ev <- function(x) {
  inherits(x, "ev")  
}

##' dplyr verbs for event objects
##' 
##' @param .data the event object
##' @param ... passed to the \code{dplyr} function
##' @rdname ev_dplyr
##' @export
mutate.ev <- function(.data, ...) {
  input_cols <- names(match.call(expand.dots=TRUE))
  .data@data <- as.data.frame(mutate(.data@data, ...))
  data_cols <- names(.data@data)
  if(any(c("tinf", "total", "until") %in% input_cols)) {
    if(all(c("rate", "tinf") %in% input_cols)) {
      if(all(c("rate", "tinf") %in% data_cols)) {
        wstop("input can include either rate or tinf, not both")
      }
    }
    if(all(c("total", "addl") %in% input_cols)) {
      if(all(c("total", "addl") %in% data_cols)) {
        wstop("input can include either total or addl, not both")
      }
    }
    if(all(c("until", "addl") %in% input_cols)) {
      if(all(c("until", "addl") %in% data_cols)) {
        wstop("input can include either until or addl, not both")
      }
    }
  }
  if("rate" %in% input_cols && "tinf" %in% data_cols) {
    wstop("cannot set rate when tinf exists")  
  }
  .data@data <- finalize_ev(.data@data)
  .data
}

##' @rdname ev_dplyr
##' @export
select.ev <- function(.data, ...) {
  .data@data <- as.data.frame(dplyr::select(.data@data, ...))
  .data 
}

##' @rdname ev_dplyr
##' @export
filter.ev <-  function(.data, ...) {
  .data@data <- as.data.frame(dplyr::filter(.data@data, ...))
  .data
}

##' Various methods for event objects
##' 
##' 
##' @param x an events object
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
##' @param add_ID numeric ID of length 1 used to add \code{ID} column only if 
##' one doesn't already exist
##' @param object used for \code{show}
##' @param ... passed to various methods
##' 
##' 
##' @examples
##' 
##' e <- ev(amt = 100)
##' 
##' names(e)
##' 
##' as.data.frame(e)
##' 
##' dim(e)
##' 
##' nrow(e)
##' 
##' @rdname ev_methods
##' @name ev_methods
NULL

##' @rdname ev_methods
##' @export
names.ev <- function(x) {
  names(x@data)  
}

##' @rdname ev_methods
##' @export
dim.ev <- function(x) {
  dim(x@data)  
}

##' @method as.matrix ev
##' @rdname ev_methods
##' @export
as.matrix.ev <- function(x,...) {
  as.matrix(x@data,...) 
}

##' @method as.data.frame ev
##' @rdname ev_methods
##' @export
as.data.frame.ev <- function(x, row.names = NULL, optional = FALSE, 
                             add_ID = NULL, ...) {
  ev_to_ds(x, id = add_ID)
}

#' @rdname ev_methods
#' @export
#' @keywords internal
setMethod("show", "ev", function(object) {
  header <- "Events:\n"
  if(object@case==1) {
    header <- "Events Data:\n"  
  }
  cat(header)
  print(object@data)
  return(invisible(NULL))
})

#' Select columns from an ev object
#' 
#' 
#' @param x ev object
#' @param name column to select
#' @param i an element to select
#' @param exact not used
#' @rdname ev_extract
#' @export
setMethod("$", "ev", function(x, name){
  x@data[[name]]
})

#' @rdname ev_extract
#' @export
setMethod("[[", "ev", function(x, i, exact=TRUE) {
  x@data[[i]] 
})

finalize_ev_data <- function(data) {
  if("tinf" %in% names(data)) {
    tinf <- data[["tinf"]]
    if(any(tinf < 0)) {
      wstop("tinf must be greater than or equal to zero")
    }
    data[["rate"]] <- 0    
    if(any(tinf > 0)) {
      i <- tinf > 0
      data[["rate"]][i] <- data[["amt"]][i]/tinf[i]
    }
  }
  if("total" %in% names(data)) {
    total <- data[["total"]]
    data[["total"]] <- NULL
    if(any(total > 0)) {
      if(any(total > 1)) {
        data[["addl"]] <- total - 1
      }
    } else {
      wstop("total required to be greater than zero")
    }
  }
  if("until" %in% names(data)) {
    if(!("ii" %in% names(data))) {
      wstop("ii is required when until is specified")
    }
    until <- data[["until"]]
    data[["until"]] <- NULL
    i <- data[["ii"]] > 0
    data[["addl"]] <- 0
    data[["addl"]][i] <- ceiling((until[i] - data[["time"]][i])/data[["ii"]][i]) - 1
    data[["addl"]] <- unlist(data[["addl"]])
  }
  ev_cols <- c("ID", "time", "amt", "rate", "ii", "ss", "addl", "cmt", "evid")
  match_cols <- intersect(ev_cols, names(data))
  other_cols <- setdiff(names(data),match_cols)
  data <- data[,c(match_cols,other_cols)]
  data
}

finalize_ev <- function(x,...) {
  if(is.data.frame(x)) {
    x <- finalize_ev_data(x,...)
  } else if(is.ev(x)) {
    x@data <- finalize_ev_data(x@data,...)
  } else {
    wstop("object must be a data frame or class ev")  
  }
  x
}

lc_tran_names <- function(x) {
  if(is.data.frame(x)) {
    nlower <- sum(names(x) %in% GLOBALS$TRAN_LOWER)
    nupper <- sum(names(x) %in% GLOBALS$TRAN_UPPER)
    return(nlower >= nupper)
  } else {
    if(is.evd(x)) return(FALSE)  
    if(is.ev(x)) return(TRUE)
  }
  stop("object is neither data.frame, ev, nor evd.")
}
