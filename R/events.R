# Copyright (C) 2013 - 2024  Metrum Research Group
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


#' Event objects for simulating PK and other interventions
#' 
#' An event object specifies dosing or other interventions that get implemented
#' during simulation. Event objects do similar things as [data_set], 
#' but simpler and easier to create.
#'
#' @param x a model object.
#' @param time event time.
#' @param amt dose amount.
#' @param evid event ID.
#' @param cmt compartment number or name.
#' @param ID subject ID.
#' @param replicate logical; if `TRUE`, events will be replicated for 
#' each individual in `ID`.
#' @param until the expected maximum **observation** time for this regimen; 
#' doses will be scheduled up to, but not including, the `until` time; 
#' see **Examples**.
#' @param tinf infusion time; if greater than zero, then the `rate` item 
#' will be derived as `amt/tinf`.
#' @param realize_addl if `FALSE` (default), no change to `addl` 
#' doses.  If `TRUE`, `addl` doses are made explicit with [realize_addl()].
#' @param object an event object to be added to a model object.
#' @param ... other items to be incorporated into the event object; see 
#' **Details**.
#' 
#' @details
#' - Required items in events objects include 
#'   `time`, `amt`, `evid` and `cmt`.
#' - If not supplied, `evid` is assumed to be 1.
#' - If not supplied, `cmt`  is assumed to be 1.
#' - If not supplied, `time` is assumed to be 0.
#' - If `amt` is not supplied, an error will be generated.
#' - If `total` is supplied, then `addl` will be set to `total-1`.
#' - Other items can include `ii`, `ss`, and `addl`
#'   (see [data_set] for details on all of these items).
#' - `ID` may be specified as a vector.
#' - If replicate is `TRUE` (default), then the events 
#'   regimen is replicated for each `ID`; otherwise, the number of
#'   event rows must match the number of `ID`s entered.
#' 
#' @return `ev()` returns an event object. 
#' 
#' @seealso [evd()], [ev_rep()], [ev_days()], [ev_repeat()], [ev_assign()], 
#' [ev_seq()], [mutate.ev()], [as.ev()], [as.evd()], [ev_methods].
#' 
#' 
#' @examples
#' mod <- mrgsolve::house()
#' 
#' mod <- mod %>% ev(amt = 1000, time = 0, cmt = 1)
#'
#' loading <- ev(time = 0, cmt = 1, amt = 1000)
#' 
#' maint <- ev(time = 12, cmt = 1, amt = 500, ii = 12, addl = 10)
#' 
#' c(loading, maint)
#' 
#' reduced_load <- dplyr::mutate(loading, amt = 750)
#' 
#' # Three additional doses in this case
#' e <- ev(amt = 100, ii = 4*7, until = 16*7)
#' e
#' # Last dose is given at 84
#' realize_addl(e)
#' 
#' # Four additional doses with last at 112 in this case
#' e <- ev(amt = 100, ii = 4*7, until = 16*7 + 0.001)
#' realize_addl(e)
#' 
#' @md
#' @export
setGeneric("ev", function(x, ...) {
  standardGeneric("ev")
})

#' @rdname ev
#' @export
setMethod("ev", "mrgmod", function(x, object = NULL, ...) {
  if(is.null(object)) {
    x@args[["events"]] <- ev(...)
    return(x)
  } 
  if(is.character(object)) {
    object <- eval(parse(text = object), envir = x@envir)
  }
  x@args[["events"]] <- object
  x
})

#' @rdname ev
#' @export
setMethod("ev", "missing", function(time=0, amt=0, evid=1, cmt=1, ID=numeric(0), 
                                    replicate=TRUE, until=NULL, tinf=NULL,
                                    realize_addl=FALSE, ...) {
  
  if(length(match.call())==1) { 
    return(new("ev", data = data.frame()[0,]))
  }
  
  if(any(evid==0)) {
    wstop("evid cannot be 0 (observation)")
  }
  
  if(missing(amt)) {
    wstop("argument \"amt\" is missing")
  }
  
  l <- list(time = time, cmt = cmt, amt = amt, evid = evid)
  if(is.numeric(tinf) && length(tinf) > 0) l[["tinf"]] <- tinf
  if(is.numeric(until) && length(until) > 0) l[["until"]] <- until
  
  qu <- quos(...)
  if(length(qu) > 0) {
    na1 <- names(l)
    na2 <- names(qu)
    j <- length(l)
    for(i in seq_along(qu)) {
      l[[j+i]] <- eval_tidy(qu[[i]], l)
    }
    names(l) <- c(na1,na2)
  }
  
  data <- as.data.frame(as_tibble(l), stringsAsFactors = FALSE)
  
  if(all(c("rate", "tinf") %in% names(data))) {
    wstop("input can include either rate or tinf, not both")
  }
  if(all(c("addl", "until") %in% names(data))) {
    wstop("input can include either addl or until, not both")
  }
  if(all(c("addl", "total") %in% names(data))) {
    wstop("input can include either addl or total, not both")
  }
  
  data <- finalize_ev(data)
  
  if(length(ID) > 0) {
    
    ID <- unique(ID)
    
    if(!is.numeric(ID)) {
      stop("ID must be numeric", call.=FALSE)
    }
    
    if(replicate) {
      if(any(!numeric_columns(data))) {
        data <- as.list(data)
        data <- lapply(data, unique)
        data <- do.call("expand.grid", 
                        c(list(ID=ID,stringsAsFactors=FALSE),data))
        data <- arrange__(data,.dots=c("ID", "time"))
        rownames(data) <- NULL
      } else {
        data <- expand_event_object(data,ID)
        rownames(data) <- NULL
      }
      
    } else {
      if(length(ID)!=nrow(data)) { 
        stop("length of ID does not match number of events while replicate = FALSE",
             call.=FALSE)
      }
      data[["ID"]] <- ID
    }
  }
  
  if(realize_addl) data <- realize_addl(data)
  return(new("ev", data = data))
})

#' @rdname ev
#' @export
setMethod("ev", "ev", function(x, realize_addl = FALSE, ...) {
  x <- set_ev_case(x, 0L)
  if(realize_addl) {
    return(realize_addl(x))
  } 
  x
})

#' Coerce an object to class ev
#' 
#' Use this function to convert a data frame to an event object.
#' 
#' @param x an object to coerce.
#' @param keep_id if `TRUE`, ID column is retained if it exists.
#' @param clean if `TRUE`, only dosing or ID information is retained in
#' the result.
#' @param ... not used.
#' 
#' @details
#' If `CMT` (or `cmt`) is missing from the input, it will be set to 1
#' in the event object.
#' 
#' If `TIME` (or `time`) is missing from the input, it will be set to 
#' 0 in the event object.
#' 
#' If `EVID` (or `evid`) is missing from the input, it will be set to 
#' 1 in the event object.
#' 
#' @examples
#' data <- data.frame(AMT = 100) 
#' 
#' as.ev(data)
#' 
#' as.ev(data, clean = TRUE)
#' 
#' @return 
#' An object with class ev.
#' 
#' @md
#' @export
setGeneric("as.ev", function(x, ...) {
  standardGeneric("as.ev")
})

df_to_ev <- function(x, keep_id = TRUE, clean = FALSE, ...) {
  
  if(nrow(x) == 0) {
    return(new("ev", data = data.frame()))
  }
  
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  convert <- c("TIME", GLOBALS[["CARRY_TRAN_UC"]])
  upper <- intersect(convert, names(x))
  
  if(length(upper) > 0) {
    where <- match(upper, names(x))
    names(x)[where] <- tolower(names(x)[where])
  }
  
  if(!has_name("cmt",x)) {
    x[["cmt"]] <- 1 
  }
  
  if(!has_name("time", x)) {
    x[["time"]] <- 0 
  }
  
  if(!has_name("evid", x)) {
    x[["evid"]] <- 1 
  } else {
    x[["evid"]] <- na2zero(x[["evid"]])
    x <- x[x[["evid"]] != 0,] 
    if(nrow(x)==0) {
      wstop("no dosing events found; could not coerce to ev object.") 
    }
  }
  
  if(has_ID(x) & !keep_id) x[,"ID"] <- NULL
  
  if(clean) {
    keep <- c("ID", GLOBALS[["CARRY_TRAN_LC"]])
    keep <- intersect(keep, names(x))
    x <- x[,keep, drop = FALSE]
  }
  
  x <- finalize_ev(x)
  
  new("ev", data = x)
}

#' @rdname as.ev
#' @export
setMethod("as.ev", "data.frame",  df_to_ev)

#' @rdname as.ev
#' @export
setMethod("as.ev", "ev", function(x, ...) {
  x@case <- 0L
  x
})

f_collect_prep <- function(x) {
  if(!inherits(x, c("ev", "data.frame"))) {
    wstop("all objects must have class ev or data.frame.")  
  }
  x <- to_data_frame(x)
  if(!"ID" %in% names(x)) x[["ID"]] <- 1
  x
}

collect_ev <- function(...) {
  x <- list(...)
  tran <- c("ID", GLOBALS$TRAN_LOWER)
  # The case of the first object will determine 
  # the case of the output; 0 = lc, 1 = uc
  case <- x[[1]]@case
  if(case==0) {
    x <- lapply(x, lctran) 
    x <- lapply(x, as.ev)
  } else {
    x <- lapply(x, uctran) 
    x <- lapply(x, as.ev)
    x <- lapply(x, as.evd)
  }
  x <- lapply(x, f_collect_prep)
  ids <- lapply(x, "[[", "ID")
  nid <- sapply(ids, function(tid) length(unique(tid)))
  idn <- cumsum(c(0, nid[-length(nid)]))
  new_ids <- Map(f = `+`, ids, idn)
  x <- bind_rows(x)
  x[["ID"]] <- unlist(new_ids, use.names = FALSE)
  tran <- intersect(tran, names(x))
  what <- names(x) %in% tran
  for(col in which(what)) {
    x[[col]] <- na2zero(x[[col]])  
  }
  na.check <- which(!what)
  if(length(na.check) > 0) {
    if(anyNA(x[, na.check])) {
      warning("missing values in some columns",call.=FALSE)
    }
  }
  take <- unique(c(match(tran,names(x)),seq_along(names(x))))
  x <- x[, take, drop = FALSE]
  if(is.na(timename(x))) {
    wstop("no time or TIME column in the data set") 
  }
  if(is.na(cmtname(x))) {
    wstop("no cmt or CMT column in the data set") 
  }
  if(!"ID" %in% names(x)) {
    wstop("no ID column in the data set")
  }
  x <- finalize_ev(x)
  if(case > 0) {
    x <- recase_ev(x, case)  
  }
  return(x)
}


#' Operations for ev objects
#'
#' @param e1 object on left hand side of operator (lhs)
#' @param e2 object on right hand side of operator (rhs)
#' @name ev_ops
#' 
#' @aliases +,ev,ev-method
#' @docType methods
#'
#' @details
#' All operations involving \code{\link[=mrgmod-class]{mrgmod}} 
#' objects have been deprecated.
#'
#' @rdname ev_ops
#' @keywords internal
setMethod("+", signature(e1="ev", e2="ev"), function(e1,e2) {
  #stop("e1 + e2 operation is now deprecated")
  return(add.ev(e1,e2))
})

#' @rdname ev_ops
#' @export
#' @keywords internal
setGeneric("%then%", function(e1,e2) standardGeneric("%then%"))

#' @rdname ev_ops
#' @export
#' @keywords internal
setMethod("%then%",c("ev", "ev"), function(e1, e2) {
  ev_seq(e1, e2)
  # left <- e1@data
  # if(!has_name("ii",left) | !has_name("addl",left)) {
  #   stop("both ii and addl are required in lhs",call.=FALSE)
  # }
  # y <- max(with(left, time + ii*addl + ii))
  # e2@data$time <- y
  # add.ev(e1,e2)
})

#' @rdname ev_ops
#' @export
#' @keywords internal
setMethod("+", c("ev", "numeric"), function(e1, e2) {
  stop("e1 + numeric operation is deprecated")
  # e1@data$time <- e1@data$time + e2
  # e1
})

#' @param x an ev object
#' @param recursive not used
#' @param ... other ev objects to collect
#' 
#' @rdname ev_ops
#' @export
setMethod("c", "ev", function(x, ..., recursive = TRUE) {
  y <- list(...)
  if(length(y)==0) return(x)
  case <- x@case
  for(i in seq_along(y)) {
    x <- add.ev(x, y[[i]])
  }
  x@case <- case
  return(x)
})

add.ev <- function(e1, e2) {
  
  short <- setdiff(names(e1@data), names(e2@data))
  long <- setdiff(names(e2@data), names(e1@data))
  
  if(any(short=="ID") | any(long=="ID")) {
    stop("ID found in one ev object but not the other.")
  }
  
  if(length(short) > 0) {
    add <- as.list(rep(0, length(short)))
    names(add) <- short
    e2@data <- cbind(e2@data, add)
  }
  
  if(length(long) > 0) {
    add <- as.list(rep(0, length(long)))
    names(add) <- long
    e1@data <- cbind(e1@data, add)
  }
  
  e1@data <- as.data.frame(bind_rows(e1@data, e2@data))
  
  if("ID" %in% names(e1@data)) {
    e1@data <- e1@data[order(e1@data$ID, e1@data$time), ]
  } else {
    e1@data <- e1@data[order(e1@data$time), ]
  }
  return(e1)
}

#' Replicate an event object
#' 
#' An event sequence can be replicated a certain number of
#' times in a certain number of IDs.
#' 
#' @param x event object.
#' @param ID numeric vector if IDs.
#' @param n passed to [ev_repeat()].
#' @param wait passed to [ev_repeat()].
#' @param as.ev if `TRUE` an event object is returned.
#' @param id deprecated; use `ID` instead.
#' 
#' @seealso [ev_repeat()]
#' 
#' @examples
#' 
#' e1 <- c(ev(amt=100), ev(amt=200, ii=24, addl=2, time=72))
#' 
#' ev_rep(e1, 1:5)
#' 
#' @return
#' A single data.frame or event object as 
#' determined by the value of [as.ev()].
#' 
#' @md
#' @export
ev_rep <- function(x, ID = 1, n = NULL, wait = 0, as.ev = FALSE, id = NULL) {
  if(!missing(id)) {
    warning("id argument is deprecated; use ID instead")
    ID <- id
  }
  if(!inherits(x, c("data.frame", "ev"))) {
    stop("x must be a data.frame or ev object.")
  }
  case <- ifelse(is.ev(x), x@case, 0)
  data <- expand_event_object(to_data_frame(x), ID)
  if(!is.null(n)) {
    if(n  > 1) {
      data <- ev_repeat(data, n = n, wait = wait)
    }
  }
  rownames(data) <- NULL
  if(isTRUE(as.ev)) {
    set_ev_case(as.ev(data), case)
  } else {
    recase_ev(data, case)
  }
} 

#' Repeat a block of dosing events
#' 
#' @param x event object or dosing data frame.
#' @param n number of times to repeat.
#' @param wait time to wait between repeats.
#' @param as.ev if `TRUE`, an event object is returned; otherwise a data.frame 
#' is returned.
#' 
#' @return 
#' See `as.ev` argument.
#' 
#' @examples
#' e1 <- ev(amt = 100, ii = 24, addl = 20)
#' e4 <- ev_repeat(e1, n = 4, wait = 168)
#' mod <- mrgsolve::house()
#' out <- mrgsim(mod, events = e4, end = 3200)
#' plot(out, "CP")
#' 
#' @md
#' @export
ev_repeat <- function(x, n, wait = 0, as.ev = FALSE) {
  if(!inherits(x, c("data.frame", "ev"))) {
    stop("x must be a data frame or ev object.")  
  }
  case <- ifelse(is.ev(x), x@case, 1)
  x <- to_data_frame(x)
  if(!exists("ii", x)) {
    x["ii"] <- 0
  }
  if(!exists("addl", x)) {
    x["addl"] <- 0
  }
  start <- x[1, "time"]
  end <- x$time + x$ii*x$addl + x$ii
  end <- max(end) + wait
  out <- vector("list", n)
  for(i in seq_len(n)) {
    nxt <- x
    nxt$time <- start + nxt$time + end*(i-1)
    out[[i]] <- nxt
  }
  out <- as.data.frame(bind_rows(out))
  if("ID" %in% names(out)) {
    out <- out[order(out$ID, out$time),, drop = FALSE]
    out <- out[, unique(c("ID", colnames(out))), drop = FALSE]
    rownames(out) <- NULL
  }
  if(isTRUE(as.ev)) {
    set_ev_case(as.ev(out), case)
  } else {
    recase_ev(out, case)
  }
}

#' Schedule a series of event objects
#' 
#' Use this function when you want to schedule two or more event objects in time
#' according the dosing interval (`ii`) and additional doses (`addl`).
#' 
#' @param ... event objects or numeric arguments named `wait` or `ii` to 
#' implement a period of no-dosing activity in the sequence (see **Details**).
#' @param ID numeric vector of subject IDs.
#' @param .dots a list of event objects that replaces `...`.
#' @param id deprecated; use `ID`.
#' 
#' @details
#' Use the generic [seq()] when the first argument is an event object.  If a 
#' waiting period (`wait` or `ii`) is the first event, you will need to use 
#' [ev_seq()].  When an event object has multiple rows, the end time for that 
#' sequence is taken to be one dosing interval after the event that takes place
#' on the last row of the event object. 
#' 
#' The doses for the next event line start after all of the doses from the 
#' previous event line plus one dosing interval from the previous event line 
#' (see **Examples**).  
#' 
#' When numerics named `wait` or `ii` are mixed in with the event objects, 
#' a period with no dosing activity is incorporated into the sequence,
#' between the adjacent dosing event objects. `wait` and `ii` accomplish a 
#' similar result, but differ by the starting point for the inactive period.
#' 
#' - Use `wait` to schedule the next dose relative to the end of the dosing 
#'   interval for the previous dose. 
#' - Use `ii` to schedule the next dose relative to the time of the the previous 
#'   dose.
#' 
#' So `wait` acts like similar to an event object, by starting the waiting 
#' period from one dosing interval after the last dose while `ii` starts the 
#' waiting period from the time of the last dose itself. Both `wait` and `ii` 
#' can accomplish identical behavior depending on whether the last dosing 
#' interval is included (or not) in the value. Values for `wait` or `ii` can 
#' be negative.
#' 
#' __NOTE__: `.ii` had been available historically as an undocumented feature. 
#' Starting with mrgsolve version `0.11.3`, the argument will be called `ii`. 
#' For now, both `ii` and `.ii` will be accepted but you will get a deprecation
#' warning if you use `.ii`. Please use `ii` instead.
#' 
#' Values for `time` in any event object act like a prefix time spacer wherever 
#' that event occurs in the event sequence (see **Examples**).
#' 
#' @examples
#' e1 <- ev(amt = 100, ii = 12, addl = 1)
#' 
#' e2 <- ev(amt = 200)
#' 
#' seq(e1, e2)
#' 
#' seq(e1, ii = 8, e2)
#' 
#' seq(e1, wait = 8, e2)
#' 
#' seq(e1, ii = 8, e2, ID = seq(10))
#' 
#' ev_seq(ii = 12, e1, ii = 120, e2, ii = 120, e1)
#' 
#' seq(ev(amt = 100, ii = 12), ev(time = 8, amt = 200))
#'
#' 
#' @return
#' A single event object sorted by `time`.
#' 
#' @md
#' @export
ev_seq <- function(..., ID = NULL, .dots = NULL, id = NULL) {
  
  if(!missing(id)) {
    warning("id argument is deprecated; using ID instead.")
    ID <- id
  }
  
  evs <- list(...)
  
  if(is.list(.dots)) {
    evs <- .dots 
  }
  out <- vector("list", length(evs))
  if(is.null(names(evs))) {
    names(evs) <- rep(".", length(evs))
  }
  case <- NULL
  start <- 0
  .ii <- NA_real_
  ii <- 0
  for(i in seq_along(out)) {
    this_name <- names(evs)[i]
    # TODO: refactor once is.numeric is handled
    spacer <- is.atomic(evs[[i]]) && is.numeric(evs[[i]]) && length(evs[[i]])==1
    # TODO: deprecate .ii
    if(this_name == ".ii" & spacer) {
      deprecate_warn(
        when = "0.11.3", 
        what = "ev_seq(.ii='has been renamed to ii')", 
        details = c(
          i = "Use `ii` instead of `.ii` to set time between doses." 
        )
      )
      this_name <- "ii"
    }
    if(this_name=="ii" & spacer) {
      .ii <- evs[[i]]
      evs[[i]] <- list()
      next
    }
    if(this_name=="wait" & spacer) {
      start <- start + eval(evs[[i]])
      evs[[i]] <- list()
      next
    }
    if(is.data.frame(evs[[i]])) {
      err <- c(
        "\n", 
        "found object with class: data.frame", 
        "\n",
        "please coerce to event object with `as.ev()`",
        "\n",
        "ev_seq() requires event objects"
      )
      stop(err)
    }
    if(!is.ev(evs[[i]])) {
      err <- c(
        "\n",
        "found object with class: ", 
        paste0(class(evs[[i]]), collapse = ", "),
        "\n",
        "ev_seq() requires event objects"
      )
      stop(err)  
    }
    if(is.null(case)) case <- evs[[i]]@case
    e <- evs[[i]]@data
    if(!("ii" %in% names(e))) {
      e[["ii"]] <- 0
    }
    if(!("addl" %in% names(e))) {
      e[["addl"]] <- 0
    }
    after <-  ifelse(is.null(e[[".after"]]), 0, e[[".after"]])
    e[["time"]] <- e[["time"]] + start + ifelse(!is.na(.ii), .ii, ii)
    elast <- slice(e, nrow(e))
    start <- 
      elast[["time"]] + 
      after + 
      elast[["ii"]]*elast[["addl"]] 
    out[[i]] <- e
    .ii <- NA_real_
    ii <- elast[["ii"]]
  }
  out <- bind_rows(out) 
  out[[".after"]] <- NULL
  if("rate" %in% names(out)) {
    out[["rate"]] <- na2zero(out[["rate"]])
  }
  if("ss" %in% names(out)) {
    out[["ss"]] <- na2zero(out[["ss"]]) 
  }
  if(is.numeric(ID)) {
    out <- ev_rep(out, ID)
  }
  ans <- as.data.frame(out, stringsAsFactors = FALSE)
  ans <- ans[order(ans$time),, drop = FALSE]
  rownames(ans) <- NULL
  set_ev_case(as.ev(ans), case)
}

#' @export
#' @rdname ev_seq
seq.ev <- function(...) {
  ev_seq(...) 
}
