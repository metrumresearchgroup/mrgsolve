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

#' Select and modify a data set for simulation
#' 
#' The input data set (`data_set`) is a data frame that specifies
#' observations, model events, and / or parameter values for a population
##' of individuals. 
#'
#' @param x a model object. 
#' @param data input data set as a data frame.
#' @param .subset an unquoted expression passed to 
#' [dplyr::filter()]; retain only certain rows in the data set
#' @param .select passed to [dplyr::select()]; retain only certain 
#' columns in the data set; this should be the result of a call to 
#' [dplyr::vars()].
#' @param object character name of an object existing in `$ENV` 
#' to use for the data set.
#' @param need passed to [inventory()].
#' @param ... other arguments passed along when `object` is a function.
#' 
#' @details
#' Input data sets are `R` data frames that can include columns 
#' with any valid name, however columns with selected names are 
#' treated specially by mrgsolve and incorporated into the 
#' simulation.
#'
#' `ID` specifies the subject ID and is required for every 
#' input data set.
#'
#' When columns have the same name as parameters (`$PARAM` or `$INPUT` in 
#' the model specification file), the values in those columns will 
#' be used to update the corresponding parameter as the simulation 
#' progresses.
#'
#' Input data set may include the following columns related to 
#' PK dosing events: `TIME`, `CMT`, `AMT`, `RATE`, `II`, `ADDL`, `SS`.  
#' Both `ID` and `TIME` are required columns in the input data set unless 
#' `$PRED` is in use.  Lower case PK dosing column names including
#' `time`, `cmt`, `amt`, `rate`, `ii`, `addl`, `ss` are also recognized.  
#' However, an error will be generated if a mix of both upper case and lower
#' case columns in this family are found. Use the functions [lctran()] and 
#' [uctran()] to convert between upper and lower case naming for these 
#' data items.
#'  
#' `TIME` is the observation or event time, `CMT` is the compartment number 
#' (see [init()]), `AMT` is the dosing amount, `RATE` is the infusion rate, 
#' `II` is the dosing interval, `ADDL` specifies additional doses to 
#' administer, and `ss` is a flag indicating that the system should be advanced 
#' to a pharmacokinetic steady state prior to administering the dose.  These 
#' column names operate similarly to other non-linear mixed effects modeling 
#' software. 
#' 
#' `EVID` is an integer value specifying the ID of an event record. Values
#' include: 
#'   - 0: observation
#'   - 1:  dose event, either bolus or infusion
#'   - 2: other-type event; in mrgsolve, this functions like an observation 
#'     record, but a discontinuity is created in the simulation at the time of 
#'     the event (i.e., the ODE solver will stop and restart at the time of the 
#'     event)
#'  - 3: reset the system 
#'  - 4: reset the system and dose
#'  - 8: replace the amount in a compartment
#'  
#' For all `EVID` greater than `0`, a discontinuity is created in the
#' simulation, as described for `EVID 2`.  
#'  
#' An error will be generated when mrgsolve detects that the data set
#' is not sorted by `time` within an individual. mrgsolve does **not** allow time
#' to be reset to zero on records where `EVID` is set to 4 (reset and dose).
#' 
#' Only numeric data can be brought in to the problem. Any non-numeric data 
#' columns will be dropped with warning. See [numerics_only()], which is used 
#' to prepare the data set. 
#' 
#' An error will be generated if any parameter columns in the 
#' input data set contain missing values (`NA`). Likewise, and error will 
#' be generated if missing values are found in the following
#' columns: `ID`, `time`/`TIME`, `rate`/`RATE`. 
#'
#' See [exdatasets] for several example data sets that are provided by 
#' mrgsolve.
#' 
#' @seealso [idata_set()], [ev()], [valid_data_set()], [valid_idata_set()], 
#' [lctran()], [uctran()].
#'
#' @examples
#'
#' mod <- mrgsolve::house()
#' 
#' data <- expand.ev(ID = seq(3), amt = c(10, 20))
#'
#' mod %>% data_set(data, ID > 1) %>% mrgsim()
#' 
#' data(extran1)
#' head(extran1)
#' 
#' mod %>% data_set(extran1) %>% mrgsim()
#' mod %>% mrgsim(data = extran1)
#' 
#' @md
#' @export
setGeneric("data_set", function(x,data,...) {
  standardGeneric("data_set")
})

##' @rdname data_set
##' @export
setMethod("data_set",c("mrgmod", "data.frame"), function(x,data,.subset=TRUE,.select=TRUE,object=NULL,need=NULL,...) {
  
  if(is.character(need)) {
    suppressMessages(inventory(x, data, need))
  }
  if(!missing(.subset)) {
    data <- dplyr::filter(data,`!!`(enquo(.subset)))
  }
  if(!missing(.select)) {
    data <- dplyr::select(data,`!!!`(.select))
  }
  if(nrow(data) ==0) {
    stop("Zero rows in data after filtering.", call. = FALSE)
  }
  if(is.character(object)) {
    data <- data_hooks(data, object, x@envir, param(x), ...) 
  }
  x@args[["data"]] <- data
  return(x)
})

#' @rdname data_set
#' @export
setMethod("data_set",c("mrgmod", "ANY"), function(x, data, ...) {
  return(data_set(x, as.data.frame(data), ...))
})

#' @rdname data_set
#' @export
setMethod("data_set", c("mrgmod", "ev"), function(x, data, ...) {
  return(data_set(x, As_data_set(data), ...))
})

#' @rdname data_set
#' @export
setMethod("data_set", c("mrgmod", "missing"), function(x, object, ...) {
  object <- data_hooks(object=object,envir=x@envir,param=param(x),...)
  return(data_set(x, as.data.frame(object) ,...))
})


#' Change the case of nmtran-like data items
#' 
#' Previous data set requirements included lower case names for data items 
#' like `AMT` and `EVID`. Lower case is no longer required. However, it is still
#' a requirement that nmtran like data column names are either all lower case
#' or all upper case. 
#' 
#' Columns that will be renamed with lower or upper case versions: 
#' 
#' - `AMT  / amt`
#' - `II   / ii`
#' - `SS   / ss`
#' - `CMT  / cmt`
#' - `ADDL / addl`
#' - `RATE / rate`
#' - `EVID / evid`
#' - `TIME / time`
#' 
#' If both lower and upper case versions of the name are present in the data 
#' frame, no changes will be made. 
#' 
#' @param data a data set with nmtran-like format or an event object.
#' @param warn if `TRUE`, a warning will be issued when there are both upper
#' and lower case versions of any nmtran-like column in the data frame.
#' @param ... for potential future use.
#' 
#' @return 
#' A data frame or event object, with column names possibly converted to upper
#' or lower case.
#' 
#' @examples
#' data <- data.frame(TIME = 0, AMT = 5, II = 24, addl = 2, WT = 80)
#' lctran(data)
#' 
#' data <- data.frame(TIME = 0, AMT = 5, II = 24, addl = 2, wt = 80)
#' uctran(data)
#' 
#' ev <- evd(amt = 100, evid = 3)
#' uctran(ev)
#' 
#' # warning
#' data <- data.frame(TIME = 1, time = 2, CMT = 5)
#' lctran(data)
#' 
#' @md
#' @export
lctran <- function(data, ...) UseMethod("lctran")
#' @rdname lctran
#' @export
lctran.data.frame <- function(data, warn = TRUE, ...) {
  n <- names(data)
  infrom <- n %in% GLOBALS$TRAN_UPPER
  haslower <- tolower(n) %in% n
  change <- infrom & !haslower
  if(any(change)) names(data)[change] <- tolower(n[change])
  if(isTRUE(warn) && any(dup <- infrom & haslower)) {
    warning(
      "There are both upper and lower case versions ", 
      "of some nmtran names in the data set"
    )
  }
  data
}
#' @rdname lctran
#' @export
lctran.ev <- function(data, ...) {
  as.ev(data)
}
#' @rdname lctran
#' @export
uctran <- function(data, ...) UseMethod("uctran")
#' @rdname lctran
#' @export
uctran.data.frame <- function(data, warn = TRUE, ...) {
  n <- names(data)
  infrom <- n %in% GLOBALS$TRAN_LOWER
  hasupper <- toupper(n) %in% n
  change <- infrom & !hasupper
  if(any(change)) names(data)[change] <- toupper(n[change])
  if(isTRUE(warn) && any(dup <- infrom & hasupper)) {
    warning(
      "There are both upper and lower case versions ", 
      "of some nmtran names in the data set."
    )
  }
  data
}
#' @rdname lctran
#' @export
uctran.ev <- function(data, ...) {
  as.evd(data)
}

data_hooks <- function(data, object, envir, param = list(), ...) {
  param <- as.list(param)
  envir <- combine_list(as.list(param), as.list(envir))
  objects <- cvec_cs(object)
  args <- list(...)
  if(missing(data)) {
    data <- eval(tparse(objects[1]), envir = envir)
    if(is.function(data)) {
      data <- do.call(data, args, envir = as.environment(envir))
    } 
    objects <- objects[-1]
  } 
  for(f in objects) {
    args$data <- data
    data <- do.call(f, args, envir = as.environment(envir))
  }
  return(data)
}

#' Create a simulation data set from ev objects or data frames
#' 
#' The goal is to take a series of event objects or data frames and combine them 
#' into a single data frame that can be passed to [data_set()]. 
#'
#' @param x an ev object or data frame. 
#' @param ... additional ev objects or data frames.
#' 
#' @details
#' Each event object or data frame is added to the data frame as an `ID` or 
#' set of `ID`s  that are distinct from the `ID`s in the other event objects. 
#' Note that including `ID` argument to the [ev()] call where `length(ID)` is 
#' greater than one will render that set of events for all of `ID`s that are 
#' requested.
#' 
#' When determining the case for output names, the `case` attribute for
#' the first `ev` object passed will be used to set the case for the output
#' data.frame. In the event `x` is a data frame, the case of special column 
#' names (like `amt/AMT` or `cmt/CMT`) in the first data frame will be assessed 
#' and the case in the output data frame will be determined based on the 
#' relative numbers of lower or upper names. 
#'
#' To get a data frame with one row (event) per `ID`, look at [expand.ev()].
#' 
#' @return 
#' A data frame suitable for passing into [data_set()]. The columns will appear
#' in a standardized order. 
#'
#' @examples
#' a <- ev(amt = c(100,200), cmt=1, ID = seq(3))
#' b <- ev(amt = 300, time = 24, ID = seq(2))
#' c <- ev(amt = 1000, ii = 8, addl = 10, ID = seq(3))
#' 
#' as_data_set(a, b, c)
#' 
#' d <- evd(amt = 500)
#' 
#' as_data_set(d, a)
#' 
#' # Output will have upper case nmtran names
#' as_data_set(
#'   data.frame(AMT = 100, ID = 1:2), 
#'   data.frame(amt = 200, rate = 5, cmt = 2)
#' )
#' 
#' # Instead of this, use expand.ev
#' as_data_set(ev(amt = 100), ev(amt = 200), ev(amt = 300))
#' 
#' @seealso [expand.ev()], [expand.evd()], [ev()], [evd()], [uctran()], 
#' [lctran()] 
#'
#' @md
#' @rdname as_data_set
#' @export
setGeneric("as_data_set", function(x,...) standardGeneric("as_data_set"))

#' @rdname as_data_set
setMethod("as_data_set", "ev", function(x, ...) {
  collect_ev(x, ...)
})

#' @rdname as_data_set
setMethod("as_data_set", "data.frame", function(x, ...) {
  # Coerce the first object, then call the ev method
  lc <- lc_tran_names(x)
  if(lc) {
    x <- lctran(x)
    x <- as.ev(x)
  } else {
    x <- uctran(x)
    x <- as.ev(x)
    x <- as.evd(x)
  }
  as_data_set(x, ...)
})

#' Replicate a list of events into a data set
#' 
#' @param l list of event objects.
#' @param idata an idata set (one ID per row).
#' @param evgroup the character name of the column in `idata` that specifies 
#' event object to implement.
#' @param join if `TRUE`, join `idata` to the data set before returning.
#' 
#' @details
#' `ev_assign()` connects events in a list passed in as the `l` argument to 
#' values in the data set identified in the `evgroup` argument.  For making 
#' assignments, the unique values in the `evgroup` column are first sorted so 
#' that the first sorted unique value in `evgroup` is assigned to the first 
#' event in `l`, the second sorted value in `evgroup` column is assigned to the 
#' second event in `l`, and so on.  This is a change from previous behavior, 
#' which did not sort the unique values in `evgroup` prior to making the 
#' assignments. 
#' 
#' @examples
#' ev1 <- ev(amt = 100)
#' ev2 <- ev(amt = 300, rate = 100, ii = 12, addl = 10)
#' 
#' idata <- data.frame(ID = seq(10)) 
#' idata$arm <- 1+(idata$ID %%2)
#' 
#' ev_assign(list(ev1, ev2), idata, "arm", join = TRUE)
#' 
#' @md
#' @export
ev_assign <- function(l, idata, evgroup, join = FALSE) {
  
  idata <- as.data.frame(idata)
  
  if(!("ID" %in% colnames(idata))) {
    stop("ID column missing from idata set.", call.=FALSE) 
  }
  
  cols <- c("ii", "addl", "evid", "rate","ss","cmt", "time", "amt")
  
  zeros <- matrix(rep(0,length(cols)),nrow=1,
                  dimnames=list(NULL,cols))
  
  l <- lapply(l,as.matrix)
  
  ucols <- unique(unlist(sapply(l,colnames,simplify=FALSE,USE.NAMES=FALSE)))
  
  if(!all(ucols %in% cols)) {
    invalid <- setdiff(ucols,cols)
    invalid <- paste(invalid,collapse=", ")
    stop("invalid event data items found: ", invalid, call.=FALSE)
  }
  
  for(i in seq_along(l)) {
    miss <- setdiff(ucols,colnames(l[[i]]))
    if(length(miss) > 0) {
      l[[i]] <- cbind(l[[i]],zeros[rep(1,nrow(l[[i]])),miss,drop=FALSE])
    }
  }
  
  l <- lapply(l,function(x) {
    x[,colnames(l[[1]]), drop=FALSE]
  })
  
  evgroup <- idata[,evgroup]
  uevgroup <- sort(unique(evgroup))
  evgroup <- match(evgroup, uevgroup)
  
  if(length(l) != length(uevgroup)) {
    stop("For this idata set, please provide exactly ", 
         length(uevgroup), 
         " event objects.",call.=FALSE)
  }
  
  x <- do.call(rbind,l[evgroup]) 
  dimnames(x) <- list(NULL, colnames(x))
  x <- as.data.frame(x)
  
  n <- (sapply(l,nrow))[evgroup]
  ID <- rep(idata[["ID"]], times = n)
  x[["ID"]] <- ID
  
  if(join) {
    nu <- numeric_columns(idata)
    x <- left_join(x,idata[,nu,drop=FALSE],by="ID") 
  }
  return(x)
}

#' @param ... used to pass arguments from `assign_ev()`.
#' to `ev_assign()`.
#' @rdname ev_assign
#' @md
#' @export
assign_ev <- function(...) ev_assign(...)

#' Schedule dosing events on days of the week
#' 
#' This function lets you schedule doses on specific 
#' days of the week, allowing you to create dosing 
#' regimens on Monday/Wednesday/Friday, or Tuesday/Thursday,
#' or every other day (however you want to define that) etc.
#' 
#' @param ev an event object.
#' @param days comma- or space-separated character string of valid days of the
#' the week (see details).
#' @param addl additional doses to administer.
#' @param ii inter-dose interval; intended use is to keep this at the 
#' default value.
#' @param unit time unit; the function can only currently handle hours or days.
#' @param ... event objects named by one of the valid days of the week 
#' (see **Details**).
#' 
#' @details
#' Valid names of the week are: 
#' - `m` for Monday
#' - `t` for Tuesday
#' - `w` for Wednesday
#' - `th` for Thursday
#' - `f` for Friday
#' - `sa` for Saturday
#' - `s` for Sunday
#' 
#' 
#' The whole purpose of this function is to schedule doses on specific
#' days of the week, in a repeating weekly schedule.  Please do use caution 
#' when changing `ii` from its default value.
#' 
#' @examples
#' 
#' # Monday, Wednesday, Friday x 4 weeks
#' e1 <- ev(amt = 100)
#' ev_days(e1, days="m,w,f", addl = 3)
#' 
#' # 50 mg Tuesdays, 100 mg Thursdays x 6 months
#' e2 <- ev(amt = 50)
#' ev_days(t = e2, th = e1, addl = 23)
#' 
#' @md
#' @export
ev_days <- function(ev=NULL,days="",addl=0,ii=168,unit=c("hours", "days"),...) {
  
  unit <- match.arg(unit)
  
  max.time <- 24
  start <- c(m=0,t=24,w=48,th=72,f=96,sa=120,s=144)
  
  if(unit=="days") {
    max.time <- 1
    start <- c(m=0,t=1,w=2,th=3,f=4,sa=5,s=6)
    if(missing(ii)) ii <- 7
  }
  if(!is.null(ev)) {
    if(missing(days)) {
      stop("days argument must be supplied with ev argument.",
           call.=FALSE) 
    }
    days <- cvec_cs(days)
    if(!all(days %in% names(start))) {
      valid <- paste(names(start),collapse=",")
      err <- paste0("invalid day; valid days are: ", valid)
      stop(err,call.=FALSE)
    }
    evs <- lapply(days,function(i) {
      return(ev)
    })
    names(evs) <- days
  } else {
    args <- list(...)
    evs <- args[names(args) %in% names(start)]
    days <- names(evs)
  }
  if(length(evs)==0) {
    stop("no events were found.", call.=FALSE) 
  }
  evs <- lapply(evs,as.data.frame)
  for(d in days) {
    if(any(evs[[d]]$time > max.time)) {
      warning("not expecting time values greater than 24 hours or 1 day.",
              call.=FALSE)  
    }
    evs[[d]]$time <- evs[[d]]$time + start[d] 
  }
  evs <- bind_rows(evs)
  evs$ii <- ii
  if(addl > 0) evs$addl <- addl
  if("ID" %in% names(evs)) {
    return(as.data.frame(arrange__(evs,.dots = c("ID","time"))))
  } else {
    return(as.data.frame(arrange__(evs,.dots = c("time"))))
  }
}


#' Insert observations into a data set
#' 
#' @param data a data set or event object.
#' @param times a vector of observation times.
#' @param unique `logical`; if `TRUE` then values for `time` are 
#' dropped if they are found anywhere in `data`.
#' @param obs_pos determines sorting order for observations; use `-1` (default)
#' to put observations first; otherwise, use large integer to ensure 
#' observations are placed after doses.
#'
#' @details
#' Non-numeric columns will be dropped with a warning.
#' 
#' @return 
#' A data frame with additional rows for added observation records.
#' 
#' @examples
#' data <- expand.ev(amt = c(100, 200, 300))
#' 
#' expand_observations(data, times = seq(0, 48, 2))
#' 
#' @md
#' @export
expand_observations <- function(data, times, unique = FALSE, obs_pos = -1L) {
  
  data <- As_data_set(data)
  if(unique) {
    tcol <- timename(data)
    times <- times[!times %in% unique(data[[tcol]])]
  }
  dont_copy <- c("ID", "amt", "cmt", "evid", "ii", "ss", "rate","time","addl","..zeros..")
  dont_copy <- unique(c(dont_copy,toupper(dont_copy)))
  dat <- data.matrix(numerics_only(data))
  dat <- cbind(dat, matrix(0,
                           ncol=1,
                           nrow=nrow(dat),
                           dimnames=list(NULL,"..zeros..")))
  copy <- which(!is.element(colnames(dat),dont_copy))-1
  a <- EXPAND_OBSERVATIONS(dat,times,copy,as.integer(obs_pos))
  ans <- as.data.frame(a$data)
  names(ans) <- colnames(dat)
  ans[["..zeros.."]] <- NULL
  ans
}
