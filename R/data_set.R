# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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

##' Select and modify a data set for simulation
##' 
##' The input data set (\code{data_set}) is a data frame that specifies
##' observations, model events, and / or parameter values for a population
##' of individuals. 
##'
##' @param x model object
##' @param data data set
##' @param .subset an unquoted expression passed to 
##' \code{dplyr::filter}; retain only certain rows in the data set
##' @param .select passed to \code{dplyr::select}; retain only certain 
##' columns in the data set; this should be the result of a call to 
##' \code{dplyr::vars()}
##' @param object character name of an object existing in \code{$ENV} 
##' to use for the data set
##' @param need passed to \code{\link{inventory}}
##' @param ... passed along
##' 
##' @details
##' Input data sets are \code{R} data frames that can include columns 
##' with any valid name, however columns with selected names are 
##' treated specially by \code{mrgsolve} and incorporated into the 
##' simulation.
##'
##' \code{ID} specifies the subject ID and is required for every 
##' input data set.
##'
##' When columns have the same name as parameters (\code{$PARAM} in 
##' the model specification file), the values in those columns will 
##' be used to update the corresponding parameter as the simulation 
##' progresses.
##'
##' Input data set may include the following columns related to 
##' PK dosing events: \code{time}, \code{cmt}, \code{amt}, \code{rate},
##' \code{ii}, \code{addl}, \code{ss}.  Along with \code{ID}, \code{time} 
##' is a required column in the input data set unless \code{$PRED} is in 
##' use.  Upper case PK dosing column names including
##' \code{TIME}, \code{CMT}, \code{AMT}, \code{RATE}, \code{II},
##' \code{ADDL}, \code{SS} are also recognized.  However, an 
##' error will be generated if a mix of upper case and lower
##' case columns in this family are found.
##'  
##' \code{time} is the observation or event time, \code{cmt} 
##' is the compartment number (see \code{\link{init}}), \code{amt} 
##' is the dosing amount, \code{rate} is the infusion rate, 
##' \code{ii} is the dosing interval, \code{addl} specifies 
##' additional doses to administer, and \code{ss} is a flag 
##' for steady state dosing.  These column names operate 
##' similarly to other non-linear mixed effects modeling 
##' software. 
##' 
##' An error will be generated when mrgsolve detects that the data set
##' is not sorted by \code{time} within an individual.  Also, an error 
##' will be generated in case mrgsolve finds negative values for 
##' \code{time}, unless \code{$PRED} is in use.
##' 
##' Only numeric data can be brought in to the problem.  
##' Any non-numeric data columns will be dropped with warning.  
##' See \code{\link{numerics_only}}, which is used 
##' to prepare the data set. 
##' 
##' An error will be generated if any parameter columns in the 
##' input data set contain \code{NA}.  Likewise, and error will 
##' be generated if missing values are found in the following
##' columns: \code{ID}, \code{time}/\code{TIME}, \code{rate}/\code{RATE}. 
##'
##' See \code{\link{exdatasets}} for different example data sets.
##' 
##' @seealso \code{\link{idata_set}}, \code{\link{ev}}, 
##' \code{\link{valid_data_set}}, \code{\link{valid_idata_set}}
##'
##' @examples
##'
##' mod <- mrgsolve:::house()
##' 
##' data <- expand.ev(ID=1:3, amt=c(10,20))
##'
##' mod %>% data_set(data, ID > 1) %>% mrgsim
##' 
##' data(extran1)
##' head(extran1)
##' 
##' mod %>% data_set(extran1) %>% mrgsim
##' mod %>% mrgsim(data=extran1)
##' 
##' @export
setGeneric("data_set", function(x,data,...) {
  standardGeneric("data_set")
})


##' @rdname data_set
##' @export
setMethod("data_set",c("mrgmod", "data.frame"), function(x,data,.subset=TRUE,.select=TRUE,object=NULL,need=NULL,...) {
  
  if(is.character(need)) {
    suppressMessages(inventory(x,data,need))
  }
  if(!missing(.subset)) {
    data <- dplyr::filter(data,`!!`(enquo(.subset)))
  }
  if(!missing(.select)) {
    data <- dplyr::select(data,`!!!`(.select))
  }
  if(nrow(data) ==0) {
    stop("Zero rows in data after filtering.", call.=FALSE)
  }
  if(is.character(object)) {
    data <- data_hooks(data,object,x@envir,param(x),...) 
  }
  x@args[["data"]] <- data
  return(x)
})

##' @rdname data_set
##' @export
setMethod("data_set",c("mrgmod", "ANY"), function(x,data,...) {
  return(data_set(x,as.data.frame(data),...))
})

##' @rdname data_set
##' @export
setMethod("data_set", c("mrgmod", "ev"), function(x,data,...) {
  return(data_set(x,As_data_set(data),...))
})

##' @rdname data_set
##' @export
setMethod("data_set", c("mrgmod", "missing"), function(x, object,...) {
  object <- data_hooks(object=object,envir=x@envir,param=param(x),...)
  return(data_set(x,as.data.frame(object),...))
})


##' Convert select upper case column names to lower case to conform 
##' to mrgsolve data expectations
##'
##' @param data an nmtran-like data frame
##' 
##' @return A data.frame with renamed columns
##'
##' @details
##' Columns that will be renamed with lower case versions: \code{AMT}, 
##' \code{II}, \code{SS}, \code{CMT}, \code{ADDL}, \code{RATE}, \code{EVID}, 
##' \code{TIME}.  If a lower case version of these names exist in the data 
##' set, the column will not be renamed.
##' 
##' @export
lctran <- function(data) {
  n <- names(data)
  infrom <- is.element(n,tran_upper)
  haslower <- is.element(tolower(n),n)
  change <- infrom & !haslower
  if(sum(change) > 0) names(data)[change] <- tolower(n[change])
  data
}


data_hooks <- function(data,object,envir,param=list(),...) {
  param <- as.list(param)
  envir <- combine_list(as.list(param),as.list(envir))
  objects <- cvec_cs(object)
  args <- list(...)
  if(missing(data)) {
    data <- eval(tparse(objects[1]),envir=envir)
    if(is.function(data)) {
      data <- do.call(data,args,envir=as.environment(envir))
    } 
    objects <- objects[-1]
  } 
  for(f in objects) {
    args$data <- data
    data <- do.call(f,args,envir=as.environment(envir))
  }
  return(data)
}


##' Create a simulation data set from ev objects
##'
##'
##' @param x ev objects
##' @param ... more ev objects
##' 
##' @details
##' The goal is to take a series of event objects and combine them 
##' into a single data set that can be passed to \code{\link{data_set}}.  
##' Each event object is added to the data frame as an \code{ID} 
##' or set of \code{ID}s  that are distinct from the  \code{ID}s 
##' in the other event objects. Note that including \code{ID} 
##' argument to the \code{\link{ev}} call where \code{length(ID)} 
##' is greater than one will render that set of 
##' events for all of \code{ID}s that are requested.
##'
##' To get a data frame with one row (event) per \code{ID} 
##' look at \code{\link{expand.ev}}.
##' 
##' @return a data frame suitable for passing into \code{\link{data_set}}
##'
##' @examples
##'
##' as_data_set(ev(amt=c(100,200), cmt=1, ID=1:3),
##'             ev(amt=300, time=24, ID=1:2),
##'             ev(amt=1000, ii=8, addl=10, ID=1:3))
##'
##' # Instead of this, use expand.ev
##' as_data_set(ev(amt=100), ev(amt=200),ev(amt=300))
##'
##' @rdname as_data_set
##' @export
setGeneric("as_data_set", function(x,...) standardGeneric("as_data_set"))

##' @rdname as_data_set
setMethod("as_data_set","ev", function(x,...) {
  other_ev <- list(...)
  if(length(other_ev)==0) {
    return(check_ev(x)) 
  }
  do.call(collect_ev,c(list(x),other_ev))
})

##' @rdname as_data_set
setMethod("as_data_set","data.frame", function(x,...) {
  as_data_set(as.ev(x),...)
})

##' Replicate a list of events into a data set
##' 
##' @param l list of event objects
##' @param idata an idata set (one ID per row)
##' @param evgroup the character name of the column in \code{idata} 
##' that specifies event object to implement
##' @param join if \code{TRUE}, join \code{idata} to the data set 
##' before returning.
##' 
##' 
##' @examples
##' ev1 <- ev(amt=100)
##' ev2 <- ev(amt=300, rate=100, ii=12, addl=10)
##' 
##' idata <- data.frame(ID=1:10) 
##' idata$arm <- 1+(idata$ID %%2)
##' 
##' ev_assign(list(ev1,ev2), idata, "arm", join=TRUE)
##' 
##' 
##' 
##' @details
##' \code{ev_assign} connects events in a list passed in as the
##' \code{l} argument to values in the data set identified in the 
##' \code{evgroup} argument.  For making assignments, the unique 
##' values in the \code{evgroup} column are first sorted so that 
##' the first sorted unique value in \code{evgroup} is assigned 
##' to the first event in \code{l}, the second sorted value in 
##' \code{evgroup} column is assigned to the second event in 
##' \code{l}, and so on.  This is a change from previous behavior, 
##' which did not sort the unique values in \code{evgroup} prior to 
##' making the assignments. 
##' 
##' 
##' @export
ev_assign <- function(l,idata,evgroup,join=FALSE) {
  
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
    x[,colnames(l[[1]]),drop=FALSE]
  })
  
  evgroup <- idata[,evgroup]
  uevgroup <- sort(unique(evgroup))
  evgroup <- match(evgroup,uevgroup)
  
  if(length(l) != length(uevgroup)) {
    stop("For this idata set, please provide exactly ", 
         length(uevgroup), 
         " event objects.",call.=FALSE)
  }
  
  x <- do.call(rbind,l[evgroup]) 
  dimnames(x) <- list(NULL,colnames(x))
  x <- as.data.frame(x)
  
  n <- (sapply(l,nrow))[evgroup]
  ID <- rep(idata[["ID"]],times=n)
  x[["ID"]] <- ID
  
  if(join) {
    nu <- sapply(idata,is.numeric)
    x <- dplyr::left_join(x,idata[,nu,drop=FALSE],by="ID") 
  }
  
  return(x)
  
}

##' @param ... used to pass arguments from \code{assign_ev}
##' to \code{ev_assign}
##' @rdname ev_assign
##' @export
assign_ev <- function(...) ev_assign(...)

##' Schedule dosing events on days of the week
##' 
##' This function lets you schedule doses on specific 
##' days of the week, allowing you to create dosing 
##' regimens on Monday/Wednesday/Friday, or Tuesday/Thursday,
##' or every other day (however you want to define that) etc.
##' 
##' @param ev an event object
##' @param days comma- or space-separated character string of valid days of the
##' the week (see details)
##' @param addl additional doses to administer
##' @param ii inter-dose interval; intended use is to keep this at the 
##' default value
##' @param unit time unit; the function can only currently handle hours or days
##' @param ... event objects named by one the valid days of the week (see details)
##' 
##' @details
##' Valid names of the week are: 
##' 
##' \itemize{
##' \item \code{m} for Monday
##' \item \code{t} for Tuesday
##' \item \code{w} for Wednesday
##' \item \code{th} for Thursday
##' \item \code{f} for Friday
##' \item \code{sa} for Saturday
##' \item \code{s} for Sunday
##' }
##' 
##' The whole purpose of this function is to schedule doses on specific
##' days of the week, in a repeating weekly schedule.  Please do use caution 
##' when changing \code{ii} from it's default value.
##' 
##' @examples
##' 
##' # Monday, Wednesday, Friday x 4 weeks
##' ev_days(ev(amt=100), days="m,w,f", addl=3)
##' 
##' # 50 mg Tuesdays, 100 mg Thursdays x 6 months
##' ev_days(t=ev(amt=50), th=ev(amt=100), addl=23)
##' 
##' 
##' @export
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


##' Insert observations into a data set
##' 
##' @param data a data set or event object
##' @param times a vector of observation times
##' @param unique `logical`; if `TRUE` then values for `time` are 
##' dropped if they are found anywhere in `data`
##'
##' 
##' @details
##' Non-numeric columns will be dropped with a warning.
##' 
##' @return A data frame
##' 
##' @examples
##' data <- expand.ev(amt = c(100,200,300))
##' 
##' expand_observations(data, times = seq(0,48,2))
##' 
##' @export
expand_observations <- function(data, times, unique = FALSE) {
  
  data <- As_data_set(data)
  if(unique) {
    tcol <- timename(data)
    times <- times[!times %in% unique(data[[tcol]])]
  }
  dont_copy <- c("ID", "amt", "cmt", "evid", "ii", "ss", "rate","time","addl")
  dont_copy <- unique(c(dont_copy,toupper(dont_copy)))
  dat <- data.matrix(numerics_only(data))
  copy <- which(!is.element(colnames(dat),dont_copy))-1
  a <- EXPAND_OBSERVATIONS(dat,times,copy)
  ans <- as.data.frame(a$data)
  names(ans) <- colnames(dat)
  ans
}
