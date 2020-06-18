# Copyright (C) 2013 - 2020  Metrum Research Group
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


# @include mrgindata.R

tran_upper <- c("AMT", "II", "SS", "CMT", "ADDL", "RATE", "EVID","TIME")

nodataset <- matrix(
  0, nrow=0, ncol=8, 
  dimnames=list(
    NULL,
    c("ID", "time", "evid", "amt", "cmt","addl", "ii", "ss")
  )
)

null_idata <- matrix(
  0, 
  nrow=0, ncol=1, 
  dimnames=list(NULL, c("ID"))
)

no_idata_set <- function() {
  structure(null_idata,class="valid_idata_set")  
}

null_data <-  matrix(
  0,  
  nrow=0, ncol=3, 
  dimnames=list(NULL, c("ID", "time", "cmt"))
)

no_data_set <- function() {
  structure(matrix(1,dimnames=list(NULL, "ID")), class="valid_data_set")
}

tgrid_matrix <- function(x) {
  n <- length(x)
  if(n==1) return(matrix(x[[1]],ncol=1))
  x <- lapply(x,stime)
  mat <- matrix(ncol=n,nrow=max(sapply(x,length)))
  for(i in seq_along(x)) {
    mat[seq_along(x[[i]]),i] <- x[[i]]
  }
  mat
}


tgrid_id <- function(col,idata) {
  if(any(nrow(idata)==0,length(col)==0,
         !is.element(col,colnames(idata)))) {
    return(integer(0)) 
  }
  
  ## Converting to C indexing here
  col <- idata[,col]
  return(match(col,sort(unique(col)))-1)
}


validate_idata <- function(idata) {
  if(is.null(idata)) return(invisible(TRUE))
  if(!(is.data.frame(idata) | is.matrix(idata)))
    wstop("idata needs to be either NULL, data.frame, or matrix.")
  return(invisible(TRUE))
}


##' Simulate from a model object
##'
##' This function sets up the simulation run from data stored in the model
##' object as well as arguments passed in.  Use \code{\link{mrgsim_q}} instead 
##' to benchmark mrgsolve or to do repeated quick simulation for tasks like 
##' parameter optimization, sensitivity analyses, or optimal design.  See
##' \code{\link{mrgsim_variants}} for other mrgsim-like functions that have 
##' more focused inputs. \code{mrgsim_df} coerces output to data.frame 
##' prior to returning.
##'
##' @param x the model object
##' @param data NMTRAN-like data set (see \code{\link{data_set}})
##' @param idata a matrix or data frame of model parameters, 
##' one parameter per row (see \code{\link{idata_set}})
##' @param events an event object
##' @param nid integer number of individuals to simulate; only used if 
##' idata and data are missing
##' @param ... passed to \code{\link[mrgsolve]{update}} and 
##' \code{\link{do_mrgsim}}
##' 
##' @return An object of class \code{\link{mrgsims}}
##' 
##' 
##' @details
##' 
##' \itemize{
##' 
##' \item Use \code{mrgsim_df} to return a data frame rather than 
##' \code{mrgsims} object
##' 
##' \item{Both \code{data} and \code{idata} will be coerced to numeric matrix}
##' 
##' \item{\code{carry_out} can be used to insert data columns into the output 
##' data set.  This is partially dependent on the nature of the data brought 
##' into the problem}
##' 
##' \item When using \code{data} and \code{idata} together, an error is 
##' generated if an  ID occurs in \code{data} but not \code{idata}.  
##' Also, when looking up data in \code{idata}, ID in \code{idata} is 
##' assumed to be uniquely keyed to ID in \code{data}.  No error is 
##' generated if ID is duplicated in \code{data}; parameters will be used 
##' from the first occurrence found in \code{idata}
##'  
##' \item \code{carry_out}: \code{idata} is assumed to be 
##' individual-level and variables that are carried from \code{idata} 
##' are repeated throughout the individual's simulated data.  Variables 
##' carried from \code{data} are carried via last-observation carry forward.  
##' \code{NA} is returned from observations that are inserted into 
##' simulated output that occur prior to the first record in \code{data}
##' 
##' \item \code{recover}: this is similar to \code{carry_out} with respect to 
##' end result, but it uses a different process.  Columns to be recovered are 
##' cached prior to running the simulation, and then joined back on to the 
##' simulated data.  So, whereas \code{carry_out} will only accept numeric 
##' data items, \code{recover} can handle data frame columns of any type.  There
##' is a small decrease in performance with \code{recover} compared to 
##' \code{carry_out}, but it is likely that the performance difference is 
##' difficult to perceive (when the simulation runs very fast) or only a small
##' fractional increase in run time when the simulation is very large.  And any
##' performance hit is likely to be well worth it in light of the convenience 
##' gain.  Just think carefully about using this feature when every millisecond
##' counts.
##' 
##' }
##' 
##' @seealso \code{\link{mrgsim_variants}}, \code{\link{mrgsim_q}}
##' 
##' @examples
##' ## example("mrgsim")
##' 
##' e <- ev(amt = 1000)
##' 
##' mod <- mrgsolve::house() 
##' 
##' out <- mod %>% ev(e) %>% mrgsim()
##' 
##' plot(out)
##'
##' out <- mod %>% ev(e) %>% mrgsim(end=22)
##' 
##' out
##'
##' data(exTheoph)
##'
##' out <- mod %>% data_set(exTheoph) %>% mrgsim()
##' 
##' out
##' 
##' out <- mod %>% mrgsim(data=exTheoph)
##'
##' out <- mrgsim(mod, data=exTheoph, obsonly=TRUE)
##' 
##' out
##'
##' out <- mod %>% mrgsim(data=exTheoph, obsaug=TRUE, carry_out="a.u.g")
##' 
##' out
##'
##' out <- mod %>% ev(e) %>% mrgsim(outvars="CP,RESP")
##' 
##' out
##' 
##' a <- ev(amt = 1000, group = 'a')
##' b <- ev(amt = 750, group = 'b')
##' data <- as_data_set(a,b)
##' 
##' out <- mrgsim_d(mod, data, recover="group")
##' 
##' out
##' 
##' @export
mrgsim <-  function(x, data=NULL, idata=NULL, events=NULL, nid=1, ...) {
  if(!is.mrgmod(x)) mod_first()
  if(is.null(data)) {
    data <- x@args$data
  }  
  if(is.null(idata)) {
    idata <- x@args$idata
  } 
  if(is.null(events)) {
    events <- x@args$events
  }
  
  have_data <- !is.null(data)
  have_idata <- !is.null(idata)
  have_events <- !is.null(events)
  
  # clear out args and process
  x@args$idata <- NULL
  x@args$data <- NULL
  x@args$events <- NULL
  
  if(nid > 1) {
    return(mrgsim_nid(x, nid, events, ...))
  } 
  
  if(have_events & !have_data) {
    if(have_idata) {
      return(mrgsim_ei(x, events = events, idata = idata, ...)) 
    } else {
      return(mrgsim_e(x, events = events, ...)) 
    }
  }
  
  if(have_data) {
    if(is.ev(data)) {
      data <- as.data.frame(data, add_ID = 1)  
    }
    if(have_idata) {
      return(mrgsim_di(x, data = data, idata = idata, ...)) 
    } else {
      return(mrgsim_d(x, data = data, ...))
    }
  }
  
  if(have_idata) {
    return(mrgsim_i(x, idata = idata, ...))
  } else {
    return(mrgsim_0(x, ...)) 
  }
} 

##' @rdname mrgsim
##' @export
mrgsim_df <- function(...,output="df") mrgsim(...,output=output)

##' mrgsim variant functions
##' 
##' These functions are called by \code{\link{mrgsim}} and have
##' explicit input requirements written into the function name.  The motivation
##' behind these variants is to give the user a clear workflow with specific,
##' required inputs as indicated by the function name. Use 
##' \code{\link{mrgsim_q}} instead to benchmark mrgsolve or to do repeated quick
##' simulation for tasks like parameter optimization,  sensitivity analyses, 
##' or optimal design.
##' 
##' @inheritParams mrgsim
##' 
##' @details
##' 
##' \bold{Important:} all of these functions require that 
##' \code{data}, \code{idata}, and/or \code{events} be pass
##' directly to the functions.  They will not recognize these
##' inputs from a pipeline. 
##' 
##' \itemize{
##'   \item \code{mrgsim_e} simulate using an event object
##'   \item \code{mrgsim_ei} simulate using an event object and 
##'     \code{idata_set}
##'   \item \code{mrgsim_d} simulate using a \code{data_set}
##'   \item \code{mrgsim_di} simulate using a \code{data_set} and  
##'     \code{idata_set}
##'   \item \code{mrgsim_i} simulate using a \code{idata_set}
##'   \item \code{mrgsim_0} simulate using just the model
##'   \item \code{mrgsim_q} simulate from a data set with quicker 
##'   turnaround (see \code{\link{mrgsim_q}})
##' }
##' 
##' @seealso \code{\link{mrgsim}}, \code{\link{mrgsim_q}}, \code{\link{qsim}}
##' @name mrgsim_variants
##' @rdname mrgsim_variants
##' @export
mrgsim_e <- function(x, events, idata = NULL, data = NULL, ...) {
  if(!is.mrgmod(x)) mod_first()
  if(!is.ev(events)) {
    if(is.data.frame(events)) {
      events <- as.ev(events) 
      return(mrgsim_e(x=x,events=events,idata=idata,data=data,...))
    }
    if(is.valid_data_set(events)) {
      return(mrgsim_d(x=x,data=events,idata=idata,events=NULL,...)) 
    }
    wstop("invalid 'events' argument") 
  }
  events <- as.data.frame(events, add_ID = 1)
  args <- list(...)
  # x <- do.call(update, c(x,args))
  args <- combine_list(x@args,args)
  do.call(
    do_mrgsim, 
    c(list(x = x, data = events, idata = no_idata_set()), args)
  )
} 

##' @rdname mrgsim_variants
##' @export
mrgsim_d <- function(x, data, idata = NULL, events = NULL, ...) {
  if(!is.mrgmod(x)) mod_first()
  if(is.ev(data)) {
    data <- as_data_set(data)  
  }
  args <- list(...)
  args <- combine_list(x@args,args)
  do.call(
    do_mrgsim, 
    c(list(x = x, data = data, idata = no_idata_set()), args)
  )
} 

##' @rdname mrgsim_variants
##' @export
mrgsim_ei <- function(x, events, idata, data = NULL, ...) {
  if(!is.mrgmod(x)) mod_first()
  if(!is.ev(events)) {
    if(is.data.frame(events)) {
      events <- as.ev(events) 
      return(mrgsim_e(x=x,events=events,idata=idata,data=data,...))
    }
    if(is.valid_data_set(events)) {
      return(mrgsim_d(x=x,data=events,idata=idata,events=NULL,...)) 
    }
    wstop("invalid 'events' argument") 
  }
  expand <- !has_ID(events) & nrow(idata) > 0
  events <- as.data.frame(events, add_ID = 1)
  idata <- as.data.frame(idata)
  if(!has_ID(idata)) {
    idata[["ID"]] <- seq_len(nrow(idata))
  } 
  if(expand) {
    events <- expand_event_object(events,idata[["ID"]])
  }
  args <- list(...)
  args <- combine_list(x@args,args)
  do.call(
    do_mrgsim, 
    c(list(x = x, data = events, idata = idata), args)
  )
}

##' @rdname mrgsim_variants
##' @export
mrgsim_di <- function(x, data, idata, events = NULL, ...) {
  if(!is.mrgmod(x)) mod_first()
  data <- as.data.frame(data, add_ID = 1)
  idata <- as.data.frame(idata)
  if(!has_ID(idata)) {
    idata <- bind_col(idata, "ID", seq_len(nrow(idata)))
  }
  args <- list(...)
  args <- combine_list(x@args,args)
  do.call(
    do_mrgsim, 
    c(list(x = x, data = data, idata = idata), args)
  )
}

##' @rdname mrgsim_variants
##' @export
mrgsim_i <- function(x, idata, data = NULL, events = NULL, ...) {
  if(!is.mrgmod(x)) mod_first()
  idata <- as.data.frame(idata)
  if(!has_ID(idata)) {
    idata <- bind_col(idata, "ID", seq_len(nrow(idata)))
  }
  data <- matrix(idata[["ID"]], ncol = 1, dimnames = list(NULL, "ID"))
  args <- list(...)
  args <- combine_list(x@args,args)
  do.call(
    do_mrgsim, 
    c(list(x = x, data = data, idata = idata), args)
  )
}

##' @rdname mrgsim_variants
##' @export
mrgsim_0 <- function(x, idata = NULL, data = NULL, events = NULL, ...) {
  if(!is.mrgmod(x)) mod_first()
  data <- matrix(1, ncol = 1, dimnames = list(NULL, "ID"))
  args <- list(...)
  args <- combine_list(x@args,args)
  do.call(
    do_mrgsim, 
    c(list(x = x, data = data, idata = no_idata_set()), args)
  )
}

mrgsim_nid <- function(x, nid, events = ev(), ...) {
  if(!is.mrgmod(x)) mod_first()
  nid <- max(nid,1)
  idata <- data.frame(ID = seq_len(nid))
  if(has_ID(events)) {
    wstop("event object cannot contain 'ID' when using 'nid' argument")
  } 
  if(is.ev(events)) {
    return(mrgsim_ei(x, events, idata, ...) )
  }
  return(mrgsim_i(x, idata, ...))
}

##' @param carry_out numeric data items to copy into the output
##' @param carry.out soon to be deprecated; use \code{carry_out} instead
##' @param recover character column names in either \code{data} or \code{idata} 
##' to join back (recover) to simulated data; may be any class (e.g. numeric, 
##' character, factor, etc)
##' @param seed deprecated
##' @param Request compartments or captured variables to retain
##' in the simulated output; this is different than the \code{request}
##' slot in the model object, which refers only to model compartments
##' @param output  if \code{NULL} (the default) a mrgsims object is returned; 
##' otherwise, pass \code{df} to return a data.frame or \code{matrix} to 
##' return a matrix
##' @param capture character file name used for debugging (not related
##' to \code{$CAPTURE})
##' @param obsonly if \code{TRUE}, dosing records are not included
##' in the output
##' @param obsaug augment the data set with time grid observations; when 
##' \code{TRUE} and a full data set is used, the simulated output is augmented 
##' with an observation at each time in \code{\link{stime}}().  When using
##' \code{obsaug}, a flag indicating augmented observations can be requested by
##' including \code{a.u.g} in  \code{carry_out}
##' @param tgrid a tgrid object; or a numeric vector of simulation times
##' or another object with an \code{stime} method
##' @param recsort record sorting flag.  Default value is 1.  Possible values 
##' are 1,2,3,4: 1 and 2 put doses in a data set after padded observations at 
##' the same time; 3 and 4 put those doses before padded observations at the 
##' same time.  2 and 4 will put doses scheduled through \code{addl} after 
##' observations at the same time; 1 and 3 put doses scheduled through 
##' \code{addl} before observations at the same time. \code{recsort} will 
##' not change the order of your input data set if both doses and observations 
##' are given.
##' @param deslist a list of tgrid objects
##' @param descol the name of a column for assigning designs
##' @param filbak carry data items backward when the first 
##' data set row has time greater than zero
##' @param tad when \code{TRUE} a column is added to simulated 
##' output is added showing the time since the last dose.  Only data records 
##' with \code{evid == 1} will be considered doses for the purposes of 
##' \code{tad} calculation. The \code{tad} can be properly calculated with 
##' a dosing lag time in the model as long as the dosing lag time (specified 
##' in \code{$MAIN})  is always  appropriate for any subsequent doses scheduled 
##' through \code{addl}.  This will always be true if the lag time doesn't 
##' change over time.  But it might (possibly) not hold if the lag time changes
##' prior to the last dose in the \code{addl} sequence.  This known limitation
##' shouldn't affect \code{tad} calculation in most common dosing lag time
##' implementations.  
##' @param nocb if \code{TRUE}, use next observation carry backward method; 
##' otherwise, use \code{locf}.  
##' @param skip_init_calc don't use \code{$MAIN} to calculate initial conditions
##' @param ss_n maximum number of iterations for determining steady state for 
##' the PK system; a warning will be issued if steady state is not achieved 
##' within \code{ss_n} iterations when \code{ss_fixed} is \code{TRUE}
##' @param ss_fixed if \code{FALSE} (the default), then a warning will be issued
##' if the system does not reach steady state within \code{ss_n} iterations
##' given the model tolerances \code{rtol} and \code{atol}; if \code{TRUE}, 
##' the number of iterations for determining steady state are capped at 
##' \code{ss_n} and no warning will be issued if steady state 
##' has not been reached within \code{ss_n} dosing iterations.
##' To silence warnings related to steady state, 
##' set \code{ss_fixed} to \code{TRUE} and set \code{ss_n} as the maximum number 
##' of iterations to try when advancing the system for steady state 
##' determination.
##' 
##' @rdname mrgsim
##' @export
do_mrgsim <- function(x,
                      data,
                      idata = no_idata_set(),
                      carry_out = carry.out,
                      carry.out = character(0),
                      recover = character(0),
                      seed = as.integer(NA),
                      Request = character(0),
                      output = NULL,
                      capture = NULL,
                      obsonly = FALSE,
                      obsaug = FALSE,
                      tgrid = NULL,
                      recsort = 1,
                      deslist = list(),
                      descol = character(0),
                      filbak = TRUE,
                      tad = FALSE,
                      nocb = TRUE,
                      skip_init_calc = FALSE,
                      ss_n = 500,
                      ss_fixed = FALSE,
                      ...) {
  
  x <- update(x,...,strict=TRUE)
  
  verbose <- x@verbose
  
  if(length(Request) > 0) {
    x <- update_outputs(x, outputs=Request)  
  }
  
  do_recover_data <- do_recover_idata <-  FALSE
  carry.recover <- character(0)
  if(length(recover) > 0) {
    recover <- cvec_cs(recover)
    rename.recov <- .ren.create(recover)
    recover <- rename.recov$old
    if(any(rename.recov$new %in% carry_out)) {
      stop("names in 'recover' cannot also be in 'carry_out'",call.=FALSE)  
    }
    recover_data <- intersect(recover,names(data))
    do_recover_data  <- length(recover_data) > 0
    if(do_recover_data) {
      join_data <- data[,recover_data,drop=FALSE]
      join_data$.data_row. <- seq_len(nrow(data))
      data$.data_row. <- join_data$.data_row.
      carry.recover <- ".data_row."
      drop <- names(which(!is.numeric(join_data)))
      data <- data[,setdiff(names(data),drop),drop=FALSE]
    }
    recover_idata <- intersect(recover,names(idata))
    recover_idata <- setdiff(recover_idata,recover_data)
    do_recover_idata <- length(recover_idata) > 0
    if(do_recover_idata) {
      join_idata <- idata[,unique(c("ID", recover_idata)),drop=FALSE]
      drop <- names(which(!is.numeric(join_idata)))
      idata <- idata[,setdiff(names(idata),drop),drop=FALSE]
    } 
  }
  
  ## data
  if(!is.valid_data_set(data)) {
    data <- valid_data_set(data,x,verbose)
  } 
  
  ## "idata"
  if(!is.valid_idata_set(idata)) {
    idata <- valid_idata_set(idata,x,verbose=verbose)
  }
  
  tcol <- timename(data)
  tcol <- if_else(is.na(tcol), "time", tcol)
  
  param <- as.numeric(param(x))
  init <-  as.numeric(Init(x))
  
  if(!identical(Pars(x),x@shlib[["par"]])) {
    wstop("the parameter list has changed since the model was compiled.")
  }
  if(!identical(Cmt(x), x@shlib[["cmt"]])) {
    wstop("the compartment list has changed since the model was compiled.")
  }
  
  ## carry can be tran/data/idata
  # Items to carry out from the data set
  if(length(carry_out) > 0) {
    rename.carry <- .ren.create(carry_out)
    carry_out <- rename.carry$old
    
    # Don't take ID,time,TIME
    carry_out <- setdiff(carry_out, c("ID", "time", "TIME"))
    
    # Only take names in GLOBALS$CARRY_TRAN
    carry.tran <- intersect(carry_out,GLOBALS[["CARRY_TRAN"]])
    carry.tran <- carry.tran[!duplicated(tolower(carry.tran))]
    
    # Non-tran items to carry out from data and idata
    carry_out <- setdiff(carry_out,carry.tran)
    
    # What to carry out from data and idata
    carry.data  <- intersect(carry_out, colnames(data))
    carry.idata <- intersect(carry_out, colnames(idata))
    
    # Carry from data_set if name is in idata_set too
    carry.idata <- setdiff(carry.idata, carry.data)
  } else {
    rename.carry <- .ren.create(carry_out)
    carry.data <- character(0)
    carry.idata <- character(0)
    carry.tran <- character(0)
  }
  
  # Big list of stuff to pass to DEVTRAN
  parin <- parin(x)
  parin$recsort <- recsort
  parin$obsonly <- obsonly
  parin$obsaug <- obsaug
  parin$filbak <- filbak
  parin$tad <- tad
  parin$nocb <- nocb
  parin$do_init_calc <- !skip_init_calc
  parin$verbose <- verbose
  parin$ss_fixed <- ss_fixed
  parin$ss_n <- ss_n
  parin$request <- Cmti(x)-1L
  
  if(tad && any(x@capture =="tad")) {
    wstop("tad argument is true and 'tad' found in $CAPTURE") 
  }
  
  # What to carry
  carry.data <- c(carry.data,carry.recover)
  parin$carry_data <- carry.data
  parin$carry_idata <- carry.idata 
  
  # This has to be lower case; that's all we're looking for
  parin$carry_tran <- tolower(carry.tran)
  
  # Now, create a rename object 
  rename.carry.tran <- .ren.create(parin[["carry_tran"]],carry.tran)
  carry.tran <- rename.carry.tran$old
  
  # Derive stime vector either from tgrid or from the object
  if(!is.null(tgrid)) {
    stime <- stime(tgrid)
  } else {
    stime <- stime(x)  
  }
  
  # Look for a deslist; if so, use that instead
  if(length(deslist) > 0) {
    parin[["tgridmatrix"]] <- tgrid_matrix(deslist)
    parin[["whichtg"]] <- tgrid_id(descol, idata)
  } else {
    parin[["tgridmatrix"]] <- tgrid_matrix(list(stime))
    parin[["whichtg"]] <- integer(0)
  }
  
  # Dump some information out to file for debugging
  if(is.character(capture)) {
    capture.output(file=capture, print(c(date=list(date()), parin=parin)))
    capture.output(file=capture, append=TRUE, print(idata))
    capture.output(file=capture, append=TRUE, print(data))
    capture.output(file=capture, append=TRUE, print(carry_out))
  }
  
  out <- .Call(
    `_mrgsolve_DEVTRAN`,
    parin,
    param,
    names(param(x)),
    init,
    names(Init(x)),
    CAPTUREI(x),
    pointers(x),
    data,idata,
    as.matrix(omat(x)),
    as.matrix(smat(x)),
    x@envir, 
    PACKAGE = "mrgsolve"
  )
  
  
  # out$trannames always comes back lower case in a specific order
  # need to rename to get back to requested case
  # Then, rename again for user-supplied renaming
  carry.tran <- .ren.rename(rename.carry.tran,out[["trannames"]])
  
  if(tad) tcol <- c(tcol,"tad")
  
  cnames <- c(
    "ID",
    tcol,
    .ren.rename(rename.carry,carry.tran), ## First tran
    .ren.rename(rename.carry,carry.data), ## Then carry data 
    .ren.rename(rename.carry,carry.idata), ## Then carry idata
    x@cmtL, # already re-named
    x@capL # already re-named
  )
  
  dimnames(out[["data"]]) <- list(NULL, cnames)
  
  ans <- as.data.frame(out[["data"]])
  
  if(do_recover_data || do_recover_idata) {
    if(do_recover_data) {
      if(!rename.recov$identical) {
        names(join_data) <- .ren.rename(rename.recov,names(join_data))
      }
      ans <- left_join(ans,join_data,by=".data_row.",suffix=c("", ".recov"))  
      ans$.data_row. <- NULL
    }
    if(do_recover_idata) {
      if(!rename.recov$identical) {
        names(join_idata) <- .ren.rename(rename.recov,names(join_idata))
      }
      ans <- left_join(ans,join_idata,by="ID",suffix=c("", ".recov"))
    }
  }
  
  if(!is.null(output)) {
    if(output=="df") {
      return(ans)  
    }
    if(output=="matrix") {
      return(out[["data"]])  
    }
  }
  
  new(
    "mrgsims",
    request = x@cmtL,
    data=ans,
    outnames=x@capL,
    mod=x
  )
}

#' Basic, simple simulation from model object
#' 
#' This is just a lighter version of [mrgsim], with fewer options.  See `Details`.  
#' 
#' @inheritParams mrgsim
#' 
#' @param data can be either event object or data set
#' @param output output data type; the default is `mrgsims`, which returns the 
#' default output object; other options include `df` (for data.frame) or 
#' `matrix`
#' @param outvars output items to request; if missing, then only captured items 
#' will be returned in the output
#' @param Req synonym for outvars
#' 
#' @details
#' There is no pipeline interface for this function; all configuration options 
#' (see `Arguments`) must be passed as formal arguments to the function.  
#' You can't `carry_out`, `Request` specific columns, or pass items in for update.  
#' Some other limitations, but only convenience-related.  See `Arguments` for 
#' available options.  Specifically, there is no `...` argument for this function.
#' Use the [mrgsolve::update] method to update the model object. 
#' 
#' @examples
#' 
#' mod <- mrgsolve::house()
#' 
#' dose <- ev(amt = 100)
#' 
#' out <- qsim(mod,dose)
#' 
#' @seealso [mrgsim_q], [mrgsim], [mrgsim_variants]
#' 
#' @md
#' 
#' @export
qsim <- function(x,
                 data,
                 idata = no_idata_set(),
                 obsonly = FALSE,
                 tgrid = NULL,
                 recsort = 1,
                 tad = FALSE,
                 Req = NULL,
                 outvars = Req,
                 skip_init_calc = FALSE,
                 output = "mrgsims") {
  
  ## ODE and init functions:
  ## This both touches the functions as well as
  ## gets the function pointers
  if(!is.mrgmod(x)) mod_first()
  
  if(is.ev(data)) {
    data <- as.data.frame.ev(data, add_ID = 1)
  }
  
  ## data
  if(!is.valid_data_set(data)) {
    data <- valid_data_set(data,x,verbose=FALSE)
  } 
  
  ## "idata"
  if(!is.valid_idata_set(idata)) {
    idata <- valid_idata_set(idata,x,verbose=FALSE)
  }
  
  tcol <- timename(data)
  if(is.na(tcol)) tcol <- "time"
  
  # First spot is the number of capture.items, followed by integer positions
  # Important to use the total length of x@capture
  if(!is.null(outvars)) {
    x <- update_outputs(x,outvars)
  }
  
  # Big list of stuff to pass to DEVTRAN
  parin <- parin(x)
  parin$recsort <- recsort
  parin$obsonly <- obsonly
  parin$obsaug <- FALSE
  parin$filbak <- TRUE
  parin$tad <- tad
  parin$nocb <- TRUE
  parin$do_init_calc <- !skip_init_calc
  
  parin$request <- Cmti(x)-1L
  parin$carry_data <- character(0)
  parin$carry_idata <- character(0)
  parin$carry_tran <- character(0)
  
  # Derive stime vector either from tgrid or from the object
  if(!is.null(tgrid)) {
    timev <- stime(tgrid)
  } else {
    timev <- stime(x)  
  }
  
  # Look for a deslist; if so, use that instead
  parin[["tgridmatrix"]] <- matrix(timev,ncol=1)
  parin[["whichtg"]] <- integer(0)
  
  out <- .Call(
    `_mrgsolve_DEVTRAN`,
    parin,
    as.numeric(Param(x)),
    Pars(x),
    as.numeric(Init(x)),
    Cmt(x),
    CAPTUREI(x),
    pointers(x),
    data,idata,
    as.matrix(omat(x)),
    as.matrix(smat(x)),
    x@envir, 
    PACKAGE = "mrgsolve"
  )
  
  if(tad) tcol <- c(tcol,"tad")
  
  dimnames(out[["data"]]) <- list(NULL, c("ID", tcol,  x@cmtL, x@capL))
  
  if(output=="df") {
    return(as.data.frame.matrix(out[["data"]]))
  }
  
  new(
    "mrgsims",
    request=x@cmtL,
    data=as.data.frame.matrix(out[["data"]]),
    outnames=x@capL,
    mod=x
  )
}

#nocov end

