# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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

nodataset <- matrix(0, nrow=0, ncol=8, dimnames=list(NULL,c("ID", "time", "evid", "amt", "cmt","addl", "ii", "ss")))
null_idata <- matrix(0, 
                     nrow=0, ncol=1, 
                     dimnames=list(NULL, c("ID")))

null_data <-  matrix(0,  
                     nrow=0, ncol=3, 
                     dimnames=list(NULL, c("ID", "time", "cmt")))

VERSION <- packageDescription("mrgsolve")$Version

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
  if(is.integer(col)) return(col-1)
  return(match(col,sort(unique(col)))-1)
}


validate_idata <- function(idata) {
  if(is.null(idata)) return(invisible(TRUE))
  if(!(is.data.frame(idata) | is.matrix(idata)))
    stop("idata needs to be either NULL, data.frame, or matrix.")
  return(invisible(TRUE))
}


##' Simulate from a model object.
##'
##' This function sets up the simulation run from data stored in the model object as well as
##' arguments passed in.  Note that there are many non-formal arguments to this function that
##' can be used to customize the simulation run and it's output.
##'
##' @name mrgsim
##' @rdname mrgsim
##' @param x the model objects
##' @param nid integer number of individuals to simulate; only used if idata and data are missing
##' @param ... passed to \code{\link[mrgsolve]{update}}
##' @return an object of class \code{\link{mrgsims}}
##' @import methods
##' @export
##' @details
##' \itemize{
##' \item{Both \code{data} and \code{idata} will be coreced to numeric matrix}
##' \item{\code{carry.out} can be used to insert data columns into the output data set.  This is partially dependent on the
##' nature of the data brought into the problem.}
##' }
##' @param data NMTRAN-like data set
##' @param idata a matrix or data frame of model parameters, one parameter per row
##' @section Additional arguments:
##'
##' \itemize{
##' \item \code{mtime} numeric vector of times where the model is evaluated (with solver reset), but results are not included in simulated output
##' \item \code{Request} a vector of compartment or table names to take in simulated output; if this is specified, \code{request} is ignored
##' \item \code{obsonly} omit records with \code{evid} != 0 from simulated output
##' \item \code{obsaug} logical; when \code{TRUE} and a full data set is used, the simulated output is augmented with an observation at each time in \code{\link{stime}}().  When using \code{obsaug}, a flag indicating augmented observations can be requested by including \code{a.u.g} in \code{carry.out}
##' \item \code{recsort}  Default value is 1.  Possible values are 1,2,3,4: 1 and 2 put doses in a data set after padded observations at the same time; 3 and 4 put those doses before padded observations at the same time.  2 and 4 will put doses scheduled through \code{addl} after observations at the same time; 1 and 3 put doses scheduled through \code{addl} before observations at the same time. \code{recsort} will not change the order of your input data set if both doses and observations are given.
##' \item \code{filbak} For each \code{ID}, carry the first record  \code{data} backward to start of the simulation
##' \item \code{tad} logical; when \code{TRUE} a column is added to simulated output is added showing the 
##' time since the last dose.  Only data records with \code{evid == 1} will be considered doses for the 
##' purposes of \code{tad} calculation.
##' }
##' @details
##' \itemize{
##' \item When using \code{data} and \code{idata} together, an error is generated if an ID occurs in \code{data} but not \code{idata}.  Also, when looking up data in \code{idata}, ID in \code{idata} is assumed to be uniquely keyed to ID in \code{data}.  No error is generated if ID is duplicated in \code{data}; parameters will be used from the first occurrence found in \code{idata}.
##'  \item \code{carry.out}: \code{idata} is assumed to be individual-level and variables that are carried from \code{idata} are repeated throughout the invidivual's simulated data.  Variables carried from \code{data} are carried via last-observation carry forward.  \code{NA} is returned from observations that are inserted into simulated output
##' that occur prior to the first record in \code{data}.
##'
##'
##' }

##' @examples
##' ## example("mrgsim")
##'
##' mod <- mrgsolve:::house() %>%  ev(amt=1000, cmt=1)
##' out <- mod %>% mrgsim()
##' plot(out)
##'
##' out <- mod %>% mrgsim(end=22)
##' out
##'
##'
##' data(exTheoph)
##'
##' out <- mod %>% data_set(exTheoph) %>% mrgsim()
##' out
##' 
##' out <- mod %>% mrgsim(data=exTheoph)
##'
##' out <- mrgsim(mod, data=exTheoph, obsonly=TRUE)
##' out
##'
##' out <- mod %>% mrgsim(data=exTheoph, obsaug=TRUE, carry.out="a.u.g")
##' out
##'
##' out <- mod %>% mrgsim(req="CENT")
##' out
##'
##' out <- mrgsim(mod, Req="CP,RESP")
##' out
##'
##'
##'


mrgsim <-  function(x,
                    data=NULL,
                    idata=NULL,
                    nid = 1,...) {
  
  if(is.null(data)) {
    data <- x@args$data
    x@args$data <- NULL
  } else {
   data <- as.data.frame(data) 
  }
  
  if(is.null(idata)) {
    idata <- x@args$idata
    x@args$idata <- NULL
  } else {
    idata <- as.data.frame(idata) 
  }
  
  args <- merge(x@args, list(...), open=TRUE)
  
  if(length(args) > 0) {
    x <- do.call("update",c(x,args))
  }
  
  
  ## Neither data nor idata passed in, but nid > 1
  ## Build a simple idata set to use
  if(is.null(data) & is.null(idata) & nid > 1) {
    idata <- data.frame(ID=1:nid)
  }
  
  validate_idata(idata)
  
  ## If idata is still null, set it to null_idata (no rows)
  if(is.null(idata)) idata <- null_idata
  
  
  ## Extract events in the object
  ev <- as.data.frame(events(x))
  
  ## If we found events and there is an ID column
  ## create a data set
  if(nrow(ev) > 0 & is.element("ID",colnames(ev)) & is.null(data)) {
    data <- valid_data_set(ev,x,x@verbose)
  }
  
  
  ## If data set is formed, do the simulation 
  if(!is.null(data)) {
    out <- do.call("tran_mrgsim", 
                   c(list(x),list(data=data, idata=idata),args))
    return(out)
  }
  
  ## If we had the null idata set
  ## make one with one ID
  if(nrow(idata)==0) {
    idata <- data.frame(ID=1)
  }
  
  ## If there is no ID column in idata, add one
  if(!is.element("ID", colnames(idata))) {
    idata <- bind_col(idata, "ID", 1:nrow(idata))
  }
  
  
  if(nrow(ev) > 0) {
    ## If we had events but no ID 
    ## expand that data frame to the number of IDs in idata 
    ev$ID <- 1
    ev <- valid_data_set(ev,x,x@verbose)
    
    data <- .Call(mrgsolve_EXPAND_EVENTS,
                  match("ID",colnames(ev),0), 
                  numeric_data_matrix(ev), 
                  idata[,"ID"])
  } else {
    ## No data, no events:
    data <- matrix(idata[,"ID"], 
                   ncol=1, 
                   dimnames=list(NULL, c("ID")))
  }
  
  data <- valid_data_set(data,x,x@verbose)
  
  ## simulate
  out <- do.call("tran_mrgsim", 
                 c(list(x),list(data=data, idata=idata),args))
  
  return(out)
  
}


tran_mrgsim <- function(x,
                        data,
                        idata=NULL,
                        carry.out=character(0),
                        mtime=numeric(0),
                        seed=as.integer(NA),
                        Request=character(0),
                        capture=NULL,
                        obsonly=FALSE,
                        obsaug=FALSE,
                        ptime=numeric(0),
                        tgrid = numeric(0),
                        recsort=1,
                        deslist = list(),
                        descol = character(0),
                        filbak=TRUE,
                        t2advance = FALSE,
                        tad = FALSE,
                        ...) {
  
  verbose <- x@verbose
  
  ## ODE and init functions:
  ## This both touches the functions as well as
  ## gets the function pointers
  ## foo <- touch_funs(x,keep_pointers=TRUE)
  
  if(!model_loaded(x)) {
    stop("The model is not properly loaded.  Aborting simulation.",call.=FALSE) 
  }

  ## data
  if(!is.valid_data_set(data)) {
    data <- valid_data_set(data,x,verbose)
  } 
  
  ## "idata"
  if(!is.valid_idata_set(idata)) {
    idata <- valid_idata_set(idata,verbose=verbose,...)
  }
  
  #idata_idcol <- idcol(idata)
  
  tcol <- timename(data)
  tcol <- ifelse(is.na(tcol), "time", tcol)
  
  param <- as.numeric(param(x))
  init <-  as.numeric(Init(x))
  
  if(!identical(names(param(x)),x@shlib$par))
    stop("The parameter list  has changed since the model was compiled.")
  if(!identical(Cmt(x), x@shlib$cmt))
    stop("The compartment list has changed since the model was compiled.")
  
  ## request is stored in the model object
  ## if request is (all) take all compartments
  if(x@request[1]=="(all)") {
    request <- cmt(x)
  } else {
    request <- cvec_cs(x@request)
  }

  # capture items; will work on this
  capt <- x@capture
  
  # Requested
  has_Request <- !missing(Request)
  # comma-separated
  rename.Request <- set_altname(cvec_c_nws(Request))
  Request <- as.character(rename.Request)
  
  # request is only for compartments
  # restrict captures and request if Req is specified
  if(has_Request) {
     request <- intersect(Request,cmt(x))
     capt    <- intersect(Request,capt)
  } else {
    request <- intersect(request,cmt(x)) 
  }
  
  # Non-compartment names in capture
  capt <- unique(setdiff(capt,cmt(x)))
  # First spot is the number of capture.items, followed by integer positions
  # Important to use the total length of x@capture
  capt_pos <- c(length(x@capture),(match(capt,x@capture)-1))
  
  ## carry can be tran/data/idata
  # Items to carry out from the data set
  # carry.out --> comma-separated names
  rename.carry <- set_altname(cvec_c_nws(carry.out))
  carry.out <- as.character(rename.carry)

  # Don't take ID,time,TIME
  carry.out <- setdiff(carry.out, c("ID", "time", "TIME"))
  
  # Only take names in GLOBALS$CARRY_TRAN
  carry.tran <- intersect(carry.out,GLOBALS[["CARRY_TRAN"]])
  carry.tran <- carry.tran[!duplicated(tolower(carry.tran))]
  
  # Non-tran items to carry out from data and idata
  carry.out <- setdiff(carry.out,carry.tran)
  
  # What to carry out from data and idata
  carry.data  <- intersect(carry.out, colnames(data))
  carry.idata <- intersect(carry.out, colnames(idata))
 
  # Carry from data_set if name is in idata_set too
  carry.idata <- setdiff(carry.idata, carry.data)

  # Big list of stuff to pass to DEVTRAN
  parin <- parin(x)
  parin$recsort <- recsort
  parin$obsonly <- obsonly
  parin$obsaug <- obsaug
  parin$mtime <- sort(unique(mtime))
  parin$ptimes <- stime(ptime)
  parin$filbak <- filbak
  parin$tad <- tad
  
  if(any(x@capture =="tad") & tad) {
    stop("tad argument is true and 'tad' found in $CAPTURE",call.=FALSE); 
  }
  
  # already took intersect
  parin$request <- as.integer(match(request, cmt(x))-1);
  
  # What to carry
  parin$carry_data <- carry.data 
  parin$carry_idata <- carry.idata 
  
  # This has to be lower case; that's all we're looking for
  parin$carry_tran <- tolower(carry.tran)
  
  # Now, create a rename object 
  # make_altnames: from, to
  rename.carry.tran <- set_altname(make_altnames(parin[["carry_tran"]],carry.tran))
  carry.tran <- as.character(rename.carry.tran)

  # Derive stime vector either from tgrid or from the object
  if(inherits(tgrid, c("tgrid","tgrids"))) {
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
    capture.output(file=capture, append=TRUE, print(carry.out))
    capture.output(file=capture, append=TRUE, print(list(capt_pos,capt)))
  }
  
  ## Set the seed:
  if(!is.na(seed)) set.seed(seed)
  
  out <- .Call(mrgsolve_DEVTRAN,
               parin,
               param,
               names(param(x)),
               init,
               names(Init(x)),
               capt_pos,
               pointers(x),
               data,idata,
               as.matrix(omat(x)),
               as.matrix(smat(x)),
               x@envir)
  
  # out$trannames always comes back lower case in a specific order
  # need to rename to get back to requested case
  # Then, rename again for user-supplied renaming
  carry.tran <- altname(rename.carry.tran,out[["trannames"]])
  
  if(tad) tcol <- c(tcol,"tad")
  
  cnames <- c("ID",
              tcol,
              altname(rename.carry,carry.tran), ## First tran
              altname(rename.carry,carry.data), ## Then carry data 
              altname(rename.carry,carry.idata), ## Then carry idata
              altname(rename.Request,request),   ## Then compartments
              altname(rename.Request,capt) ## Then captures
  )
  
  dimnames(out[["data"]]) <- list(NULL, cnames)
  
  new("mrgsims",
      request=altname(rename.Request,request),
      data=as.data.frame(out[["data"]]),
      outnames=altname(rename.Request,capt),
      mod=x,
      seed=as.integer(seed))
}


param_as_parent <- function(x) {
  e <- as.environment(as.list(param(x)))
  parent.env(e) <- .GlobalEnv
  parent.env(x@envir) <- e
}
global_as_parent <- function(x) {
  parent.env(x@envir) <- .GlobalEnv 
}

