## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

##' @include mrgindata.R
##' @include utils.R

tran_upper <- c("AMT", "II", "SS", "CMT", "ADDL", "RATE", "EVID","TIME")

nodataset <- matrix(0, nrow=0, ncol=8, dimnames=list(NULL,c("ID", "time", "evid", "amt", "cmt","addl", "ii", "ss")))
null_idata <- matrix(0, 
                     nrow=0, ncol=1, 
                     dimnames=list(NULL, c("ID")))

null_data <-  matrix(0,  
                     nrow=0, ncol=3, 
                     dimnames=list(NULL, c("ID", "time", "cmt")))

VERSION <- packageDescription("mrgsolve")$Version

tgrid_matrix <- function(...) {
  x <- list(...)
  x <- lapply(x,stime)
  n <- sapply(x,length)
  x <- lapply(x,function(x) {length(x) <- max(n); x})
  m <- matrix(unlist(x), ncol=length(n))
  m
}

tgrid_id <- function(col,idata) {
  if(nrow(idata)==0) return(integer(0))
  if(length(col)==0) return(integer(0))
  if(!is.element(col,colnames(idata))) return(integer(0))
  ## Converting to C indexing here
  if(is.integer(idata[,col])) return(idata[,col]-1)
  return(match(idata[,col],sort(unique(idata[,col])))-1)
}




##' Convert select upper case column names to lower case to conform to \code{mrgsolve} data expectations.
##'
##'
##' @param data an nmtran-like data frame
##'
##' @return A data.frame with renamed columns.
##'
##' @details
##' Columns that will be renamed with lower case versions: \code{AMT}, \code{II}, \code{SS}, \code{CMT}, \code{ADDL}, \code{RATE}, \code{EVID}, \code{TIME}.  If a lower case version
##' of these names exist in the data set, the column will not be renamed.
##' @export
lctran <- function(data) {
  n <- names(data)
  infrom <- is.element(n,tran_upper)
  haslower <- is.element(tolower(n),n)
  change <- infrom & !haslower
  if(sum(change) > 0) names(data)[change] <- tolower(n[change])
  data
}

match_param <- function(a,b,off=-1) {
  ans1 <- match(a,b)
  ans2 <- match(b[ans1],a)
  list(par.loc=(ans1[!is.na(ans1)] + as.integer(off)), data.loc=(ans2[!is.na(ans2)] + as.integer(off)))
}

ifmatch <- function(a,b,off=-1) {
  ans <- match(a,b)
  return(ans[!is.na(ans)] + as.integer(off))
}

fmatch <- function(x,y) {
  x <- match(x,y,0)
  sort(x,decreasing=TRUE)
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
##' \item \code{trequest} a vector of names of table data items to take in simulated ouput
##' \item \code{Trequest} same as \code{trequest}, except that when Trequest is specified, all model compartments (in \code{request}) are dropped; this is just a shorter syntax for saying request="", trequest="name1,name2"
##' \item \code{Request} a vector of compartment or table names to take in simulated output; if this is specified, \code{request}, \code{trequest}, and \code{Trequest} are ignored
##' \item \code{obsonly} omit records with \code{evid} != 0 from simulated output
##' \item \code{obsaug} logical; when \code{TRUE} and a full data set is used, the simulated output is augmented with an observation at each time in \code{\link{stime}}().  When using \code{obsaug}, a flag indicating augmented observations can be requested by including \code{a.u.g} in \code{carry.out}
##' \item \code{recsort}  Default value is 1.  Possible values are 1,2,3,4: 1 and 2 put doses in a data set after padded observations at the same time; 3 and 4 put those doses before padded observations at the same time.  2 and 4 will put doses scheduled through \code{addl} after observations at the same time; 1 and 3 put doses scheduled through \code{addl} before observations at the same time. \code{recsort} will not change the order of your input data set if both doses and observations are given.
##'
##' \item \code{filbak} For each \code{ID}, carry the first record  \code{data} backward to start of the simulation
##' \item \code{mindt} The minimum allowable difference between \code{tto} and \code{tfrom}; if \code{(tto-tfrom)/denom} < \code{mindt} where \code{denom} is \code{tfrom} if \code{tfrom} is > 0 and 1 otherwise, then \code{tto} is set to \code{tfrom}.  This adjustment is
##' usually helpful when there are infusions in the problem and the end of an infusion is very close to an observation time. When this is the
##' case, the solver may fail with the message \code{DLSODA-  TOUT(=R1) too close to T(=R2) to start integration.}  When \code{mindt==0} the adjustment is not attempted.
##'
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
##' out <- mrgsim(mod)
##' plot(out)
##'
##' out <- mrgsim(mod, end=22)
##' out
##'
##'
##' data(exTheoph)
##'
##' out <- mrgsim(mod, data=exTheoph)
##' out
##'
##' out <- mrgsim(mod, data=exTheoph, obsonly=TRUE)
##' out
##'
##' out <- mrgsim(mod, data=exTheoph, obsaug=TRUE, carry.out="a.u.g")
##' out
##'
##' out <- mrgsim(mod, req="CENT")
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
  
  if(missing(data)) data <- x@args$data; x@args$data <- NULL
  if(missing(idata)) idata <- x@args$idata; x@args$idata <- NULL
  
  args <- merge(x@args, list(...), strict=FALSE)
  
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
    data <- mrgindata(ev,x,x@verbose)
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
    ev <- mrgindata(ev,x,x@verbose)
    
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
  
  data <- mrgindata(data,x,x@verbose)
  
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
                        trequest=character(0),
                        Trequest=character(0),
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
                        ...) {
  
  verbose <- x@verbose
  
  ## ODE and init functions:
  ## This both touches the functions as well as
  ## gets the function pointers
  foo <- touch_funs(x,keep_pointers=TRUE)
  
  param <- as.numeric(param(x))
  init <-  as.numeric(init(x))
  
  if(!identical(names(param(x)),x@shlib$par))
    stop("The parameter list  has changed since the model was compiled.")
  if(!identical(names(init(x)), x@shlib$cmt))
    stop("The compartment list has changed since the model was compiled.")
  
  ## request is stored in the model object
  ## if request is (all) take all compartments
  if(x@request[1]=="(all)") {
    request <- cmt(x)
  } else {
    request <- x@request
  }
  
  ## Requesting table items
  if(missing(trequest)) trequest <- c(foo$tnames,x@capture)
  
  ## If Trequest is supplied,
  ## take only tabled items and drop compartments
  if(!missing(Trequest)) {
    request <- ""
    trequest <- Trequest
  }
  
  ## Request is an explicit listing of all
  ## items that are compartments or tabled items
  if(!missing(Request)) {
    request <- trequest <- Request
  }
  
  request <- as.cvec2(request)
  rename.request <- set_altname(request)
  request <- as.character(rename.request)
  
  rename.carry <- set_altname(as.cvec2(carry.out))
  carry.out <- as.character(rename.carry)
  
  
  trequest <- as.cvec(trequest)
  
  ## Set the seed:
  if(!is.na(seed)) set.seed(seed)
  
  ## "idata"
  if(!is.valid_idata(idata)) idata <- valid_idata(idata,verbose=verbose,...)
  idata_icdol <- idcol(idata)
  
  ## data
  if(!is.mrgindata(data)) data <- mrgindata(data,x,verbose)
  
  tcol <- timename(data)
  tcol <- ifelse(is.na(tcol), "time", tcol)
  
  # Don't take ID,time,TIME
  carry.out <- setdiff(carry.out, c("ID", "time", "TIME"))
  
  # Only take names in GLOBALS$CARRY_TRAN
  carry.tran <- intersect(carry.out,GLOBALS[["CARRY_TRAN"]])
  carry.tran <- carry.tran[!duplicated(tolower(carry.tran))]
  
  # Non-tran items to carry out from data and idata
  carry.out <- setdiff(carry.out,carry.tran)
  
  # What to carry out from data and idata
  carry.data <- intersect(carry.out,colnames(data))
  carry.idata <- intersect(carry.out, colnames(idata))
  carry.idata <- setdiff(carry.idata, carry.data)
  
  parin <- parin(x)
  parin$t2advance <- as.integer(as.logical(t2advance))
  parin$recsort <- recsort
  
  parin$obsonly <- obsonly
  parin$obsaug <- obsaug
  request <- intersect(request,cmt(x))
  parin$request <- match(request, cmt(x));
  parin$request <- as.integer(parin$request[!is.na(parin$request)]-1)
  parin$filbak <- filbak
  
  parin$carry_data <- ifmatch(carry.data,colnames(data))
  parin$carry_idata <- ifmatch(carry.idata,colnames(idata))
  
  # This has to be lower case; that's all we're looking for
  parin$carry_tran <- tolower(carry.tran)
  
  # Now, create a rename object 
  # make_altnames: from, to
  rename.carry.tran <- set_altname(make_altnames(parin[["carry_tran"]],carry.tran))
  carry.tran <- as.character(rename.carry.tran)
  
  # Only accept table names that are not compartments
  parin$table_names <-  unique(intersect(as.cvec(trequest),setdiff(foo[["tnames"]],cmt(x))))
  capture_names <- unique(intersect(setdiff(x@capture,cmt(x)),as.cvec(trequest)))
  to_capture <- c(length(x@capture),(which(x@capture %in% capture_names)-1))
  parin$mtime <- sort(unique(mtime))
  
  stime <- stime(x)
  
  if(inherits(tgrid, c("tgrid","tgrids"))) stime <- stime(tgrid)
  
  if(length(deslist) > 0) {
    parin[["tgridmatrix"]] <- do.call("tgrid_matrix",deslist)
    parin[["whichtg"]] <- tgrid_id(descol, idata)
  } else {
    parin[["tgridmatrix"]] <- tgrid_matrix(stime)
    parin[["whichtg"]] <- integer(0)
  }
  
  parin$stimes <- stime
  parin$ptimes <- stime(ptime)
  
  if(is.character(capture)) {
    capture.output(file=capture, print(c(date=list(date()), parin=parin)))
    capture.output(file=capture, append=TRUE, print(idata))
    capture.output(file=capture, append=TRUE, print(data))
    capture.output(file=capture, append=TRUE, print(carry.out))
    capture.output(file=capture, append=TRUE, print(list(to_capture,capture_names)))
    
  }
  
  out <- .Call(mrgsolve_DEVTRAN,
               parin,
               param,
               names(param(x)),
               init,
               names(init(x)),
               to_capture,
               foo[["pointers"]],
               data,idata,
               as.matrix(omat(x)),as.matrix(smat(x)))
  
  if(length(out$issues)>0) stop(render_errors(unique(out$issues)),call.=FALSE)
  
  # out$trannames always comes back lower case in a specific order
  # need to rename to get back to requested case
  # Then, rename again for user-supplied renaming
  carry.tran <- altname(rename.carry.tran,out[["trannames"]])
  
  cnames <- c("ID",
              tcol,
              altname(rename.carry,carry.tran),
              altname(rename.carry,carry.data),
              altname(rename.carry,carry.idata),
              altname(rename.request,request),
              altname(rename.request,out$outnames),
              altname(rename.request,capture_names)
  )
  
  dimnames(out$data) <- list(NULL, cnames)
  
  new("mrgsims",
      request=altname(rename.request,request),
      data=as.data.frame(out$data),
      outnames=altname(rename.request,c(out$outnames,capture_names)),
      mod=x,
      seed=as.integer(seed),
      date=date())
}

setGeneric("parin", function(x) standardGeneric("parin"))
setMethod("parin", "mrgmod", function(x) {
  list(rtol=x@rtol,atol=x@atol, hmin=as.double(x@hmin), hmax=as.double(x@hmax),ixpr=x@ixpr,
       maxsteps=as.integer(x@maxsteps),mxhnil=x@mxhnil,verbose=as.integer(x@verbose),debug=x@debug,
       digits=x@digits, tscale=x@tscale,stimes=stime(x),mindt=x@mindt, advan=x@advan)
})



##' Get inits from compiled function.
##'
##' @param x mrgmod model object
##' @param keep_pointers should function pointers be returned?
##' @export
touch_funs <- function(x,keep_pointers=TRUE) {
  
  funp <- pointers(x)
  
  tfun <- funp[["table"]]
  ifun <- funp[["main"]]
  dfun <- funp[["ode"]]
  cfun <- funp[["config"]]
  
  param <- as.numeric(param(x))
  init <- as.numeric(x@init)
  neta <- sum(nrow(omat(x)))
  neps <- sum(nrow(smat(x)))
  
  out <- .Call(mrgsolve_TOUCH_FUNS,param,init,neta,neps,x@capture,ifun, tfun, dfun)
  
  names(out$init) <- names(init)
  
  if(keep_pointers) {
    out[["pointers"]] <- funp
  }
  
  out
  
}


house <- function(...) {
  att <- readRDS(file=pfile("mrgsolve", "project", "housemodel", "RDS"))
  x <- new("packmod",
           att,
           package="mrgsolve",
           model="housemodel"
  )
  x@soloc <- dirname(sodll(x))
  x <- compiled(x,TRUE)
  x <- update(x,...,strict=FALSE)
  z <- pointers(x)
  x
}


SUPERMATRIX <- function(x,keep_names=FALSE) {
  
  x <- .Call("mrgsolve_SUPERMATRIX",x,keep_names)
  if(nrow(x) >0 & !keep_names) dimnames(x) <- list(paste0(1:nrow(x), ": "), NULL)
  x
}




