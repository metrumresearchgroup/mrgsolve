## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.



##' @include mrgsims.R
##' @include classes.R

tran.use <- c("time", "amt", "ii", "rate","ss", "addl","cmt")
tran.alt <- c("Time", "Amt", "Ii", "Rate", "Ss", "Addl", "Cmt")
dot.tran.use <- paste(".", tran.use, sep="")
knobable <- c()
knobable <- c(knobable, tran.use)

protect <- function(x) {
  paste("KNOB<", x, ">", sep="")
}
protected <- function(x,logical = FALSE) {
  re <- "^KNOB<.*>$"
  if(!logical) return(grep(re, x, value=TRUE))
  if(logical)  return(grepl(re, x))
}
unprotect <- function(x) {
  gsub("^\\KNOB<(.+)>$", "\\1", x)
}

##' @title Run sensitivity analysis on model settings
##'
##' @description Knobs can be parameter values or PK dosing items (e.g. amt).  By design, all combinations of specified knob/values are simulated.
##'
##' @param x the model object
##' @param carry.out passed to \code{\link{mrgsim}}
##' @param drop defines which knobs to drop in the matrix of simulated data; with \code{drop} = "none", the values of all knobs appear in the simulated data matrix; with \code{drop}  = "all", no knob names appear in the simulated data matrix; when \code{drop} is "default", selected non-moving columns related to PK dosing are dropped: \code{cmt}, \code{time}, \code{addl}, \code{ii}, \code{ss}, \code{evid}.  In every case, the simulation run settings can be retreived with the \code{batch} method for the \code{batch_mrgsims} output object.
##' @param update a list of arguments that are passed to update prior to running the knobs
##' @param ... knobs: named numeric vectors that identify knob names and knob values for a
##' batch run.  See details.
##' @name knobs
##' @return An object of class \code{batch_mrgsims}.  Most methods for \code{mrgsims} objects also work on \code{batch_mrgsims} object.
##' @details
##' Valid knob names include: any parameter name (in \code{param(mod)}), time variables (\code{start}, \code{end}, \code{delta}), PK dosing items (\code{amt}, \code{ii}, \code{rate}, and others ...), and solver settings (\code{atol}, \code{hmax}, etc...).
##' @export
##' @examples
##' ## example("knobs")
##'
##' mod <- mrgsolve:::house(end=72)
##'
##' events <- ev(amt=1000, cmt=1, addl=3, ii=12)
##'
##' out <- mod %>% ev(events) %>% knobs(CL=c(1,2,3))
##' plot(out)
##'
##' out
##' moving(out)
##' batch(out)
##'
##'
##' out <- mod %>% ev(events) %>% knobs(CL=c(1,2,3), VC=c(5,20,50))
##' plot(out)
##' plot(out,CP~.)
##' plot(out, CP~time|VC, groups=CL, lty=2)
##'
##' out <- knobs(mod, amt=c(100,300,500), cmt=1,time=0)
##' plot(out)
##'
##' out <- mod %>% knobs(amt=c(100,300), CL=c(1,3),VC=c(5,20), cmt=1, time=0)
##' plot(out)
##' plot(out, CP~.)
##' plot(out, CP~time|CL*VC, groups=Amt)
##'
##' out <- knobs(mod, CL=c(1,2,3), drop="all")
##' out
##'
##' out <- knobs(mod, CL=c(1,2,3), drop="none")
##' out
setGeneric("knobs", function(x,y,...) standardGeneric("knobs"))
##' @export
##' @rdname knobs
setMethod("knobs", c("mrgmod", "missing"), function(x,...,
                      carry.out=character(0),
                      drop=c("default", "none" ,"all"),
                      update=list()) {

    drop <- match.arg(drop)

    input <- list(...)


    keep <- is.element(names(input), c(knobable, pars(x)))
    toupdate <- input[!keep]
    args <- input[keep]

    input$carry.out<- carry.out
    input$drop <- drop
    input$update <- update

  if(length(args)==0) stop("No valid knobs were found.")

  toupdate <- merge(toupdate, update, strict=FALSE)
  x <- do.call("update", c(x,toupdate))

  pass <- list(data=NULL,idata=NULL)

  knob.names <- names(args)

  moving <- knob.names[sapply(args, length)>1]
  nomoving <- length(moving)==0
  moving <- paste("", moving, sep="")

  param.args <- is.element(names(args), names(param(x)))
  tran.args  <- is.element(names(args), tran.use)
  param.knobs <- names(args)[param.args]
  tran.knobs <- names(args)[tran.args]
  other.knobs <- names(args)[!param.args & !tran.args]

  carry.out <- setdiff(carry.out, param.knobs)

  data <- param <- data.frame()
  found.data <- sum(tran.args) > 0
  found.param <- sum(param.args) > 0


  kdata <- expand.grid(args)
  kdata$ID <- 1:nrow(kdata)


  if(found.data) {
    if(!exists("time",kdata)) kdata$time <- 0
    if(!exists("evid",kdata)) kdata$evid <- 1
    data <- kdata[,unique(c("ID", "evid", "time", tran.knobs)), drop=FALSE]
  }
  idata <- kdata
  if(nrow(data)==0) data <- NULL

  carry <- c( param.knobs, tran.knobs)
  tran.drop <- c("time", "cmt", "addl", "ss", "evid")
  if(drop == "none")   tran.drop <- c()
  if(drop == "all")    tran.drop <- carry
  if(drop !="all")     tran.drop <- setdiff(tran.drop,moving)

  carry <- setdiff(carry,tran.drop)
  idata <- idata[, c("ID", intersect(names(idata),carry)), drop=FALSE]

  protect.idata <- names(idata) %in% tran.knobs
  protect.carry <- carry %in% tran.knobs

  names(idata)[protect.idata] <- protect(names(idata)[protect.idata])
  carry[protect.carry] <- protect(carry[protect.carry])

  out <- mrgsim(x,data=data, idata=idata, carry.out =c(carry.out,carry))
  request <- out@request

  blah <- mrgsim(x, end=1, delta=1, verbose=FALSE)
  out <- as.matrix(out)

  dimnames(out) <- list(NULL, unprotect(mapvalues(colnames(out), protect(tran.use),tran.alt,warn_missing=FALSE)))

  new("batch_mrgsims", data=out, mod=x, batch=kdata, knobs=names(args),
      request=request,
      moving=moving,outnames=blah@outnames,input=input)

})


##' @export
##' @rdname knobs
setMethod("knobs", c("mrgmod", "batch_mrgsims"), function(x,y,...) {
    input <- merge(y@input, list(...), strict=FALSE)
    do.call("knobs", c(list(x),input))
})



##' @export
##' @rdname knobs
##' @param row.names passed to \code{\link{as.data.frame}}
##' @param optional passed to \code{\link{as.data.frame}}
setMethod("as.data.frame","batch_mrgsims", function(x,row.names=NULL, optional=FALSE,...) {
  as.data.frame(x@data, row.names,optional,...)
})
##' @export
##' @rdname knobs
##' @param y batch_mrgsims object
setMethod("as.matrix","batch_mrgsims", function(x,y,...) {
  x@data
})

##' @export
##' @rdname knobs
setGeneric("batch", function(x,...) standardGeneric("batch"))

##' @export
##' @rdname knobs
setGeneric("moving", function(x,...) standardGeneric("moving"))

##' @export
##' @rdname knobs
setMethod("batch", "batch_mrgsims", function(x,...) {
  x@batch
})
##' @export
##' @rdname knobs
setMethod("knobs", "batch_mrgsims", function(x,...) {
  x@knobs
})
##' @export
##' @rdname knobs
setMethod("moving", "batch_mrgsims", function(x,...) {
  x@moving
})


##' @rdname knobs
##' @export
##' @param object passed to show
setMethod("show", "batch_mrgsims", function(object) {
  message("Knobs simulation run summary:")
  cat("Model: ", model(mod(object)),"\n")
  cat("Batch (head): \n")
  print(head(object@batch,n=5))

  mov <- object@moving
  if(all(mov == "")) mov <- "none"
  cat("[",mov, "]\n\n")

  cat("Head:\n")
  print(head(object@data,n=5))

  mov <- mapvalues(object@moving, tran.use, tran.alt, warn_missing=FALSE)
  if(all(mov =="")) mov <- "none"
  cat("[", mov, "]")
  return(invisible(NULL))
})

##' Plot method for mrgsims objects.
##'
##' @param x mrsims object
##' @param y a formula passed to xyplot
##' @param show.grid print grid in the plot
##' @param lwd passed to xyplot
##' @param yval variables to plot
##' @param limit maximum number of yval to plot
##' @param scales passed to xyplot
##' @param auto.key passed to xyplot
##' @param type passed to xyplot
##' @param as transformation for every yval that is plotted
##' @param ... arguments passed to xyplot
##' @export
##' @rdname plot_batch_mrgsims
setMethod("plot", c("batch_mrgsims","missing"), function(x,yval=variables(x),limit=9,...) {

  mov <- moving(x)
  rename <- mov %in% tran.use
  mov[rename] <- mapvalues(mov[rename], tran.use,tran.alt)

  data <- as.data.frame(x)

  ny <- length(yval)

  if(ny>limit) {
      if(missing(limit)) warning(paste0("NOTE: showing first ",
                                        limit,
                                        " variables.  Check limit argument."
                                        ), call.=FALSE)
      yval <- yval[1:limit]
  }

  yval <- paste(yval, collapse="+")

  drop <- c()

  if(all(mov=="")) {
      fmla <- as.formula(paste(yval, "~time", sep=""))
      groups <- rep(1,nrow(data))
      mov <- character(0)
  }

  if(length(mov)==1) {
    fmla <- as.formula(paste(yval, "~time", sep=""))
    groups <- factor(data[,mov[1]], labels=paste(mov[1], sort(unique(data[,mov[1]]))))
  }
  if(length(mov)>=2) {
    labels1 <- paste(mov[2],sort(unique(data[,mov[2]])))
    fmla <- as.formula(paste(yval, "~time|factor(",mov[2],",labels=labels1)", sep=""))
    groups  <- factor(data[,mov[1]], labels=paste(mov[1], sort(unique(data[,mov[1]]))))
    if(length(mov) >=3) drop <- mov[3:length(mov)]
  }

  if(length(mov) >= 3 & ny==1) {
    labels1 <- paste(mov[2],sort(unique(data[,mov[2]])))
    labels2 <- paste(mov[3],sort(unique(data[,mov[3]])))
    fmla <- as.formula(paste(yval, "~time|factor(",mov[2],",labels=labels1)*factor(",mov[3],",labels=labels2)", sep=""))
    groups  <- factor(data[,mov[1]], labels=paste(mov[1], sort(unique(data[,mov[1]]))))
    if(length(mov)<=3) drop <- c()
    if(length(mov)>=4) drop <- mov[4:length(mov)]
  }

  if(length(drop)>=1) {
    message("showing only smallest values for ", paste(drop, collapse=','), " in the plot")
    data <- as.matrix(x)

    retain <- apply(data[,drop, drop=FALSE], MARGIN=2,FUN=min)
    retain <-apply(data[,drop,drop=FALSE], MARGIN=1, function(x) all(x==retain))

    x@data <- data[retain, , drop=FALSE]
    x@moving <- setdiff(mov,drop)
  }

  plot(x,fmla,..., groups=groups)


})

##' @export
##' @rdname plot_batch_mrgsims
setMethod("plot", c("batch_mrgsims","formula"), function(x,y,
                                                         show.grid=TRUE,
                                                         lwd=2,
                                                         type="l",
                                                         as="raw",
                                                         auto.key=list(columns=1),
                                                         scales=list(y=list(relation='free')),
                                                         ...) {
  requireNamespace("lattice", quietly=TRUE)

  if(y[[3]] == '.') {
    yval <- all.vars(y[[2]])
    return(plot(x,yval=as.character(yval),
                show.grid=show.grid,
                lwd=lwd, type=type,
                auto.key=auto.key,as=as,
                scales=scales,...))
  }

  data <- as.data.frame(x)

  if(as=="log") {
      scales$y$log="e"
      scales$y$at=10^seq(-10,10)
  }
  if(as=="log10") {
      scales$y$log=10
      scales$y$at=10^seq(-10,10,1)
  }
  lattice::xyplot(y,data=data,
                  type=type,
                  scales=scales,
                  drop.unused.levels=TRUE,
                  lwd=lwd,auto.key=auto.key,
                  panel=function(...) {
                      if(show.grid) lattice::panel.grid(h=-1,v=-1)
                      lattice::panel.xyplot(...)
                  }, ...)

})
