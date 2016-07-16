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
setMethod("knobs", c("mrgmod", "missing"),  function(x,...) {
  
  input <- list(...)
  
  if(is.element("time", names(input))) stop("time cannot be a knob", call.=FALSE)
  
  ## If `amt` is passed in, then we will create data set;
  ## if not, other dosing items are removed and we pass as idata set
  has.amt <- is.element("amt", names(input))
  if(!has.amt) input <- input[!is.element(names(input),s(ii,amt,rate,addl,ss,cmt))]
  
  p <- pars(x)
  
  keep <- is.element(names(input),c(p,s(ii,amt,rate,addl,ss,cmt)))
  
  toupdate <- input[!keep]
  
  input <- input[keep]
  
  whatkn <- names(input)
  
  moving <- whatkn[sapply(input, length)>1]
  
  if(any(is.element(s(addl,ss,cmt), moving))) 
    stop("addl, cmt, and ss can have only one level",call.=FALSE)
  
  if(length(input)==0) stop("No valid knobs found.", call.=FALSE)
  
  data <- do.call("expand.ev", input)
  
  x <- do.call("update", c(list(x),toupdate))
  
  if(has.amt) {
    x <- x %>% data_set(data)
  } else {
    x <- x %>% idata_set(data) 
  }
  
  out <-
    x %>%
    obsonly %>%
    mrgsim(carry.out="",recsort=3)
  
  request <- out@request
  outn <- out@outnames
  
  out <- out %>% as.data.frame
  
  data <- data %>% dplyr::select_(.dots=c("ID",whatkn))
  out <- dplyr::left_join(out,data, by="ID") 
  
  new("batch_mrgsims",
      data=as.data.frame(out),
      mod=x,
      batch=data,
      knobs=whatkn,
      request=request,
      moving=moving,
      outnames=outn,
      input=input)
  
})


##' @export
##' @rdname knobs
setMethod("knobs", c("mrgmod", "batch_mrgsims"), function(x,y,...) {
  input <- merge(y@input, list(...), strict=FALSE)
  do.call("knobs", c(list(x),input))
})


# old_knobs <- function(x,...,
#                       carry.out=character(0),
#                       drop=c("default", "none" ,"all"),
#                       update=list()) {
#   
#   drop <- match.arg(drop)
#   
#   input <- list(...)
#   
#   
#   keep <- is.element(names(input), c(knobable, pars(x)))
#   toupdate <- input[!keep]
#   args <- input[keep]
#   
#   input$carry.out<- carry.out
#   input$drop <- drop
#   input$update <- update
#   
#   if(length(args)==0) stop("No valid knobs were found.")
#   
#   toupdate <- merge(toupdate, update, strict=FALSE)
#   x <- do.call("update", c(x,toupdate))
#   
#   pass <- list(data=NULL,idata=NULL)
#   
#   knob.names <- names(args)
#   
#   moving <- knob.names[sapply(args, length)>1]
#   nomoving <- length(moving)==0
#   moving <- paste("", moving, sep="")
#   
#   param.args <- is.element(names(args), names(param(x)))
#   tran.args  <- is.element(names(args), tran.use)
#   param.knobs <- names(args)[param.args]
#   tran.knobs <- names(args)[tran.args]
#   other.knobs <- names(args)[!param.args & !tran.args]
#   
#   carry.out <- setdiff(carry.out, param.knobs)
#   
#   data <- param <- data.frame()
#   found.data <- sum(tran.args) > 0
#   found.param <- sum(param.args) > 0
#   
#   
#   kdata <- expand.grid(args)
#   kdata$ID <- 1:nrow(kdata)
#   
#   
#   if(found.data) {
#     if(!exists("time",kdata)) kdata$time <- 0
#     if(!exists("evid",kdata)) kdata$evid <- 1
#     data <- kdata[,unique(c("ID", "evid", "time", tran.knobs)), drop=FALSE]
#   }
#   idata <- kdata
#   if(nrow(data)==0) data <- NULL
#   
#   carry <- c( param.knobs, tran.knobs)
#   tran.drop <- c("time", "cmt", "addl", "ss", "evid")
#   if(drop == "none")   tran.drop <- c()
#   if(drop == "all")    tran.drop <- carry
#   if(drop !="all")     tran.drop <- setdiff(tran.drop,moving)
#   
#   carry <- setdiff(carry,tran.drop)
#   idata <- idata[, c("ID", intersect(names(idata),carry)), drop=FALSE]
#   
#   protect.idata <- names(idata) %in% tran.knobs
#   protect.carry <- carry %in% tran.knobs
#   
#   names(idata)[protect.idata] <- protect(names(idata)[protect.idata])
#   carry[protect.carry] <- protect(carry[protect.carry])
#   
#   out <- mrgsim(x,data=data, idata=idata, carry.out =c(carry.out,carry))
#   request <- out@request
#   
#   blah <- mrgsim(x, end=1, delta=1, verbose=FALSE)
#   out <- as.data.frame(out)
#   
#   names(out) <- c(unprotect(mapvalues(colnames(out), protect(tran.use),tran.alt,warn_missing=FALSE)))
#   
#   new("batch_mrgsims", 
#       data=out, mod=x, batch=kdata, knobs=names(args),
#       request=request,
#       moving=moving,outnames=blah@outnames,input=input)
#   
# }


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
  
  cat("Model: ", model(mod(object)),"\n")
  cat("Batch (head): \n")
  print(head(object@batch,n=3))
  
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
setMethod("plot", c("batch_mrgsims","missing"), function(x,...) {
  new_plot_knobs(x,...)
})


new_plot_knobs <- function(x,yval=variables(x),limit=9,...) {
  
  m <- moving(x)
  
  ny <- length(yval)
  y1 <- ny==1
  nm <- length(m)
  
  ## var1+var2+var3 ...
  yval <- paste(yval, collapse="+")
  ## Group by the first moving value
  grval <- moving(x)[1]
  ## The formula
  form <- paste0(yval,"~time")
  
  keep <- 1
  ## If we have two or more moving values
  if(nm >= 2) form <- paste0(form, "|", moving(x)[2])
  ## If one y output and three or more moving
  if(ny==1 & nm >=3) {
    form <- paste0(form, "*", moving(x)[3])
    if(nm > 3) keep <- 3
  }
  
  ## But if there is more than one y and more than two moving, keep only 2
  if(ny > 1 & nm >=2) keep <- 2
  
  df <- as.data.frame(x)
  grval <- factor(df[,m[1]], labels=paste0(m[1],sort(unique(df[,m[1]]))))
  if(nm > keep) {
    dr <- df[1,m[(keep+1):nm]]
    df <- dplyr::inner_join(df,dr, by=names(dr))
  } 
  
  x@data <- as.data.frame(df)
  x@moving <- m[1:keep]
  
  ncol <- nlevels(grval)
  if(ncol > 7) ncol <- ceiling(ncol/2)
  
  plot(x,as.formula(form),...,auto.key=list(columns=ncol), groups=grval)
  
}

# 
# old_plot_knobs <- function(x,yval=variables(x),limit=9,...) {
#   
#   mov <- moving(x)
#   rename <- mov %in% tran.use
#   mov[rename] <- mapvalues(mov[rename], tran.use,tran.alt)
#   
#   data <- as.data.frame(x)
#   tcol <- intersect(c("time", "TIME"), names(data))
#   
#   
#   ny <- length(yval)
#   
#   if(ny>limit) {
#     if(missing(limit)) warning(paste0("NOTE: showing first ",
#                                       limit,
#                                       " variables.  Check limit argument."
#     ), call.=FALSE)
#     yval <- yval[1:limit]
#   }
#   
#   yval <- paste(yval, collapse="+")
#   
#   drop <- c()
#   
#   if(all(mov=="")) {
#     fmla <- as.formula(paste0(yval, "~",tcol))
#     groups <- rep(1,nrow(data))
#     mov <- character(0)
#   }
#   
#   if(length(mov)==1) {
#     fmla <- as.formula(paste0(yval, "~",tcol))
#     groups <- factor(data[,mov[1]], labels=paste(mov[1], sort(unique(data[,mov[1]]))))
#   }
#   if(length(mov)>=2) {
#     labels1 <- paste(mov[2],sort(unique(data[,mov[2]])))
#     fmla <- as.formula(paste0(yval, "~",tcol,"|factor(",mov[2],",labels=labels1)"))
#     groups  <- factor(data[,mov[1]], labels=paste(mov[1], sort(unique(data[,mov[1]]))))
#     if(length(mov) >=3) drop <- mov[3:length(mov)]
#   }
#   
#   if(length(mov) >= 3 & ny==1) {
#     labels1 <- paste(mov[2],sort(unique(data[,mov[2]])))
#     labels2 <- paste(mov[3],sort(unique(data[,mov[3]])))
#     fmla <- as.formula(paste0(yval, "~",tcol,"|factor(",mov[2],",labels=labels1)*factor(",mov[3],",labels=labels2)"))
#     groups  <- factor(data[,mov[1]], labels=paste(mov[1], sort(unique(data[,mov[1]]))))
#     if(length(mov)<=3) drop <- c()
#     if(length(mov)>=4) drop <- mov[4:length(mov)]
#   }
#   
#   if(length(drop)>=1) {
#     message("showing only smallest values for ", paste(drop, collapse=','), " in the plot")
#     data <- as.matrix(x)
#     
#     retain <- apply(data[,drop, drop=FALSE], MARGIN=2,FUN=min)
#     retain <-apply(data[,drop,drop=FALSE], MARGIN=1, function(x) all(x==retain))
#     
#     x@data <- data[retain, , drop=FALSE]
#     x@moving <- setdiff(mov,drop)
#   }
#   
#   plot(x,fmla,..., groups=groups)
#   
#   
# }

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
