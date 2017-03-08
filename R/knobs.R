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

batch <- function(x) x@batch
moving <- function(x) x@moving

##' Run sensitivity analysis on model settings.
##'
##' Knobs can be parameter values or PK dosing items (e.g. amt).  
##' By design, all combinations of specified knob/values are simulated.
##'
##' @param x the model object
##' @param y a \code{batch_mrgsims} object
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
##'
##' out <- mod %>% ev(events) %>% knobs(CL=c(1,2,3), VC=c(5,20,50))
##' plot(out)
##' plot(out,CP~.)
##' plot(out, CP~time|VC, groups=CL, lty=2)
##'
##' out <- knobs(mod, amt=c(100,300,500), cmt=1)
##' plot(out)
##'
##' out <- mod %>% knobs(amt=c(100,300), CL=c(1,3), VC=c(5,20), cmt=1)
##' plot(out)
##' plot(out, CP~.)
##'
##' out <- knobs(mod, CL=c(1,2,3))
##' out
##'
##' out <- knobs(mod, CL=c(1,2,3))
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
  
  if(!has.amt) input <- input[!is.element(names(input),c("ii","amt","rate","addl","ss","cmt"))]
  
  p <- pars(x)
  
  keep <- is.element(names(input),c(p,c("ii","amt","rate","addl","ss","cmt")))
  
  toupdate <- input[!keep]
  
  input <- input[keep]
  
  whatkn <- names(input)
  
  aremoving <- whatkn[sapply(input, length)>1]
  
  if(any(is.element(c("addl","ss","cmt"), aremoving))) 
    stop("addl, cmt, and ss can have only one level",call.=FALSE)
  
  if(any(duplicated(aremoving))) stop("Duplicate knobs were found.", call.=FALSE)
  
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
  out <- dplyr::select_(out,.dots=setdiff(names(out),whatkn))
  
  data <- data %>% dplyr::select_(.dots=c("ID",whatkn))
  out <- dplyr::left_join(out,data, by="ID") 
  
  new("batch_mrgsims",
      data=as.data.frame(out),
      mod=x,
      batch=data,
      knobs=whatkn,
      request=request,
      moving=aremoving,
      outnames=outn,
      input=input)
  
})


##' @export
##' @rdname knobs
setMethod("knobs", c("mrgmod", "batch_mrgsims"), function(x,y,...) {
  input <- merge(y@input, list(...), open=TRUE)
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
setMethod("knobs", "batch_mrgsims", function(x,...) {
  x@knobs
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
  
  mov <- object@moving
  if(all(mov =="")) mov <- "none"
  cat("[", mov, "]")
  return(invisible(NULL))
})

##' Plot method for mrgsims objects.
##'
##' @param x mrsims object
##' @param y a formula passed to xyplot
##' @param yval y varialbes to plot
##' @param show.grid print grid in the plot
##' @param lwd passed to xyplot
##' @param scales passed to xyplot
##' @param auto.key passed to xyplot
##' @param mincol minimum number of columns in key
##' @param type passed to xyplot
##' @param ... arguments passed to xyplot
##' @export
##' @rdname plot_batch_mrgsims
setMethod("plot", c("batch_mrgsims","missing"), function(x,yval=variables(x),auto.key=list(),mincol=3,...) {
  new_plot_knobs(x,yval,auto.key,mincol,...)
})


new_plot_knobs <- function(x,yval,auto.key,mincol,...) {
  
  m <- moving(x)

  ny <- length(yval)
  y1 <- ny==1
  nm <- length(m)
  
  ## var1+var2+var3 ...
  yval <- paste(yval, collapse="+")
  
  ## The formula
  form <- paste0(yval,"~time")
  
  df <- as.data.frame(x)
  keep <- 1
  ## If we have two or more moving values
  if(nm >= 2) {
    form <- paste0(form, "|", m[2])
    df[,m[2]] <- factor(df[,m[2]],labels=paste0(m[2]," ",sort(unique(df[,m[2]]))))
    keep <- 2
  }
  ## If one y output and three or more moving
  if(ny==1 & nm >=3) {
    form <- paste0(form, "*", m[3])
    df[,m[3]] <- factor(df[,m[3]],labels=paste0(m[3]," ",sort(unique(df[,m[3]]))))
    keep <- 3
  }
  
  ## But if there is more than one y and more than two moving, keep only 2
  if(ny > 1 & nm >=2) keep <- 2
  
  grval <- factor(df[,m[1]], labels=paste0(m[1]," ",sort(unique(df[,m[1]]))))
  
  if(nm > keep) {
    kp <- unique(df[,m[c(1:keep)],drop=FALSE])
    dr <- unique(df[1,m[c((keep+1):nm)],drop=FALSE])
    retain <- cbind(kp,dr)
    df <- dplyr::inner_join(df,retain, by=names(retain))
    message("Dropping knobs: ", paste(names(dr),collapse=","))
  } 
  
  x@data <- as.data.frame(df)
  
  x@moving <- m[1:keep]
  
  if(identical(auto.key,list())) {
    auto.key <- list(columns = min(nlevels(grval),mincol))
  }
  
  plot(x,as.formula(form),..., groups=grval,auto.key=auto.key)
  
}

##' @export
##' @rdname plot_batch_mrgsims
setMethod("plot", c("batch_mrgsims","formula"), function(x,y,
                                                         show.grid=TRUE,
                                                         lwd=2,
                                                         type="l",
                                                         yval=variables(x),
                                                         auto.key=list(columns=1),
                                                         scales=list(y=list(relation='free')),
                                                         ...) {
  
  requireNamespace("lattice", quietly=TRUE)
  
  if(y[[3]] == '.') {
    yval <- all.vars(y[[2]])
    return(plot(x,yval=as.character(yval),
                show.grid=show.grid,
                lwd=lwd, type=type,
                auto.key=auto.key,
                scales=scales,...))
  }
  
  data <- as.data.frame(x)
  
  lattice::xyplot(y,
                  data=data,
                  type=type,
                  scales=scales,
                  drop.unused.levels=TRUE,
                  lwd=lwd,auto.key=auto.key,
                  panel=function(...) {
                    if(show.grid) lattice::panel.grid(h=-1,v=-1)
                    lattice::panel.xyplot(...)
                  }, ...)
  
})
