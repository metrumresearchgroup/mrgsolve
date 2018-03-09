# Copyright (C) 2013 - 2018  Metrum Research Group, LLC
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

##' A quick simulation function
##' 
##' @param x model object
##' @param e event object
##' @param idata individual data set
##' @param req compartments to request
##' @param tgrid \code{tgrid} object; used if \code{e} is an \code{ev} object
##' @param skip_init_calc not used
##' @param ... passed to \code{\link{qsim}}
##' 
##' @details
##' Use when simulating with no intervention or a simple intervention.
##' The rule of thumb to keep in mind with this function is that 
##' the timing of all events and observations is determined prior to 
##' the model run.  This has particular consequences for infusion
##' duration and lag times that might be set in \code{$MAIN}. 
##' Specifically, bioavailability is implemented for bolus but not 
##' infusion doses.  Infusion rates and durations are not modeled
##' from \code{$MAIN}. Dose lag times are not modeled
##' from \code{$MAIN}. If these features are needed or when in doubt, 
##' use \code{\link{mrgsim}}.
##' 
##' @examples 
##' 
##' mod <- mrgsolve:::house()
##' 
##' des <- tgrid(0,2400,1)
##' 
##' data <- recmatrix(ev(amt=1000, ii=24, addl=100),des)
##' 
##' out <- mod %>% qsim(data)
##' 
##' @export
##' 
qsim <- function(x,e=NULL,idata=NULL,req=NULL,tgrid=NULL,
                 skip_init_calc = FALSE) {
  
  if(is.null(idata)) {
    idata <- matrix(1,dimnames=list(NULL,"ID"))
  }
  
  if(is.null(e)) e <- ev()
  
  if(is.ev(e)) {
    if(!is.null(tgrid)) {
      e <- data_qsim(e,tgrid) 
    } else {
      e <- data_qsim(e,stime(x))
    }
  } 
  
  cm <- reqn <-  cmt(x)
  if(is.null(req)) {
    req <- seq_along(reqn)
  } else {
    req <- match(intersect(req,cm),cm)
    reqn <- cm[req]
  }
  
  cap <- c(length(x@capture),seq_along(x@capture)-1)
  
  parin <- parin(x)
  
  parin$do_init_calc <- !skip_init_calc
  
  out <- .Call(`_mrgsolve_QUICKSIM`, 
               PACKAGE = 'mrgsolve',
               parin,
               as.numeric(param(x)),
               as.numeric(init(x)),
               Pars(x),
               e,attr(e,"n"),
               data.matrix(idata),
               as.integer(req-1),
               cap,
               pointers(x),
               as.integer(c(sum(nrow(omat(x))),
                            sum(nrow(smat(x)))))
  )
  
  dimnames(out) <- list(NULL, c("ID","time", reqn,x@capture))
  
  out
  
}

##' @rdname qsim
##' @export
qsim_df <- function(...) {
  as_data_frame(qsim(...)) 
}


as_ev_matrix <- function(ev) {
  n <- ev$addl+1
  m1 <- matrix(nrow=n,ncol=5,
               dimnames=list(NULL,c("time", "cmt","evid", "amt", "rate")),
               c(ev$start + seq(0,ev$addl*ev$ii,ev$ii),
                 rep(ev$cmt,n),
                 rep(1,n),
                 rep(ev$amt,n),
                 rep(ev$rate,n)))
  
  if(ev$rate > 0) {
    m2 <- matrix(nrow=n,ncol=5,byrow=FALSE,
                 c(m1[,1] + m1[,4]/ev$rate,
                   rep(ev$cmt,n),
                   rep(9,n),
                   rep(0,n),
                   m1[,5])
    )
    m1 <- rbind(m1,m2)
  }
  m1
}

obs_matrix <- function(x,n=1) {
  if(n > 1) x <- rep(x,times=n)
  matrix(nrow=length(x),ncol=5,
         dimnames=list(NULL,c("time", "cmt","evid", "amt", "rate")),
         c(x,vector("numeric",4*length(x))))
}

id_obs_matrix <- function(obs,ids) {
  ids <- unique(ids)
  mat <- obs_matrix(obs,length(ids))
  ID <- rep(ids,each=length(obs))
  cbind(matrix(ID,nrow=length(ID),dimnames=list(NULL,"ID")),mat)
}


##' Create a matrix of events for simulation
##' 
##' This function is for use with \code{\link{qsim}} only.
##'
##' @param x an events object
##' @param times object that can be coerced to numeric with \code{\link{stime}}
##' @param c_indexing if \code{TRUE}, compartment numbers will be decremented by 1
##' @export
##' 
recmatrix <- function(x, times, c_indexing=TRUE) {
  x <- as.data.frame(x)
  if(nrow(x) > 0) {
    if(!has_name("rate", x)) x$rate <- 0
    if(!has_name("addl", x)) x$addl <- 0
    if(!has_name("ii", x)) x$ii <- 0
    if(!has_name("start", x)) x$start <- 0
    if(c_indexing) x[["cmt"]] <- x[["cmt"]]-1
  }
  if(is.null(times)) stop("Please supply simulation times.")
  x <- lapply(split(x,seq_len(nrow(x))),as_ev_matrix)
  x <- do.call(rbind,c(x,list(obs_matrix(stime(times)))))
  structure(x[order(x[,1],x[,4]),],n=sum(x[,"evid"]==0))
}

##' Create a matrix of events and observations for simulation
##' 
##' This function is to be used with \code{\link{qsim}} only.
##' 
##' @param e an event object
##' @param times numeric vector of observation times or a 
##' \code{tgrid} object
##' 
##' @return A numeric matrix with at minimum columns of 
##' \code{time}, \code{cmt}, \code{evid}, \code{amt}, 
##' \code{rate}.
##' 
##' @examples
##' e <- ev(amt = 100, ii = 12, addl = 2, rate = 50)
##' 
##' times <- tgrid(end = 240, delta = 6)
##' 
##' data_qsim(e, times)
##' 
##' 
##' @export
data_qsim <- function(e, times) {
  
  times <- stime(times)
  
  d <- as.data.frame(e)
  
  if(!exists("rate",d)) {
    d$rate <- 0  
  }
  if(!exists("addl", d)) {
    d$addl <- 0  
  }
  if(!exists("ii", d)) {
    d$ii <- 0  
  }
  
  dmat <- data.matrix(d)
  
  cols <- unique(c("time", "cmt", "evid", "amt", "rate", colnames(dmat)))
  
  dmat[,"cmt"] <- dmat[,"cmt"] - 1
  
  dmat <- dmat[,cols,drop = FALSE]
  
  if(any(d$addl > 0)) {
    
    admat <- dmat[dmat[,"addl"] > 0,,drop = FALSE]
    
    reps <- unlist(
      sapply(
        admat[,"addl"],seq, simplify = FALSE, USE.NAMES = FALSE
      ), 
      use.names = FALSE
    )
    
    admat <- admat[
      rep(seq(nrow(admat)),times = admat[,"addl"]),,
      drop = FALSE]
    
    admat[,"time"] <- 
      admat[,"time"] +
      admat[,"ii"]*reps
    
    dmat <- rbind(dmat,admat)
  }
  
  nrate <- 0
  if(any(d$rate > 0)) {
    drate <- dmat[dmat[,"rate"] > 0,,drop = FALSE]
    drate[,"evid"] <- 9
    drate[,"time"] <- 
      drate[,"time"] + 
      drate[,"amt"]/drate[,"rate"]
    dmat <- rbind(dmat,drate)
    nrate <- nrow(drate)
  }
  
  dmat[,"addl"] <- dmat[,"ii"] <- 0
  
  
  
  totrows <- length(times) + nrow(d) + sum(d$addl) + nrate
  
  mat <- matrix(0, nrow = totrows, ncol = ncol(d))
  
  dimnames(mat) <- list(NULL, colnames(dmat))
  
  mat[seq(nrow(dmat)),seq(ncol(dmat))] <- dmat
  
  tindex <- seq(nrow(dmat)+1, nrow(mat))
  
  mat[tindex,"time"] <- times
  
  mat <- mat[order(mat[,"time"],mat[,"evid"]),]
  
  structure(mat, n = sum(mat[,"evid"]==0))
}
