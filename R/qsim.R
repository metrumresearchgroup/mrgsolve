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

##' A quick simulation function.
##' 
##' @param x model object
##' @param e event object
##' @param idata individual data set
##' @param req compartments to request
##' @param tgrid \code{tgrid} object; used if \code{e} is an \code{ev} object
##' 
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
qsim <- function(x,e,idata,req=NULL,tgrid=NULL) {
  
  if(missing(idata)) {
    idata <- matrix(1,dimnames=list(NULL,"ID"))
  }
  
  if(is.ev(e)) {
    if(!is.null(tgrid)) {
      e <- recmatrix(e,tgrid) 
    } else {
      e <- recmatrix(e,stime(x))
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
  
  out <- .Call(`_mrgsolve_QUICKSIM`, 
               PACKAGE = 'mrgsolve',
               parin(x),
               as.numeric(param(x)),
               as.numeric(init(x)),
               pars(x),
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


# qsim_data <- function(x,data,req=NULL,stime=NULL) {
# 
#   cm <- reqn <-  cmt(x)
#   if(is.null(req)) {
#     req <- seq_along(reqn)
#   } else {
#     req <- match(intersect(req,cm),cm)
#     reqn <- cm[req]
#   }
# 
#   cap <- c(length(x@capture),seq_along(x@capture)-1)
# 
#   NN <- sum(data[,3] %in% c(0,2))
# 
#   out <- .Call(`_mrgsolve_QUICKSIM_DATA`,
#                PACKAGE = 'mrgsolve',
#                parin(x),
#                as.numeric(param(x)),
#                as.numeric(init(x)),
#                pars(x),
#                NN,
#                data.matrix(data),
#                as.integer(req-1),
#                cap,
#                pointers(x),
#                as.integer(c(sum(nrow(omat(x))),
#                             sum(nrow(smat(x)))))
#   )
# 
#   dimnames(out) <- list(NULL, c("ID","time", reqn,x@capture))
# 
#   out
# 
# }
# 

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

# get_rate_off <- function(x) {
#   dur <- x$amt/x$rate
#   y <- mutate(x,time=time+dur,rate=-1*rate)
#   y
# }

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


##' Create a matrix of events for simulation.
##' 
##' This function is for use with \code{\link{qsim}} only.
##'
##' @param x an events object
##' @param times object that can be coerced to numeric with \code{\link{stime}}
##' @param c_indexing if \code{TRUE}, compartment numbers will be decremented by 1
##' @export
##' 
recmatrix <- function(x,times,c_indexing=TRUE) {
  x <- as.data.frame(x)
  if(!has_name("rate", x)) x$rate <- 0
  if(!has_name("addl", x)) x$addl <- 0
  if(!has_name("ii", x)) x$ii <- 0
  if(!has_name("start", x)) x$start <- 0
  if(c_indexing) x[["cmt"]] <- x[["cmt"]]-1
  
  if(is.null(times)) stop("Please supply simulation times.")
  x <- lapply(split(x,1:nrow(x)),as_ev_matrix)
  x <- do.call(rbind,c(x,list(obs_matrix(stime(times)))))
  structure(x[order(x[,1],x[,4]),],n=sum(x[,"evid"]==0))
}
