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


##' Get THETA, OMEGA and SIGMA from a completed NONMEM run
##'
##' @param run run number
##' @param project project directory
##' @param file the complete path to the \code{run.xml} file
##' @param theta logical; if TRUE, the \code{$THETA} vector is returned
##' @param omega logical; if TRUE, the \code{$OMEGA} matrix is returned
##' @param sigma logical; if TRUE, the \code{$SIGMA} matrix is returned
##' @param olabels labels for \code{$OMEGA}
##' @param slabels labels for \code{$SIGMA}
##' @param oprefix prefix for \code{$OMEGA} labels
##' @param sprefix prefix for \code{$SIGMA} labels
##' @param tname name for \code{$THETA}
##' @param oname name for \code{$OMEGA}
##' @param sname name for \code{$SIGMA}
##' @param ... not used
##' @aliases NMXML
##' @details
##' If \code{run} and \code{project} are supplied, the .xml file is 
##' assumed to be located in \code{run.xml}, in directory \code{run} 
##' off the \code{project} directory.  If \code{file} is supplied, 
##' \code{run} and \code{project} arguments are ignored.
##' 
##' This function requires that the \code{xml2} package 
##' be installed and loadable.  If \code{requireNamespace("xml2")}
##' fails, an error will be generated. 
##' 
##' @return A list with theta, omega and sigma elements, 
##' depending on what was requested
##' 
##' @examples
##' 
##' if(requireNamespace("xml2")) {
##'   proj <- system.file("nonmem", package = "mrgsolve")
##'   mrgsolve:::nmxml(run = 1005, project = proj)
##' }
##' 
nmxml <- function(run=numeric(0), project=character(0),
                  file=character(0),
                  theta=TRUE, omega=TRUE, sigma=TRUE,
                  olabels = NULL, slabels = NULL,
                  oprefix = "", sprefix="",
                  tname="THETA", oname="...", sname="...", ...) {
  
  if(!requireNamespace("xml2")) {
    stop("Could not load namespace for package xml2", call.=FALSE)
  }
  
  theta <- theta | !missing(tname)
  omega <- omega | !missing(oname)
  sigma <- sigma | !missing(sname)
  
  if(!missing(file)) {
    target <- file
  } else {
    if(missing(run) | missing(project)) {
      stop("Both file and run/project are missing")
    }
    target <- file.path(project, run, paste0(run, ".xml"))
  }
  
  tree <- xml2::as_list(xml2::read_xml(target))
  
  # https://github.com/r-lib/xml2/blob/master/NEWS.md#xml2-120
  if(packageVersion("xml2") >= "1.2.0") {
    tree <- tree[["output"]][["nonmem"]][["problem"]][["estimation"]]      
  } else {
    tree <- tree[["nonmem"]][["problem"]][["estimation"]]
  }
  
  th <- list()
  om <- matrix(0,0,0)
  sg <- matrix(0,0,0)
  
  if(theta) {
    stopifnot(nchar(tname) > 0)
    th <- sapply(tree$theta, "[", USE.NAMES=FALSE)
    th <- as.list(as.numeric(th))
    names(th) <- paste0(tname, seq(length(th)))
  }
  
  if(omega) {
    stopifnot(nchar(oname) > 0)
    om <- nm_xml_matrix(tree$omega)
    if(is.null(olabels)) {
      olabels <- rep('.', nrow(om))
    } else {
      olabels <- paste0(oprefix,olabels)
    }
    olabels <- list(olabels)
  } else {
    olabels <- list()
  }
  
  if(sigma) {
    stopifnot(nchar(sname) > 0)
    sg <- nm_xml_matrix(tree$sigma)
    if(is.null(slabels)) {
      slabels <- rep('.', nrow(sg))
    } else {
      slabels <- paste0(sprefix,slabels)
    }
    slabels <- list(slabels)
  } else {
    slabels <- list()
  }

  om <- create_matlist(setNames(list(om),oname), 
                       labels=olabels, 
                       class="omegalist")
  
  sg <- create_matlist(setNames(list(sg),sname), 
                       labels=slabels, 
                       class="sigmalist")
  
  ans <- list(theta=th, omega=om, sigma=sg)
  
  return(structure(ans,class="NMXMLDATA"))
  
}


nm_xml_matrix <- function(x) {
  m <- matrix(0,nrow = length(x), ncol = length(x))
  for(i in seq(x)) {
    row <- x[[i]]
    ri <- as.integer(attr(row, "rname"))
    for(j in seq(i)) {
      ci <- as.integer(attr(row[[j]], "cname"))
      m[ri,ci] <- m[ci,ri] <- as.numeric(row[[j]][[1]])
    }
  } 
  m
}

##' Extract estimates from NONMEM ext file
##' 
##' 
##' @param run a run number or run identifier
##' @param project the NONMEM project directory
##' @param file the ext file name
##' 
##' @return A list with theta, omega, and sigma in a format
##' ready to be used to update a model object.
##' 
##' @examples
##' project <- system.file("nonmem", package = "mrgsolve")
##' 
##' est <- read_nmext(1005, project = project)
##' 
##' est$theta
##' 
##' est$omega
##' 
##' est$sigma
##' 
##' @export 
read_nmext <- function(run, file = paste0(run, ".ext"), project = getwd()) {
  file <- file.path(project, run, file)
  if(!file.exists(file)) {
    stop("The file ", file, " does not exist.", call. = FALSE)
  }
  df <- read.table(file, skip = 1, header = TRUE)
  ans <- df[df[["ITERATION"]] == -1E9,]
  if(nrow(ans) != 1) {
    stop("Could not find estimates in the file: ", basename(file), 
         call. = FALSE)
  }
  ans <- as.list(ans)
  names(ans) <- gsub("[[:punct:]]", "", names(ans))
  ans <- list(
    theta = ans[grepl("THETA", names(ans))],
    omega = as_bmat(ans, "OMEGA"), 
    sigma = as_bmat(ans, "SIGMA"),
    raw = ans  
  )
  return(ans)
}

