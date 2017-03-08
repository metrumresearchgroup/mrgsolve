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
##' @param ... passed along
##' @aliases NMXML
##' @details
##' If \code{run} and \code{project} are supplied, the .xml file is assumed to be located in \code{run.xml}, in directory \code{run} off the \code{project} directory.  If \code{file} is supplied, \code{run} and \code{project} arguments are ignored.
##' @return a list with theta, omega and sigma elements, depending on what was requested
nmxml <- function(run=numeric(0), project=character(0),
                  file=character(0),
                  theta=TRUE,omega=FALSE,sigma=FALSE,
                  olabels = NULL,slabels=NULL,
                  oprefix = "", sprefix="",
                  tname="THETA", oname="...",sname="...",...) {

    if(!requireNamespace("XML")) stop("Could not load namespace for package XML.", call.=FALSE)

    theta <- theta | !missing(tname)
    omega <- omega | !missing(oname)
    sigma <- sigma | !missing(sname)

    if(!missing(file)) {
        target <- file
    } else {
        if(missing(run) | missing(project)) stop("Both file and run/project are missing")
        target <- file.path(project, run, paste0(run, ".xml"))
    }

    tree <-XML::xmlParse(readLines(target), asText = TRUE, error = NULL)

    th <- list()
    om <- matrix(0,0,0)
    sg <- matrix(0,0,0)

    if(theta) {
        stopifnot(nchar(tname) > 0)
        th <-XML::xpathSApply(tree, "//nm:theta/nm:val", fun = XML::xmlValue)
        th <- as.list(as.numeric(th))
        names(th) <- paste0(tname, 1:length(th))
    }

    if(omega) {
        stopifnot(nchar(oname) > 0)
        om <- XML::xpathSApply(tree,'//nm:omega/nm:row/nm:col', fun= XML::xmlValue)
        om <- lower2matrix(om)
        if(is.null(olabels)) {
            olabels <- rep('.', nrow(om))
        } else {
            olabels <- paste0(oprefix,olabels)
        }

    }

    if(sigma) {
        stopifnot(nchar(sname) > 0)
        sg <- XML::xpathSApply(tree, "//nm:sigma/nm:row/nm:col", fun=XML::xmlValue)
        sg <- lower2matrix(sg)
        if(is.null(slabels)) {
            slabels <- rep('.', nrow(sg))
        } else {
            slabels <- paste0(sprefix,slabels)
        }
    }

    XML::free(tree)

    om <- create_matlist(setNames(list(om),oname), labels=list(olabels), class="omegalist")
    sg <- create_matlist(setNames(list(sg),sname), labels=list(slabels), class="sigmalist")
    ans <- list(theta=th, omega=om,sigma=sg)
    
    return(structure(ans,class="NMXMLDATA"))

}




