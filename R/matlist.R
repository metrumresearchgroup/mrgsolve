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


##' Manipulate OMEGA matrices.
##'
##' The primary function is \code{omat} that can be used to both get the \code{$OMEGA} matrices
##' out of a model object and to update \code{$OMEGA} matrices in a model object.
##'
##' @param .x a matrix, list of matrices or \code{matlist} object
##' @param x  \code{matlist} object
##' @param labels character vector of names for \code{$OMEGA} elements; must be equal 
##' to number of rows/columns in the matrix
##' @param open passed to \code{\link{merge.list}}
##' @param ... passed to other functions, including \code{\link{modMATRIX}}
##' @export
##' @name omega
##' @rdname omega
##' @aliases omat  OMEGA
##' @examples
##' ## example("omega")
##' mat1 <- matrix(1)
##' mat2 <- diag(c(1,2,3))
##' mat3 <- matrix(c(0.1, 0.002, 0.002, 0.5), 2,2)
##' mat4 <- dmat(0.1, 0.2, 0.3, 0.4)
##'
##' omat(mat1)
##' omat(mat1, mat2, mat3)
##' omat(A=mat1, B=mat2, C=mat3)
##'
##' mod <- mrgsolve:::house() %>% omat(mat4)
##'
##' omat(mod)
##' omat(mod, make=TRUE)
##'
##'
##' \dontrun{
##'
##' $OMEGA
##' 1 2 3
##'
##' $OMEGA @block
##' 1 0.1 2
##'
##' $OMEGA \@cor
##' \@ prefix ETA_
##' \@ labels CL VC KA
##' 0.1
##' 0.67 0.2
##' 0 0 0.3
##'
##' }

setGeneric("omat",function(.x,...) standardGeneric("omat"))
##' @export
##' @rdname omega
setMethod("omat", "missing", function(...) {
  x <- list(...)
  if(length(x)==0) return(create_matlist(class="omegalist"))
  omat(lapply(x,as.matrix))
})

##' @export
##' @rdname omega
setMethod("omat", "matrix", function(.x,..., labels=list()) {
  omat(c(list(.x),list(...)),labels=labels)
})

##' @export 
##' @rdname omega
setMethod("omat", "NULL", function(.x,...) {
  omat(list(),...)
})

##' @export
##' @rdname omega
setMethod("omat", "list", function(.x,...) {
  create_matlist(x=.x,class="omegalist",...)
})

##' @export
##' @rdname omega
setMethod("omat", "omegalist", function(.x,...) {return(.x)})

##' @export
##' @rdname omega
##' @param make logical; if TRUE, matrix list is rendered into a single matrix
setMethod("omat", "mrgmod", function(.x,...,make=FALSE,open=FALSE) {
  
  args <- list(...)
  if(length(args)>0) return(update(.x, omega=omat(...), open=open))
  
  if(!make) return(.x@omega)
  as.matrix(.x@omega)
})

##' @export
##' @rdname omega
setMethod("omat", "mrgsims", function(.x,make=FALSE,...) {
  if(!make) return(mod(.x)@omega)
  as.matrix(mod(.x)@omega)
})

##' Manipulate SIGMA matrices.
##'
##' The primary function is \code{smat} that can be used to both get the 
##' \code{$SIGMA} matrices out of a model object and to update \code{$SIGMA} 
##' matrices in a model object.
##'
##' @param .x a matrix, list of matrices or \code{matlist} object
##' @param x  \code{matlist} object
##' @param labels character vector of names for \code{$SIGMA} elements; must be equal 
##' to number of rows/columns in the matrix
##' @param ... passed to other functions, including \code{\link{modMATRIX}}
##' @param open passed to \code{\link{merge.list}}
##' 
##' @name sigma
##' @rdname sigma
##' 
##' @aliases smat SIGMA
##' 
##' @examples
##' ## example("sigma")
##' mat1 <- matrix(1)
##' mat2 <- diag(c(1,2))
##' mat3 <- matrix(c(0.1, 0.002, 0.002, 0.5), 2,2)
##' mat4 <- dmat(0.1, 0.2, 0.3, 0.4)
##'
##' smat(mat1)
##' smat(mat1, mat2, mat3)
##' smat(A=mat1, B=mat2, C=mat3)
##'
##' mod <- mrgsolve:::house() %>% smat(mat1)
##'
##' smat(mod)
##' smat(mod, make=TRUE)
##' @export
setGeneric("smat",function(.x,...) standardGeneric("smat"))

##' @export
##' @rdname sigma
setMethod("smat", "missing", function(...) {
  x <- list(...)
  if(length(x)==0) return(create_matlist(class="omegalist"))
  smat(lapply(x,as.matrix))
})

##' @export
##' @rdname sigma
setMethod("smat", "matrix", function(.x,...,labels=list()) {
  smat(c(list(.x),list(...)),labels=labels)
})

##' @export
##' @rdname sigma
setMethod("smat", "list", function(.x,...) {
  create_matlist(.x,class="sigmalist",...)
})

##' @export
##' @rdname sigma
setMethod("smat", "sigmalist", function(.x,...) return(.x))

##' @export
##' @rdname sigma
##' @param make logical; if TRUE, matrix list is rendered into a single matrix
setMethod("smat", "mrgmod", function(.x,...,make=FALSE,open=FALSE) {
  args <- list(...)
  if(length(args)>0) return(update(.x, sigma=smat(...), open=open))
  if(!make) return(.x@sigma)
  as.matrix(.x@sigma)
})

##' @export
##' @rdname sigma
setMethod("smat", "NULL", function(.x,...) {
  smat(list(),...)
})

##' @export
##' @rdname sigma
setMethod("smat", "mrgsims", function(.x,make=FALSE,...) {
  
  if(!make) return(mod(.x)@sigma)
  as.matrix(mod(.x)@sigma)
})

##' Methods for working with matrix-list objects.
##'
##' @param .x a matlist object
##' @param x a matlist object
##' @param ... passed along
##'
##' @export
##' @aliases zero.re drop.re
##' @name matlist
##' @rdname matlist
setGeneric("zero.re", function(.x,...) standardGeneric("zero.re"))

##' @export
##' @rdname matlist
setMethod("zero.re", "mrgmod", function(.x,...) {
  what <- as.character(eval(substitute(alist(...))))
  if(length(what)==0) what <- c("omega", "sigma")
  if(is.element("omega", what) & !is.null(nrow(omat(.x)))) {
    .x <- update(.x,omega=unname(lapply(nrow(omat(.x)),diag,x=0)))
  }
  if(is.element("sigma", what) & !is.null(nrow(smat(.x)))) {
    .x <- update(.x,sigma=unname(lapply(nrow(smat(.x)),diag,x=0)))
  }
  return(.x)
})

##' @rdname matlist
##' @export
zero_re <- function(...) zero.re(...)

##' @export
##' @rdname matlist
setGeneric("drop.re", function(.x,...) standardGeneric("drop.re"))

##' @export
##' @rdname matlist
setMethod("drop.re", "mrgmod", function(.x,...) {
  
  what <- as.character(eval(substitute(alist(...))))
  if(length(what)==0) what <- c("omega", "sigma")
  if(is.element("omega", what)) .x@omega <- new("omegalist")
  if(is.element("sigma", what)) .x@sigma <- new("sigmalist")
  
  return(.x)
})

##' @rdname matlist
##' @export
drop_re <- function(...) drop.re(...)

##' @export
##' @rdname matlist
setMethod("as.list", "matlist", function(x, ...) x@data)

##' @export
##' @rdname matlist
setMethod("as.matrix", "matlist", function(x,...) {
  SUPERMATRIX(x@data,...)
})

##' @export
##' @rdname matlist
setMethod("names", "matlist", function(x) names(x@data))

##' @export
##' @rdname matlist
setMethod("length", "matlist", function(x) length(x@data))

##' @export
##' @rdname matlist
setMethod("labels", "matlist", function(object,...) {
  object@labels
})

##' @export
##' @rdname matlist
setMethod("dim", "matlist", function(x)  lapply(x@data, dim))

##' @export
##' @rdname matlist
setMethod("nrow", "matlist", function(x) unlist(lapply(x@data, nrow)))

##' @export
##' @rdname matlist
##' @param object passed to showmatlist
setMethod("show", "matlist", function(object) showmatlist(object))
showmatlist <- function(x,...) {
  
  if(length(x@data)==0) {
    cat("No matrices found\n")
    return(invisible(NULL))
  }
  
  tot <- cumsum(unlist(lapply(x@data, ncol)))
  
  out <- mapply(x@data,tot,x@labels,SIMPLIFY=FALSE, FUN=function(out,y,l) {
    if(all(l=='.')) {
      index <- paste0((y-ncol(out)+(1:ncol(out))),": ")
    } else {
      index <- paste0(l, ": ")
    }
    if(nrow(out) > 0) dimnames(out) <- list(index,colnames(out))
    return(out)
    
  })
  print(out)
  return(invisible(NULL))
}

cumoffset <- function(x) {
  off <- sapply(as.list(x), nrow)
  if(length(off)==0) return(integer(0))
  ans <- cumsum(c(0,off[-length(off)]))
  names(ans) <- names(x)
  ans
}

setGeneric("rename",function(x,...) standardGeneric("rename"))
setMethod("rename", "matlist", function(x,names,...) {
  names(x@data) <- names
  return(x)
})

setGeneric("gettag", function(x,...) standardGeneric("gettag"))
setMethod("gettag", "matlist", function(x,...) {
  return(names(x@data))
})

##' Operations with matlist objects.
##' 
##' @param x a matlist object
##' @param ... other matlist objects
##' @param recursive not used
##' @rdname matlist_ops
##' @export
setMethod("c", "matlist", function(x,...,recursive=FALSE) {
  what <- c(list(x),list(...))
  stopifnot(all(sapply(what,is.matlist)))
  if(length(what)==1) return(x)
  d <- lapply(what,as.matrix)
  d <- setNames(d,sapply(what,names))
  l <- sapply(unname(what), labels)
  create_matlist(d,labels=l, class=class(x)[1])
})

collapse_matrix <- function(x,class) {
  l <- list(unlist(labels(x)))
  m <- list(as.matrix(x))
  create_matlist(m,l,class=class)
}

collapse_sigma <- function(x) {
  x@sigma <- collapse_matrix(smat(x),class="sigmalist")
  x
}
collapse_omega <- function(x) {
  x@omega <- collapse_matrix(omat(x),class="omegalist")
  x
}
