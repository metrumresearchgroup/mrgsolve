# Copyright (C) 2013 - 2022  Metrum Research Group
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


##' Manipulate OMEGA matrices
##'
##' The primary function is \code{omat} that can be used to both get the 
##' \code{$OMEGA} matrices out of a model object and to update \code{$OMEGA} 
##' matrices in a model object.
##'
##' @param .x a matrix, list of matrices or \code{matlist} object
##' @param x  \code{matlist} object
##' @param labels character vector of names for \code{$OMEGA} elements; must 
##' be equal to number of rows/columns in the matrix
##' @param open passed to \code{\link{merge.list}}
##' @param make logical; if TRUE, matrix list is rendered into a single matrix
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
##' mod <- mrgsolve::house() %>% omat(mat4)
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
##' $OMEGA \@block
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

##' Manipulate SIGMA matrices
##'
##' The primary function is \code{smat} that can be used to both get the 
##' \code{$SIGMA} matrices out of a model object and to update \code{$SIGMA} 
##' matrices in a model object.
##'
##' @param .x a matrix, list of matrices or \code{matlist} object
##' @param x  \code{matlist} object
##' @param labels character vector of names for \code{$SIGMA} elements; must be
##' equal 
##' to number of rows/columns in the matrix
##' @param make logical; if TRUE, matrix list is rendered into a single matrix
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
##' mod <- mrgsolve::house() %>% smat(mat1)
##'
##' smat(mod)
##' smat(mod, make=TRUE)
##' @export
setGeneric("smat",function(.x,...) standardGeneric("smat"))

##' @export
##' @rdname sigma
setMethod("smat", "missing", function(...) {
  x <- list(...)
  if(length(x)==0) return(create_matlist(class="sigmalist"))
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

#' Zero out random effects in a model object
#' 
#' Sets all elements of the OMEGA or SIGMA matrix to zero
#' 
#' @param .x a model object
#' @param ... which matrix to zero out; pass `omega` to just zero out `omega`, 
#' `sigma` to just zero out `sigma`; passing nothing will zero out both
#' 
#' @return 
#' An updated object with elements of OMEGA and / or SIGMA set to zero..
#' 
#' @examples
#' 
#' mod <- house()
#' revar(mod)
#' mod <- zero_re(mod)
#' revar(mod)
#'
#' \dontrun{
#' mod <- modlib("popex", compile = FALSE)
#' mod <- zero_re(mod, omega)
#' revar(mod)
#' }
#' @md
#' @export
setGeneric("zero_re", function(.x, ...) standardGeneric("zero_re"))

#' @rdname zero_re
#' @export
setMethod("zero_re", "mrgmod", function(.x, ...) {
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


#' Methods for working with matrix-list objects
#'
#' @param .x a matlist object
#' @param x a matlist object
#' @param ... passed through to other methods
#'
#' @name matlist
#' @rdname matlist
NULL

#' @param detailed if `TRUE`, then a simple list of matrices is returned; 
#' otherwise, then entire `matlist` object data is returned along with the 
#' name of the `class` (e.g. either `omegalist` or `sigmalist`) as well 
#' as the `names` of the matrices
#' @md
#' @rdname matlist
#' @export
setMethod("as.list", "matlist", function(x, detailed = FALSE, ...) {
  if(isTRUE(detailed)) {
    return(
      list(
        data = unname(x@data),
        n = x@n, 
        names = names(x@data),
        labels = x@labels, 
        class = as.character(class(x))
      )
    )
  }
  ans <- x@data
  for(i in seq_along(ans)) {
    if(any(x@labels[[i]] != ".")) {
      dimnames(ans[[i]]) <- list(x@labels[[i]], x@labels[[i]])
    }
  }
  ans
})


#' @rdname matlist
#' @export
setMethod("as.matrix", "matlist", function(x, ...) {
  if(length(x@data)==0) {
    return(matrix(nrow = 0, ncol = 0))
  }
  SUPERMATRIX(x@data, ...)
})

#' @rdname matlist
#' @export
names.matlist <- function(x) {
  names(x@data)  
}

#' @rdname matlist
#' @export
length.matlist <- function(x) {
  length(x@data)  
}

#' @rdname matlist
#' @export
setMethod("labels", "matlist", function(object,...) {
  object@labels
})

#' @rdname matlist
#' @export
setMethod("dim", "matlist", function(x)  lapply(x@data, dim))

#' @rdname matlist
#' @export
setMethod("nrow", "matlist", function(x) unlist(lapply(x@data, nrow)))


#' @param object passed to showmatlist
#' @rdname matlist
#' @keywords internal
#' @export
setMethod("show", "matlist", function(object) showmatlist(object))
showmatlist <- function(x,...) {
  
  if(length(x@data)==0) {
    cat("No matrices found\n")
    return(invisible(NULL))
  }

  tot <- cumsum(vapply(x@data, ncol, 1L))
  
  out <- mapply(x@data,tot,x@labels,SIMPLIFY=FALSE, FUN=function(out,y,l) {
    index <- (y-ncol(out)+(seq(ncol(out))))
    rname <- paste0(l, ": ")
    rname[l=='.'] <- paste0(index[l=='.'], ": ")
    if(nrow(out) > 0) dimnames(out) <- list(rname, colnames(out))
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

##' Operations with matlist objects
##' 
##' @param x a matlist object
##' @param ... other matlist objects
##' @param recursive not used
##' @rdname matlist_ops
##' @export
setMethod("c", "matlist", function(x,...,recursive=FALSE) {
  what <- c(list(x), list(...))
  stopifnot(all(sapply(what, is.matlist)))
  what <- what[sapply(what, slot, name = "n") > 0]
  if(length(what)==1) return(x)
  d <- lapply(what, as.matrix)
  d <- setNames(d, sapply(what, names))
  l <- sapply(unname(what), labels)
  create_matlist(d, labels = l, class = class(x)[1])
})


#' Collapse OMEGA or SIGMA matrix lists
#' 
#' If multiple `OMEGA` (or `SIGMA`) blocks were written into the model, 
#' these can be collapsed into a single matrix. This will not change the 
#' functionality of the model, but will alter how `OMEGA` (or `SIGMA`) are 
#' updated, usually making it easier. This "collapsing" of the matrix list 
#' is irreversible. 
#' 
#' @param x a `mrgmod` object
#' @param name a new name for the collapsed matrix; note that this is the 
#' matrix name, not the labels which alias `ETA(n)` or `EPS(n)`; specifying a 
#' name will only alter how this matrix is potentially updated in the future
#' @param range numeric vector of length 2 specifying the range of matrices 
#' to collapse in case there are more than 2. The second element may be `NA` 
#' to indicate the length of the list of matrices. 
#' 
#' @examples
#' code <- '
#' $OMEGA 1 2 3
#' $OMEGA 4 5
#' $OMEGA 6 7 8 9
#' '
#' 
#' mod <- mcode("collapse-example", code, compile = FALSE)
#' revar(mod)
#' collapse_omega(mod) %>% omat()
#' collapse_omega(mod, range = c(2,3), name = "new_matrix") %>% omat()
#' collapse_omega(mod, range = c(2,NA), name = "new_matrix") %>% omat()
#' 
#' @return
#' A model object with updated `OMEGA` or `SIGMA` matrix lists.
#' 
#' @seealso [collapse_matrix()]
#' @md
#' @rdname collapse_matrices
#' @export
collapse_omega <- function(x, range = NULL, name = NULL) {
  stopifnot(is.mrgmod(x))
  x@omega <- collapse_matrix(
    omat(x), 
    range = range, 
    name = name
  )
  x
}

#' @md
#' @rdname collapse_matrices
#' @export
collapse_sigma <- function(x, range = NULL, name = NULL) {
  stopifnot(is.mrgmod(x))
  x@sigma <- collapse_matrix(
    smat(x), 
    range = range, 
    name = name
  )
  x
}


#' Collapse the matrices of a matlist object
#' 
#' This function is called by [collapse_omega()] and [collapse_sigma()] to 
#' convert multiple matrix blocks into a single matrix block. This "collapsing"
#' of the matrix list is irreversible. 
#' 
#' @inheritParams collapse_omega
#' @param x an object that inherits from `matlist`; this object is most
#' frequently extracted from a model object using [omat()] or [smat()] for 
#' `OMEGA` and `SIGMA`, respectively
#' 
#' @examples
#' omega <- omat(list(dmat(1, 2), dmat(3, 4, 5)))
#' omega
#' collapse_matrix(omega)
#' 
#' @return
#' An update `matlist` object (either `omegalist` or `sigmalist`).
#' 
#' @seealso [collapse_omega()], [collapse_sigma()], [omat()], [smat()]
#' @md
#' @export
collapse_matrix <- function(x, range = NULL, name = NULL) {
  
  if(!inherits(x, "matlist")) {
    stop("x must be a `matlist` object")  
  }
  
  if(length(x) <= 1) return(x)
  
  .class <- class(x)[1]
  
  update_name <- is.character(name) && length(name)==1
  
  if(is.null(range)) {  
    l <- list(unlist(labels(x)))
    m <- list(as.matrix(x))
    if(update_name) {
      names(m) <- name  
    }
    return(create_matlist(m, l, class = .class))
  }
  
  if(length(range) != 2) {
    stop("`range` must be length 2") 
  }
  if(!is.numeric(range)) {
    stop("`range` must be numeric type") 
  }
  if(is.na(range[2])) range[2] <- length(x)
  if(is.na(range[1])) {
    stop("`range[1]` must not be NA")  
  }
  if(range[1] <= 0) {
    stop("`range[1]` must be > 0")
  }
  if(range[2] > length(x)) {
    stop("`range[2]` must be <= length(x)")
  }
  if(range[2] <= range[1]) {
    stop("`range[2]` must be > `range[1]`")  
  }
  
  l <- labels(x)
  m <- as.list(x)
  
  start <- range[1]
  end <- range[2]
  second <- seq(start, end, by = 1)
  matc <- list(SUPERMATRIX(m[second]))
  labc <- list(unlist(l[second]))
  if(update_name) {
    names(matc) <- name  
  }
  if(start != 1) {
    first <- seq(1, start-1)
    matc <- c(m[first], matc)
    labc <- c(l[first], labc)
  }
  if(end < length(x)) {
    last <- seq(end + 1, length(x))
    matc <- c(matc, m[last])
    labc <- c(labc, l[last])
  }
  create_matlist(x = matc, labels = labc, class = .class)
}
