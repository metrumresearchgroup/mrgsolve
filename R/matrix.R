SUPERMATRIX <- function(x,keep_names=FALSE) {
  x <- .Call("mrgsolve_SUPERMATRIX",x,keep_names)
  if(nrow(x) > 0 & !keep_names) dimnames(x) <- list(paste0(1:nrow(x), ": "), NULL)
  x
}

call_ZERO <- function(x) .Call("mrgsolve_ZERO",x, PACKAGE="mrgsolve")
ZERO_MATRIX <- function(x) diag(nrow(x))
decorr <- function(x) {
  off <- x[lower.tri(x)]
  if(any(off < -1 | off > 1)) stop("For correlation matrix, all off-diagonal elements must be in [-1,1].")
  return(invisible(.Call("mrgsolve_decorr", x)))
}




##' Create a square numeric matrix from the lower-triangular elements.
##'
##' @param x numeric data
##' @param context the working context
##' @return a square symmetric numeric matrix with column names
##'
lower2matrix <- function(x, context=NULL) {
  x <- as.numeric(x)
  if(length(x)==1) return(matrix(x,nrow=1, ncol=1 ))
  n <- 0.5*(sqrt(1-4*(-2*length(x)))-1)
  if(!n==as.integer(n)) {
    stop(paste0("Block matrix has invalid specification (", context, ")."),call.=FALSE)
  }
  mat <- diag(n)
  mat[upper.tri(mat,diag=TRUE)] <- x
  mat <- mat+t(mat) - diag(diag(mat))
  mat
}

##' Create a diagonal numeric matrix from diagonal elements.
##'
##' @param x numeric data
##' @param context used to generate column names
##' @return a numeric diagonal matrix
##'
numeric2diag <- function(x,context=NULL) {
  x <- as.numeric(x)
  if(length(x)==1) {
    mat <- matrix(x)
  } else {
    mat <- diag(x)
  }
  mat
}


##' Create a matrix.
##'
##' @param x data for building the matrix.  Data in \code{x} are assumed to be on-diagonal elements if \code{block} is \code{FALSE} and lower-triangular elements if \code{block} is \code{TRUE}
##' @param use logical; if FALSE, all matrix elements are set to 0
##' @param block logical; if TRUE, try to make a block matrix; diagonal otherwise
##' @param correlation logical; if TRUE, off diagonal elements are assumed to be correlations and converted to covariances; if correlation is TRUE, then block is set to TRUE
##' @param digits if value of this argument is greater than zero, the matrix is passed to signif (along with digits) prior to returning
##' @param context the working context
##' @param ... passed along
##'
##' @examples
##' modMATRIX("1 2.2 333")
##' modMATRIX("1 1.1 2.2", block=TRUE)
##' modMATRIX("23 234 234 5234", use=FALSE)
##'
##' ans <- modMATRIX("1.1 0.657 2.2", correlation=TRUE, block=TRUE)
##' ans
##' cov2cor(ans)
##'
##' @export
##'
##'
modMATRIX <- function(x,
                      use=TRUE,
                      block=FALSE,
                      correlation=FALSE,
                      digits=-1,
                      context="matlist",
                      ...) {
  
  if(length(x)==0 | is.null(x)) return(matrix(nrow=0,ncol=0))
  
  if(correlation) block <- TRUE
  #if(is.character(x)) x <- unlist(strsplit(x, "\\s+",perl=TRUE))
  if(is.character(x)) x <- cvec_cs(x)
  x <- x[x!=""]
  if(block) {
    x <- lower2matrix(x, context)
    if(correlation) decorr(x)
  } else {
    x <- numeric2diag(x)
  }
  if(any(is.na(x))) stop("mrgsolve: NA values generated when forming matrix ", "(", context, ").")
  if(!use) x <- ZERO(x)
  if(digits > 0) x <- signif(x, digits=digits)
  return(x)
}


Diag <- function(x) {
  if(is.matrix(x)) return(x)
  diag(x, nrow=length(x),ncol=length(x))
}

##' Create matrices from vector input.
##'
##' @param ... matrix data
##' @param correlation logical; if TRUE, off diagonal elements are assumed to be correlations and converted to covariances
##' @param digits if greater than zero, matrix is passed to signif (along with digits) prior to returning
##' @details
##' \code{bmat} makes a block matrix.  \code{cmat} makes a correlation matrix.  \code{dmat} makes a diagonal matrix.
##' 
##'
##' @export
##' @examples
##'
##' dmat(1,2,3)/10
##'
##' bmat(0.5,0.01,0.2)
##'
##' cmat(0.5, 0.87,0.2)
##'
##'
##' @seealso as_bmat
##'
bmat <- function(...,correlation=FALSE, digits=-1) {
  x <- lower2matrix(unlist(list(...)),context="bmat")
  if(correlation) decorr(x)
  if(digits>0) x <- signif(x,digits=digits)
  return(x)
}


##' @export
##' @rdname bmat
cmat <- function(...,digits=-1) {
  bmat(...,digits=digits,correlation=TRUE)
}
##' @export
##' @rdname bmat
##' @seealso as_dmat
dmat <- function(...) {
  Diag(as.numeric(unlist(list(...))))
}

##' Coerce R objects to block or diagonal matrices.
##'
##' @param x an R object
##' @param pat regular expression, character
##' @param cols column names to use instead of \code{pat}
##' @param ... passed along
##' @return A numeric matrix for list and numeric methods.  For data.frames, a list of matrices are returned.
##' @seealso bmat, dmat
##' @export
##' @examples
##'
##' df <- data.frame(OMEGA1.1 = c(1,2),
##'                  OMEGA2.1 = c(11,22),
##'                  OMEGA2.2 = c(3,4),
##'                  SIGMA1.1 = 1,
##'                  FOO=-1)
##'
##' as_bmat(df, "OMEGA")
##' as_dmat(df,"SIGMA")
##' as_dmat(df[1,],"OMEGA")
##'
##' @rdname as_bmat
##' @name as_bmat
##'

setGeneric("as_bmat", function(x,...) standardGeneric("as_bmat"))
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "list", function(x,...) {as_bmat(unlist(x),...)})
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "numeric", function(x,pat="*",...) {
  x <- grepn(x,pat, !missing(pat))
  do.call("bmat", list(x))
})
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "data.frame", function(x,pat="*",cols=NULL, ...) {
  if(is.character(cols)) {
    cols <- cvec_cs(cols)
    if(!all(cols %in% names(x))) {
      stop("Invalid colums in cols argument.") 
    }
    cols <- names(x) %in% cols
  } else {
    cols <- grepl(pat,names(x))
  }
  x <- x[,cols,drop=FALSE]
  lapply(seq_len(nrow(x)), function(i) bmat(unlist(x[i,])))
})
##' @export
##' @rdname as_bmat
setMethod("as_bmat", "ANY", function(x,...) {as_bmat(as.data.frame(x),...)})

##' @export
##' @rdname as_bmat
setGeneric("as_dmat", function(x,...) standardGeneric("as_dmat"))
##' @export
##' @rdname as_bmat
setMethod("as_dmat", "list", function(x,...) {as_dmat(unlist(x),...)})
##' @export
##' @rdname as_bmat
setMethod("as_dmat", "ANY", function(x,...) {as_dmat(as.data.frame(x),...)})

##' @export
##' @rdname as_bmat
setMethod("as_dmat", "numeric", function(x,pat="*",...) {
  x <- grepn(x,pat, !missing(pat))
  do.call("dmat", list(x))
})
##' @export
##' @rdname as_bmat
setMethod("as_dmat", "data.frame", function(x,pat="*",cols=NULL, ...) {
  if(is.character(cols)) {
    cols <- cvec_cs(cols)
    if(!all(cols %in% names(x))) {
      stop("Invalid colums in cols argument.") 
    }
    cols <- names(x) %in% cols
  } else {
    cols <- grepl(pat,names(x))
  }
  x <- x[,cols,drop=FALSE]
  lapply(seq_len(nrow(x)), function(i) dmat(unlist(x[i,])))
})

