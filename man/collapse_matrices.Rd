% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matlist.R
\name{collapse_omega}
\alias{collapse_omega}
\alias{collapse_sigma}
\title{Collapse OMEGA or SIGMA matrix lists}
\usage{
collapse_omega(x, range = NULL, name = NULL)

collapse_sigma(x, range = NULL, name = NULL)
}
\arguments{
\item{x}{a model object.}

\item{range}{numeric vector of length 2 specifying the range of matrices
to collapse in case there are more than 2. The second element may be \code{NA}
to indicate the length of the list of matrices.}

\item{name}{a new name for the collapsed matrix; note that this is the
matrix name, not the labels which alias \code{ETA(n)} or \code{EPS(n)}; specifying a
name will only alter how this matrix is potentially updated in the future.}
}
\value{
A model object with updated \code{OMEGA} or \code{SIGMA} matrix lists.
}
\description{
If multiple \code{OMEGA} (or \code{SIGMA}) blocks were written into the model,
these can be collapsed into a single matrix. This will not change the
functionality of the model, but will alter how \code{OMEGA} (or \code{SIGMA}) are
updated, usually making it easier. This "collapsing" of the matrix list
is irreversible.
}
\examples{
code <- '
$OMEGA 1 2 3
$OMEGA 4 5
$OMEGA 6 7 8 9
'

mod <- mcode("collapse-example", code, compile = FALSE)
revar(mod)
collapse_omega(mod) \%>\% omat()
collapse_omega(mod, range = c(2,3), name = "new_matrix") \%>\% omat()
collapse_omega(mod, range = c(2,NA), name = "new_matrix") \%>\% omat()

}
\seealso{
\code{\link[=collapse_matrix]{collapse_matrix()}}
}
