% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/param.R
\name{param}
\alias{param}
\alias{param,mrgmod-method}
\alias{param,mrgsims-method}
\alias{param,missing-method}
\alias{param,list-method}
\alias{param,ANY-method}
\alias{allparam}
\title{Create and work with parameter objects}
\usage{
param(.x, ...)

\S4method{param}{mrgmod}(.x, .y = NULL, ..., .pat = "*", .strict = FALSE)

\S4method{param}{mrgsims}(.x, ...)

\S4method{param}{missing}(..., .strict = TRUE)

\S4method{param}{list}(.x, ...)

\S4method{param}{ANY}(.x, ...)

allparam(.x)
}
\arguments{
\item{.x}{the model object.}

\item{...}{passed along or name/value pairs to update the parameters
in a model object; when passing new values this way, all values
must be numeric and all all names must exist in the parameter list
for \code{.x}.}

\item{.y}{an object to be merged into parameter list; non-\code{NULL} values
must be named \code{list}, \code{data.frame}, \code{numeric} vector, or \code{parameter_list}
object; named items that do not exist in the parameter list are allowed and
will be silently ignored; use the \code{.strict} argument to require that all
names in \code{.y} exist already in the parameter list.}

\item{.pat}{a regular expression (character) to be applied as a filter
for which parameters to show when printing.}

\item{.strict}{if \code{TRUE}, all names to be updated must be found
in the parameter list.}
}
\value{
An object of class \code{parameter_list} (see \link{numericlist}).
}
\description{
See \link{numericlist} for methods to deal with \code{parameter_list} objects.
}
\details{
Can be used to either get a parameter list object from a \code{mrgmod}
model object or to update the parameters in a model object.
For both uses, the return value is a \code{parameter_list} object. For the
former use, \code{param()} is usually called to print the parameters to the
screen, but the \code{parameter_list} object can also be coerced to a list
or numeric R object.

Use \code{allparam()} to get a \code{parameter_list} object including
both model parameters and data items listed in \verb{$FIXED}.

The update to parameters can be permissive (candidates with names that don't
exist in the parameter list are silently ignored) or strict (all candidates
must already exist in the parameter list). When passing candidate values
via \code{...}, the update is strict and an error is generated if you pass a
name that isn't found in the parameter list. When candidate values
are passed as a named object via \code{.y}, then the update is permissive. Any
permissive update can be made strict (error if foreign names are found in
the candidates) by passing \code{.strict = TRUE}.

An alternative is to assess the incoming names using \code{\link[=inventory]{inventory()}}.
}
\examples{
## example("param")

mod <- house()

param(mod)

param(mod, .pat="^(C|F)") ## may be useful when large number of parameters

class(param(mod))

param(mod)$KA

param(mod)[["KA"]]

as.list(param(mod))

as.data.frame(param(mod))

mod <- param(mod, CL = 1.2)

new_values <- list(CL = 1.3, VC = 20.5)

mod <- param(mod, new_values)


}
\seealso{
\code{\link[=inventory]{inventory()}}
}
\keyword{param}
