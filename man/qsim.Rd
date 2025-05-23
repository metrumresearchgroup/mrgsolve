% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrgsolve.R
\name{qsim}
\alias{qsim}
\title{Basic, simple simulation from model object}
\usage{
qsim(
  x,
  data,
  idata = no_idata_set(),
  obsonly = FALSE,
  tgrid = NULL,
  recsort = 1,
  tad = FALSE,
  Req = NULL,
  outvars = Req,
  skip_init_calc = FALSE,
  output = "mrgsims"
)
}
\arguments{
\item{x}{the model object.}

\item{data}{can be either event object or data set.}

\item{idata}{a matrix or data frame of model parameters,
one parameter per row (see \code{\link[=idata_set]{idata_set()}}).}

\item{obsonly}{if \code{TRUE}, dosing records are not included
in the output.}

\item{tgrid}{a tgrid object; or a numeric vector of simulation times
or another object with an \code{stime} method.}

\item{recsort}{record sorting flag.  Default value is 1.  Possible values
are 1,2,3,4: 1 and 2 put doses in a data set after padded observations at
the same time; 3 and 4 put those doses before padded observations at the
same time.  2 and 4 will put doses scheduled through \code{addl} after
observations at the same time; 1 and 3 put doses scheduled through
\code{addl} before observations at the same time. \code{recsort} will
not change the order of your input data set if both doses and observations
are given.}

\item{tad}{when \code{TRUE} a column is added to simulated
output is added showing the time since the last dose.  Only data records
with \code{evid == 1} will be considered doses for the purposes of \code{tad}
calculation. The \code{tad} can be properly calculated with a dosing lag time in
the model as long as the dosing lag time (specified in \verb{$MAIN})  is always
appropriate for any subsequent doses scheduled through \code{addl}.  This will
always be true if the lag time doesn't change over time.  But it might
(possibly) not hold if the lag time changes prior to the last dose in the
\code{addl} sequence.  This known limitation shouldn't affect \code{tad} calculation
in most common dosing lag time implementations.}

\item{Req}{synonym for \code{outvars}.}

\item{outvars}{output items to request; if missing, then only captured items
will be returned in the output.}

\item{skip_init_calc}{don't use \verb{$MAIN} to calculate initial conditions.}

\item{output}{output data type; the default is \code{mrgsims}, which returns the
default output object; other options include \code{df} (for data.frame) or
\code{matrix}.}
}
\description{
This is just a lighter version of \code{\link[=mrgsim]{mrgsim()}}, with fewer options but with
better efficiency in certain cases.  See \strong{Details}.
}
\details{
\code{qsim()} mainly cuts some of the overhead from the simulation. So, the
primary efficiency gain from using \code{qsim()} comes when the simulation
executes very quickly. It is unlikely you will see a big performance
difference between \code{qsim()} and \code{\link[=mrgsim]{mrgsim()}} when the model is difficult to
solve or if there is a large input data set.

There is no pipeline interface for this function; all configuration options
(see \strong{Arguments}) must be passed as formal arguments to the function.
You can't \code{carry_out}, \code{Request} specific columns, or pass items in for update.
Some other limitations, but only convenience-related.  See \strong{Arguments} for
available options.  Specifically, there is no \code{...} argument for this function.
Use the \code{\link[=update]{update()}} method to update the model object.
}
\examples{

mod <- mrgsolve::house()

dose <- ev(amt = 100)

out <- qsim(mod,dose)

}
\seealso{
\code{\link[=mrgsim_q]{mrgsim_q()}}, \code{\link[=mrgsim]{mrgsim()}}, \link{mrgsim_variants}
}
