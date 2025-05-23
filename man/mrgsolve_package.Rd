% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\docType{package}
\name{mrgsolve-package}
\alias{mrgsolve}
\alias{mrgsolve-package}
\title{mrgsolve: Simulate from ODE-Based Models}
\description{
mrgsolve is an R package maintained under the auspices of
Metrum Research Group that facilitates simulation from
models based on systems of ordinary differential equations (ODE)
that are typically employed for understanding pharmacokinetics,
pharmacodynamics, and systems biology and pharmacology. mrgsolve
consists of computer code written in the R and C++ languages,
providing an interface to a C++ translation of the lsoda differential
equation solver. See \link{aboutsolver} for more information.
}
\section{Resources}{

\itemize{
\item Main mrgsolve resource page: \url{https://mrgsolve.org}
\item User guide: \url{https://mrgsolve.org/user_guide/}
\item Package documentation and vignettes: \url{https://mrgsolve.org/docs/}
}
}

\section{Package-wide options}{

\itemize{
\item \code{mrgolve.project}: sets the default project director (\code{\link[=mread]{mread()}})
\item \code{mrgsolve.soloc}: sets the default package build directory (\code{\link[=mread]{mread()}})
\item \code{mrgsolve_mread_quiet}: don't print messages during \code{\link[=mread]{mread()}}
\item \code{mrgsolve.update.strict}: this option has been deprecated; use the \code{strict}
argument to \code{\link[=update]{update()}} instead
}
}

\examples{

## example("mrgsolve")

mod <- mrgsolve::house(delta=0.1)  \%>\% param(CL=0.5)

events <-  ev(amt=1000, cmt=1, addl=5, ii=24)

events

mod

see(mod)

\dontrun{
stime(mod)
}
param(mod)

init(mod)

out <- mod \%>\% ev(events) \%>\% mrgsim(end=168)

head(out)
tail(out)
dim(out)

plot(out, GUT+CP~.)

sims <- as.data.frame(out)

t72 <- dplyr::filter(sims, time==72)

str(t72)

idata <- data.frame(ID=c(1,2,3), CL=c(0.5,1,2),VC=12)
out <- mod \%>\% ev(events) \%>\% mrgsim(end=168, idata=idata, req="")
plot(out)

out <- mod \%>\% ev(events) \%>\% mrgsim(carry_out="amt,evid,cmt,CL")
head(out)

ev1 <- ev(amt=500, cmt=2,rate=10)
ev2 <- ev(amt=100, cmt=1, time=54, ii=8, addl=10)
events <- c(ev1+ev2)
events

out <- mod \%>\% ev(events) \%>\% mrgsim(end=180, req="")
plot(out)


## "Condensed" data set
data(extran1)
extran1

out <- mod \%>\% data_set(extran1) \%>\% mrgsim(end=200)

plot(out,CP~time|factor(ID))


## idata
data(exidata)

out <- 
  mod \%>\% 
  ev(amt=1000, cmt=1) \%>\% 
  idata_set(exidata) \%>\%  
  mrgsim(end=72)

plot(out, CP~., as="log10")


# Internal model library
\dontrun{
mod <- mread("irm1", modlib())

mod

x <- mod \%>\% ev(amt=300, ii=12, addl=3) \%>\% mrgsim

}

}
\seealso{
Useful links:
\itemize{
  \item \url{https://mrgsolve.org/docs/}
  \item \url{https://github.com/metrumresearchgroup/mrgsolve}
  \item Report bugs at \url{https://github.com/metrumresearchgroup/mrgsolve/issues}
}

}
\author{
\strong{Maintainer}: Kyle T Baron \email{kyleb@metrumrg.com} (\href{https://orcid.org/0000-0001-7252-5656}{ORCID})

Other contributors:
\itemize{
  \item Bill Gillespie [contributor]
  \item Charles Margossian [contributor]
  \item Devin Pastoor [contributor]
  \item Bill Denney (\href{https://orcid.org/0000-0002-5759-428X}{ORCID}) [contributor]
  \item Dilawar Singh [contributor]
  \item Felicien Le Louedec (\href{https://orcid.org/0000-0003-3699-2729}{ORCID}) [contributor]
  \item Timothy Waterhouse (\href{https://orcid.org/0000-0002-0954-9660}{ORCID}) [contributor]
  \item Kyle Meyer [contributor]
  \item Metrum Research Group [copyright holder]
}

}
