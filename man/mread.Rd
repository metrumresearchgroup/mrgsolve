% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mread.R
\name{mread}
\alias{mread}
\alias{mread_cache}
\alias{mread_file}
\title{Read a model specification file}
\usage{
mread(
  model,
  project = getOption("mrgsolve.project", getwd()),
  code = NULL,
  file = NULL,
  udll = TRUE,
  ignore.stdout = TRUE,
  raw = FALSE,
  compile = TRUE,
  audit = TRUE,
  quiet = getOption("mrgsolve_mread_quiet", FALSE),
  check.bounds = FALSE,
  warn = TRUE,
  soloc = getOption("mrgsolve.soloc", tempdir()),
  capture = NULL,
  preclean = FALSE,
  recover = FALSE,
  ...
)

mread_cache(
  model = NULL,
  project = getOption("mrgsolve.project", getwd()),
  file = paste0(model, ".cpp"),
  code = NULL,
  soloc = getOption("mrgsolve.soloc", tempdir()),
  quiet = FALSE,
  preclean = FALSE,
  capture = NULL,
  ...
)

mread_file(file, ...)
}
\arguments{
\item{model}{model name.}

\item{project}{location of the model specification file an any
headers to be included; see also the discussion about model; this argument
can be set via \code{\link[=options]{options()}}.
library under details as well as the \code{\link[=modlib]{modlib()}} help topic.}

\item{code}{a character string with model specification code to be
used instead of a model file.}

\item{file}{the full file name (with extension, but without path)
where the model is specified.}

\item{udll}{use unique name for shared object.}

\item{ignore.stdout}{passed to system call when compiling the model; set this
to \code{FALSE} to print output to the R console.}

\item{raw}{if \code{TRUE}, return model content as a list, bypassing the compile
step; this argument is typically used for debugging problems with the
model build.}

\item{compile}{logical; if \code{TRUE}, the model will be built.}

\item{audit}{check the model specification file for errors.}

\item{quiet}{don't print messages from mrgsolve when compiling.}

\item{check.bounds}{check boundaries of parameter list.}

\item{warn}{logical; if \code{TRUE}, print warning messages that may arise while
building the model.}

\item{soloc}{the directory location where the model shared object is built
and stored; see details; this argument can be set via \code{\link[=options]{options()}};
if the directory does not exist, \code{mread()} will attempt to create it.}

\item{capture}{a character vector or comma-separated string of additional
model variables to capture; these variables will be added to the capture
list for the current call to \code{mread()} only.}

\item{preclean}{logical; if \code{TRUE}, compilation artifacts are
cleaned up first.}

\item{recover}{if \code{TRUE}, a list of build will be returned in case
the model shared object fails to compile; use this option to and
the returned object to collect information assist in debugging.}

\item{...}{passed to \code{\link[=update]{update()}}; also arguments passed
to \code{mread()} from \code{mread_cache()}.}
}
\description{
\code{mread()} reads and parses the mrgsolve model specification file,
builds the model, and returns a model object for simulation.
\code{mread_cache()} does the same, but caches the compilation result for
later use. \code{mread_file()} can be used for convenience, taking the model
file name as the first argument.
}
\details{
The \code{model} argument is required.  For typical use,
the \code{file} argument is omitted and the value
for \code{file} is generated from the value for \code{model}.
To determine the source file name, mrgsolve will look for
a file extension in \code{model}.  A file extension is
assumed when it finds a period followed by one to three alpha-numeric
characters at the end of the string (e.g. \code{mymodel.txt} but not
\code{my.model}).  If no file extension is found, the extension \code{.cpp}
is assumed (e.g. \code{file} is \verb{<model-name>.cpp}).  If a file
extension is found, \code{file} is \verb{<model-name>}.

Best practice is to avoid using \code{.} in \code{model} unless
you are using \code{model} to point to the model specification
file name. Otherwise, use \code{mread_file()}.

Use the \code{soloc} argument to specify a directory location for building
the model.  This is the location where the model shared object will be
stored on disk.  The default is a temporary directory, so compilation
artifacts are lost when R restarts when the default is used.  Changing
\code{soloc} to a persistent directory location will preserve those
artifacts across R restarts. Also, if simulation from a single model is
being done in separate processes on separate compute nodes, it might be
necessary to store these compilation artifacts in a local directory
to make them accessible to the different nodes. If the \code{soloc}
directory does not exist, \code{mread()} will attempt to create it.

Similarly, using \code{mread_cache()} will cache results in the temporary
directory and the cache cannot be accessed after the R process is
restarted.
}
\section{Model Library}{


mrgsolve comes bundled with several pre-coded PK, PK/PD, and
other systems models that are accessible via the \code{mread()} interface.

Models available in the library include:
\itemize{
\item PK models: \code{pk1cmt}, \code{pk2cmt}, \code{pk3cmt}, \code{pk1}, \code{pk2}, \code{popex}, \code{tmdd}
\item PKPD models: \code{irm1}, \code{irm2}, \code{irm3}, \code{irm4}, \code{emax}, \code{effect}
\item Other models: \code{viral1}, \code{viral2}
}

When the library model is accessed, mrgsolve will compile and load
the model as you would for any other model.  It is only necessary to
reference the correct model name and point the \code{project} argument
to the mrgsolve model library location via \code{\link[=modlib]{modlib()}}.

For more details, see \link{modlib_pk}, \link{modlib_pkpd}, \link{modlib_tmdd},
\link{modlib_viral}, and \link{modlib_details} for more information about the state
variables and parameters in each model.
}

\examples{

\dontrun{
code <- '
$PARAM CL = 1, VC = 5
$CMT CENT
$ODE dxdt_CENT = -(CL/VC)*CENT;
'

mod <- mcode("ex_mread", code)
mod

mod \%>\% init(CENT=1000) \%>\% mrgsim() \%>\% plot()

mod <- mread("irm3", modlib())

# if the model is in the file mymodel.cpp
mod <- mread("mymodel")

# if the model is in the file mymodel.txt
mod <- mread(file = "mymodel.txt")

or

mod <- mread_file("mymodel.txt")
}

}
\seealso{
\code{\link[=mcode]{mcode()}}, \code{\link[=mcode_cache]{mcode_cache()}}
}
