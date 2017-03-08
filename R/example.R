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

##' Extract example model from system library
##'
##' @param model name of model
##' @param project working directory
##' @param overwrite passed to file.copy
##' @param quiet don't print any status messages to the screen
##' @param ... additional arguments
##' @return NULL
##' @export mrgsolve_example
##'
mrgsolve_example <- function(model=c("pkExample", "pkpdExample","firstmodeExample","viralExample","popExample"),
                             project=getwd(),overwrite=FALSE,quiet=FALSE,...) {
  stop("mrgsolve: mrgsolve_example function has been deprecated.")
  model <- match.arg(model)
  file <- paste0(model, ".cpp")
  src <- file.path(system.file(package="mrgsolve"), "models",file)
  target <- file.path(project, file)
  if(!quiet) message("Copying ", file, " to ", project)
  if(!(file.copy(src, target, overwrite=overwrite))) {
    if(file.exists(target)) stop("\nDestination file already exists.\nRename destination or set overwrite to TRUE.")
  } else {
    if(!quiet) message("Use mread() to load and compile.")
  }
  return(invisible(NULL))
}


##' Create model specification file from template
##' @param model name of the model to create
##' @param project working directory
##' @param writeable logical; if \code{TRUE}, parameters may be overwritten in the  \code{main} block
##' @param overwrite logical; if \code{TRUE}, an existing file with same stem will be overwritten
##' @return NULL
##' @export mrgsolve_template
mrgsolve_template <- function(model="template",
                              project=getwd(),
                              writeable = FALSE,
                              overwrite=FALSE) {
  
  stop("mrgsolve: mrgsolve_template function has been deprecated.")
  
  # if(!file.exists(project)) stop("project directory doesn't exist")
  # if(file.access(project, 2) != 0) stop("project directory must be writeable")
  # 
  # dest <- file.path(project, paste(model, "cpp", sep="."))
  # if(!overwrite & file.exists(dest)) stop("Destination file already exists")
  # 
  # src <- file.path(system.file(package="mrgsolve"),"template", "template.cpp")
  # 
  # con <- file(src, "r")
  # txt <- readLines(con, warn=FALSE)
  # close(con)
  # 
  # con <- file(dest, "w")
  # cat(txt, file=con, sep="\n")
  # close(con)
  # message("Wrote model specification template to ", dest)
  # message("Additional editing is required before compiling model.")
}





