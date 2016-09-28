## This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
## To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
## Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.



##' Extract example model from system library
##'
##' @param model name of model
##' @param project working directory
##' @param overwrite passed to file.copy
##' @param quiet don't print any status messages to the screen
##' @param ... additional arguments
##' @return NULL
##' @export mrgsolve_example
##' @examples
##' ## example("mrgsolve_example", package="mrgsolve")
##'
##' mrgsolve_example("pkpdExample", project=getwd())
##'
##' mod <- mread("pkpdExample", project=getwd()) %>% ev(amt=1000, time=0, cmt=1)
##'
##' see(mod)
##'
##' out <- mod %>% mrgsim(end=48,delta=0.1)
##'
##' out
##'
##' plot(out)
##'
##' out <- mod %>%
##'   ev(amt=1000, ii=24, cmt=1, addl=10)  %>%
##'   mrgsim(end=300)
##'
##' plot(out)
##' plot(out, CP~time)
##'
mrgsolve_example <- function(model=c("pkExample", "pkpdExample","firstmodeExample","viralExample","popExample"),
                             project=getwd(),overwrite=FALSE,quiet=FALSE,...) {
  
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





