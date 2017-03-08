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


# @param model model name
# @param project project directory; where the model is read from
# @param soloc the build directory
# @param code model code
# @param udll logical; if FALSE, a random dll name is generated
new_build <- function(model,project,soloc,code=NULL,preclean=FALSE,udll=FALSE) {
  
  ## Check for spaces in the model name
  if(charthere(model," ")) {
    stop("model name cannot contain spaces.")
  }
  
  if(any(charthere(project,"\n"))) {
    stop("project argument contains newline(s); did you mean to call mcode?",call.=FALSE) 
  }

  env <- new.env()
  env$win <- .Platform$OS.type=="windows"
  
  ## Both project and soloc get normalized
  if(!file_writeable(soloc)) {
    stop("soloc directory must exist and be writeable.",call.=FALSE) 
  }
  soloc <-   normalizePath(soloc, mustWork=TRUE, winslash="/")
  
  env$soloc <-   as.character(create_soloc(soloc,model,preclean))
  
  if(!file_readable(project)) {
    stop("project directory must exist and be readable.",call.=FALSE) 
  }
  env$project <- normalizePath(project, mustWork=TRUE, winslash="/")
  
  ## The model file is <stem>.cpp in the <project> directory
  env$modfile <- file.path(project,paste0(model, ".cpp"))
  
  ## If code is passed in as character:
  if(is.character(code)) {
    mod.con <- file(env$modfile, open="w")
    cat(code, "\n", file=mod.con)
    close(mod.con)
  }
  
  if(!file_exists(env$modfile)) {
    stop("The model file ", basename(env$modfile), " does not exist.",call.=FALSE) 
  }
  
  env$md5 <- tools::md5sum(env$modfile)
  
  env$package <- ifelse(udll,rfile(model),model)
  
  env$compfile <- compfile(model)
  env$compbase <- compbase(model)
  env$compout <- compout(model)
  env$compdir <- compdir()
  env$cachfile <- cachefile()
  
  return(env)

}

rfile <- function(pattern="",tmpdir=normalizePath(getwd(),winslash="/")){
  basename(tempfile(pattern=so_stem(pattern),tmpdir='.'))
}

## A random file name
so_stem <- function(x) paste0(x,"-so-")

## Form a file name / path for the file that is actually compiled
comppart <- "-mread-source"

compbase <- function(model) paste0(model, comppart)

compfile <- function(model) paste0(model, comppart,".cpp")

compout  <- function(model) paste0(model, comppart, .Platform$dynlib.ext)

compdir <- function() {
  paste(c("mrgsolve","so",
          as.character(GLOBALS[["version"]]),
          R.version$platform),collapse="-")
}

cachefile <- function(model) "model-cache.RDS"

create_soloc <- function(loc,model,preclean) {
  
  soloc <- file.path(loc,compdir(),model)
  
  if(preclean) unlink(soloc,recursive=TRUE)
  
  if(!file_exists(soloc)) dir.create(soloc,recursive=TRUE)
  
  return(soloc)
}


