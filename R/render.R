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


##' Render a model to a document.
##' 
##' @param x model object or the model name
##' @param project the directory containing the \code{.cpp} model file 
##' @param ... passed to \code{rmarkdown::render}
##' 
##' 
##' @examples
##' \dontrun{
##' mod <- mrgsolve:::house()
##' mrgsolve:::render(mod)
##' mrgsolve:::render("irm2", modlib())
##' }
##' 
##' @rdname render
setGeneric("render", function(x,...) standardGeneric("render"))

##' @rdname render
setMethod("render", "character", function(x,project,...) {
  dorender(x,project,...)
})

##' @rdname render
setMethod("render", "mrgmod", function(x,...) {
    project <- tempdir()
    file <- basename(cfile(x))
    cat(code(x), file=filename(project,file),sep="\n")
    dorender(model(x),project,...)
})

##' @param compile logical; if true, the model will be compiled to run
##' @param model model name
##' @param template template document
##' @rdname render
dorender <- function(model,project,template=NULL,compile=TRUE,...) {
  
  if(!requireNamespace("rmarkdown")) {
    stop("need rmarkdown to use this function, please install via install.packages('rmarkdown')")
  } 
  
  if(!file.exists(project)) {
    stop("project directory doesn't exist") 
  }
  
  if(is.null(template)) {
    template <- system.file("Rmd", "mrgsolve_render_template.Rmd",package="mrgsolve")
  }
  
  out <- filename(tempdir(),model,".Rmd")
  
  file.copy(template,out,overwrite=TRUE)
  
  pdf <- rmarkdown::render(out,params=list(model=model,project=project,compile=compile),...)
  
  invisible(file.copy(pdf, getwd(),overwrite=TRUE))

}

