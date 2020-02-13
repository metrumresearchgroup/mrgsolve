# Copyright (C) 2013 - 2020  Metrum Research Group
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

eng_mrgsolve_c <- function(options) {
  code <- options$code
  options$eval <- FALSE
  options$engine <- "c"
  knitr::engine_output(options, code, '')
}

eng_mrgsolve_r <- function(options) {
  code <- options$code
  options$eval <- FALSE
  options$engine <- "r"
  knitr::engine_output(options, code, '')
}

document <- function(number_sections = TRUE, highlight = "pygments", 
                     output = "html", ...) {
  if(!requireNamespace("knitr")) {
    stop("the knitr package is required to use mrgsolve:::document.", 
         call.=FALSE)  
  }
  if(!requireNamespace("rmarkdown")) {
    stop("the rmarkdown package is required to use mrgsolve:::document.", 
         call.=FALSE)  
  }
  
  knitr::knit_engines$set(
    ode = eng_mrgsolve_c, param=eng_mrgsolve_c,
    cmt = eng_mrgsolve_c, capture = eng_mrgsolve_c, 
    main = eng_mrgsolve_c, set = eng_mrgsolve_r, 
    omega = eng_mrgsolve_c, sigma = eng_mrgsolve_c, 
    table = eng_mrgsolve_c, include = eng_mrgsolve_c, 
    global = eng_mrgsolve_r, env = eng_mrgsolve_r, 
    preamble = eng_mrgsolve_c, pkmodel = eng_mrgsolve_r, 
    theta = eng_mrgsolve_c, yaml = eng_mrgsolve_c, 
    pred = eng_mrgsolve_c, init = eng_mrgsolve_c
  )
  fun <- rmarkdown::html_document
  if(output=="pdf") fun <- rmarkdown::pdf_document
  fun(..., number_sections = number_sections, highlight = highlight)
}


##' Render a model to a document
##' 
##' @param x model object or the model name
##' @param project the directory containing the \code{.cpp} model file 
##' @param ... passed to \code{rmarkdown::render}
##' 
##' 
##' @examples
##' \dontrun{
##' mod <- mrgsolve::house()
##' mrgsolve:::render(mod)
##' mrgsolve:::render("irm2", modlib())
##' }
##' 
##' @rdname render
setGeneric("render", function(x,...) standardGeneric("render"))

##' @rdname render
setMethod("render", "character", function(x,project=NULL,...) {
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
    stop("need rmarkdown to use this function, 
         please install via install.packages('rmarkdown')")
  } 
  
  if(!file.exists(project)) {
    stop("project directory doesn't exist") 
  }
  
  if(is.null(template)) {
    template <- system.file("Rmd", "mrgsolve_render_template.Rmd",package="mrgsolve")
  }
  
  out <- filename(tempdir(), model,".Rmd")
  
  file.copy(template, out, overwrite=TRUE)
  
  pdf <- rmarkdown::render(
    out,
    params=list(model = model, project=project,  compile=compile), ...
  )
  
  invisible(file.copy(pdf, getwd(), overwrite=TRUE))
  
}

