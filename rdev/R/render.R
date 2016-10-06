
##' Render an \code{mrgsolve} model.
##' 
##' @param x model object
##' @param file file name to use for building; should have \code{.Rmd} extension
##' @param quiet not used
##' @param build if \code{TRUE}, the document is built with \code{rmarkdown::render}
##' @param ... not used
##' 
##' @return 
##' If the document is built, the path to the \code{.pdf} file 
##' is returned.  If the document is not built, path to 
##' the \code{.Rmd} file is returned.
##' 
##' @examples
##' \dontrun{
##' mod <- mrgsolve:::house()
##' 
##' mrgsolve:::render(mod,file="house.Rmd")
##' }
##' 
##' 

setGeneric("render", function(x,...) standardGeneric("render"))
setMethod("render", "character", function(x,project,...) {
  dorender(x,project,...)
})
setMethod("render", "mrgmod", function(x,...) {
    project <- tempdir()
    file <- basename(cfile(x))
    cat(code(x), file=filename(project,file),sep="\n")
    dorender(model(x),project,...)
})


dorender <- function(model,project,template=NULL,...) {
  
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
  
  pdf <- rmarkdown::render(out,params=list(model=model,project=project),...)
  
  file.copy(pdf, getwd(),overwrite=TRUE)
  
}
