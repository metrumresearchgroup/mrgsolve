
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

