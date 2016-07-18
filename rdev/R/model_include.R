includes <- new.env()
plugins <- new.env()
plugins[[".depends"]] <- list(mrgx=c("Rcpp"),simeta=c("RcppArmadillo"))


include_order <- c("RcppArmadillo", "Rcpp","BH", "mrgx", "base")

get_restore <- function(what=c("PKG_LIBS", "CYGWIN", "CLINK_CPPFLAGS")) {
  as.list(Sys.getenv(what, unset=NA)) 
}
do_restore <- function(restore) {
  unset <- sapply(restore, is.na)
  if(any(!unset))  do.call(Sys.setenv,restore[!unset])
  if(any(unset))   Sys.unsetenv(names(restore[unset]))
}


get_plugins <- function(what) {
  what <- c(what,"base")
  what <- unique(c(get_depends(what),what))
  if(all(c("Rcpp", "RcppArmadillo") %in% what)) {
    what <- what[what != "Rcpp"] 
  }
  x <- lapply(what,get_plugin)
  names(x) <- s_pick(x,"name")
  x
}

get_depends <- function(what) {
  what <- intersect(what,ls(plugins[[".depends"]]))
  plugins[[".depends"]][what]
}

get_plugin <- function(what) {
  if(!exists(what,plugins)) {
    stop("Plugin ", what, " could not be found.")
  }
  plugins[[what]]
}

make_clink <- function(x) {
  if(is.null(x)) return(NULL)
  link <- unique(s_pick(x,"linkto"))
  link <- sapply(link,function(ln) {
    y <- find.package(dirname(ln))
    build_path(file.path(y,basename(ln)))
  })
  paste(paste0("-I\"",link, "\""),collapse=" ")
}

make_libs <- function(x) {
  if(is.null(x)) return(NULL)
  libs <- unique(s_pick(x,"libs"))
  paste0(libs, collapse=" ")
}

plugin_code <- function(x) {
  if(is.null(x)) return(NULL)
  s_pick(x,"code") 
}

plugin_names <- function(x) {
  if(is.null(x)) return("// NO PLUGINS")
  x <- s_pick(x,"name")
  paste0("// PLUGINS: ", paste(x, collapse=" "))
}

set_clink <- function(x) {
  if(is.null(x)) return(invisible(NULL))
  Sys.setenv(CLINK_CPPFLAGS = make_clink(x)) 
}

set_libs <- function(x) {
  if(is.null(x)) return(invisible(NULL))
  Sys.setenv(PKG_LIBS = make_libs(x)) 
}

set_nodos <- function() {
  Sys.setenv(CYGWIN = "nodosfilewarning") 
}

set_up_env <- function(x) {
  restore <- get_restore()
  set_nodos()
  if(!is.null(x)) {
    set_clink(x)
    set_libs(x)
  }
  return(restore)
}

plugins[["base"]] <- list(
  linkto="mrgsolve/base", name="base"
)

plugins[["simeta"]] <- list(
  linkto="mrgsolve/mrgx",
  code='#include "simeta.h"\n',name="simeta"
)

plugins[["mrgx"]] <- list(
  linkto="mrgsolve/mrgx",
  code='#include "mrgx.h"\n',name="mrgx"
)
plugins[["Rcpp"]] <- list(
  code = "#include <Rcpp.h>\n",
  linkto = "Rcpp/include", name="Rcpp"
)
plugins[["RcppEigen"]] <- list(
  code = "#include <RcppEigen.h>\n",
  linkto = c("Rcpp/include","RcppEigen/include"), name="RcppEigen"
)

plugins[["RcppArmadillo"]] <- list(
  name="RcppArmadillo",
  libs = c("$(LAPACK_LIBS)", "$(BLAS_LIBS)", "$(FLIBS)"),
  code="#define ARMA_DONT_USE_CXX11\n#include <RcppArmadillo.h>\n#define NDEBUG 1\n",
  linkto = c("Rcpp/include", "RcppArmadillo/include")
)

plugins[["BH"]] <- list(linkto="BH/include", name="BH")

