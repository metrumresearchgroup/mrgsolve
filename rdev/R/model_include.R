includes <- new.env()
plugins <- new.env()
plugins[[".depends"]] <- list(mrgx=c("Rcpp"),simeta="RcppArmadillo")


include_order <- c("RcppArmadillo", "Rcpp","BH", "mrgx")


get_plugins <- function(what) {
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


plugin_code <- function(x) {
  if(is.null(x)) return(NULL)
  s_pick(x,"code") 
}

set_clink <- function(x) {
  Sys.setenv(CLINK_CPPFLAGS = make_clink(x)) 
  return(invisible(NULL))
}

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
  code="#define ARMA_DONT_USE_CXX11\n#include <RcppArmadillo.h>\n#define NDEBUG 1\n",
  linkto = c("Rcpp/include", "RcppArmadillo/include")
)



plugins[["BH"]] <- list(linkto="BH/include", name="BH")

