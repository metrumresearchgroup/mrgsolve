includes <- new.env()

include_order <- c("RcppArmadillo", "Rcpp","BH", "mrgx")

order_includes <- function(x) {
  x[match(intersect(include_order,names(x)),names(x))]
}



#  Construct a path to a package to add to CLINK_CPPFLAGS
link_to <- function(package,dir="include") {
  path <- find.package(package, quiet=TRUE)
  path <- file.path(path,dir)
  return(paste0("-I\"",path,"\""))
}

# Get one include
get_include <- function(name) {
  if(!exists(name,includes)) {
    stop("Model include: ", name, " could not be found.")
  }
  includes[[name]]()
}


# Get all includes, include dependencies
get_includes <- function(what) {
  x <- lapply(what,get_include)
  dep <- s_pick(x,"depends")
  need <- setdiff(dep,what)
  x <- c(lapply(need,get_include),x)
  names(x) <- c(need,what)
  if(all(is.element(c("Rcpp","RcppArmadillo"),names(x)))) {
    x <- x[names(x) != "Rcpp"] 
  }
  order_includes(x)
}

reshape_includes <- function(x) {
  
  data <- x
  
  clink <- paste(unique(s_pick(x,"CLINK")),collapse=" ")
  
  new_env <- list(CLINK_CPPFLAGS = clink)
  
  code <- s_pick(x,"code")
  
  list(data=data,clink=clink,code=code,new_env=new_env)
  
}


includes[["Rcpp"]] <- function(deps=TRUE) {
  list(code=c("#include <Rcpp.h>"),
       CLINK = link_to("Rcpp"),
       name="Rcpp")
}

includes[["BH"]] <- function(deps=TRUE) {
  list(CLINK = link_to("BH"), name="BH")
}

includes[["RcppArmadillo"]] <- function(deps=TRUE) {
  
  list(CLINK = paste0(link_to("RcppArmadillo")," ",
                      link_to("Rcpp")),
       name="RcppArmadillo",
       code="#define ARMA_DONT_USE_CXX11
             #include <RcppArmadillo.h>
             #define NDEBUG 1"
  )
}

includes[["mrgx"]] <- function(deps=TRUE) {
  list(code='#include "mrgx.h"',
       name="mrgx",
       CLINK = link_to("mrgsolve", "mrgx"),
       depends=c("RcppArmadillo"))
}

modify_env <- function(x) {
  if(length(x$new_env)==0) return(invisible(NULL))
  return(invisible(do.call(Sys.setenv,x[["new_env"]])))
}

restore_env <- function(x) {
  Sys.unsetenv("CLINK_CPPFLAGS")
  return(invisible(NULL))
}

