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


includes <- new.env()
plugins <- new.env()
plugins[[".depends"]] <- list(mrgx=c("Rcpp"))

include_order <- c("RcppArmadillo", "Rcpp","BH", "mrgx")


get_plugins <- function(what) {
  what <- c(cvec_cs(what), "base")
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


plugin_code <- function(x) {
  if(is.null(x)) return(NULL)
  s_pick(x,"code") 
}

plugin_names <- function(x) {
  if(is.null(x)) return("// NO PLUGINS")
  x <- s_pick(x,"name")
  paste0("// PLUGINS: ", paste(x, collapse=" "))
}


make_clink <- function(x,clink) {
  if(is.null(x)) return(NULL)
  link <- unique(s_pick(x,"linkto"))
  link <- sapply(link,function(ln) {
    y <- find.package(dirname(ln))
    build_path(file.path(y,basename(ln)))
  })
  link <- c(link, build_path(clink))
  if(length(link)==0) return("")
  paste(paste0("-I\"",unique(link), "\""),collapse=" ")
}

set_clink <- function(x,clink=NULL) {
  if(is.null(x) & is.null(clink)) return(invisible(NULL))
  Sys.setenv(CLINK_CPPFLAGS = make_clink(x,clink)) 
}


make_libs <- function(x) {
  if(is.null(x)) return(NULL)
  libs <- unique(s_pick(x,"libs"))
  paste0(libs, collapse=" ")
}

set_libs <- function(x) {
  if(is.null(x)) return(invisible(NULL))
  Sys.setenv(PKG_LIBS = make_libs(x)) 
}

set_nodos <- function() {
  Sys.setenv(CYGWIN = "nodosfilewarning") 
}

set_up_env <- function(x,...) {
  restore <- get_restore()
  set_nodos()
  if(!is.null(x)) {
    set_clink(x,...)
    set_libs(x)
  }
  return(restore)
}

get_restore <- function(what=c("PKG_LIBS", "CYGWIN", "CLINK_CPPFLAGS")) {
  as.list(Sys.getenv(what, unset=NA)) 
}

do_restore <- function(restore) {
  unset <- sapply(restore, is.na)
  if(any(!unset))  do.call(Sys.setenv,restore[!unset])
  if(any(unset))   Sys.unsetenv(names(restore[unset]))
}


plugins[["base"]] <- list(
  linkto="mrgsolve/base", name="base"
)

# plugins[["simeta"]] <- list(
#   linkto="mrgsolve/mrgx",
#   code='#include "simeta.h"\n',name="simeta"
# )

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

plugins[["BH"]] <- list(
  linkto="BH/include", name="BH"
)

# @param x the build object
# 
# @details
# We write a Makevars file in the soloc
# Also, copy required headers (from mrgsolve base) there as well
write_build_env <- function(x) {
  
  mkv <- file.path(x$soloc,"Makevars")
  
  clink <- paste0("PKG_CPPFLAGS=",paste(Sys.getenv("CLINK_CPPFLAGS"), collapse=" "))
  libs <- paste0("PKG_LIBS=",paste(Sys.getenv("PKG_LIBS"),collapse=" "))
  
  
  cat(file=mkv, "# from write_make_vars", "\n")
  cat(file=mkv, clink, "\n", append=TRUE)
  cat(file=mkv, libs,  "\n", append=TRUE)
  
  headers <- file.path(system.file("base",package="mrgsolve"),c("modelheader.h", "mrgsolv.h"))
  
  if(!all(file.copy(headers, x$soloc, overwrite=TRUE))) {
    stop("Couldn't find mrgsolve install location.") 
  }

}
