# Copyright (C) 2013 - 2026  Metrum Research Group
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

# Write PREAMBLE 
write_block_header <- function(stub, code, defs, model) {
  if(!length(code)) return(NULL)
  q <- '"'
  start <- paste0("#include ", q, "mrgsolve-unused-variable-start.h", q)
  end   <- paste0("#include ", q, "mrgsolve-unused-variable-end.h", q)
  hfile <- paste0(model, "-mread-decode-", stub, ".h")
  con <- file(hfile, encoding = "UTF-8")
  on.exit(close(con))
  writeLines(c(start, defs, end), con = con)
  paste0("#include ", q, hfile, q)
}

# Get the labels out of omega or sigma matrixlist object
generate_matrix_label_rd <- function(obj) {
  lab <- NULL
  prefix <- ifelse(inherits(obj, "omegalist"), "ETA", "EPS")
  if(sum(nrow(obj)) > 0) {
    index <- seq_len(sum(nrow(obj)))
    lab <- unlist(obj@labels, use.names = FALSE)
    stopifnot(length(index)==length(lab))
    which <- lab != "."
    if(sum(which) > 0) {
      lab <- paste0("#define ", lab[which], " _x", prefix, "(",index[which],")")
    } else {
      lab <- NULL
    }
  } 
  lab
}

generate_rdef_ref <- function(x) {
  if(!is.character(x)) return(x)
  paste0("double& ", x)
}

generate_rdef_const_ref <- function(x) {
  if(!is.character(x)) return(x)
  paste0("const double& ", x)
}

generate_rdef_const <- function(x) {
  if(!is.character(x)) return(x)
  paste0("const double ", x)
}

generate_rdefs <- function(x, cmtn = NULL, plugin = NULL, ...) {
  
  func <- ode_func(x)
  init_fun <- main_func(x)
  table_fun <- table_func(x)
  event_fun <- event_func(x)
  config_fun <- config_func(x)
  model <- model(x)
  
  pars <- Pars(x)
  cmts <- Cmt(x)
  ncmt <- length(cmts)
  npar <- length(pars)
  ipar <- seq(npar)-1
  icmt <- seq(ncmt)-1
  
  rpars <- paste0(pars, " = _THETA_[", ipar, "];")
  rcmt  <- paste0(cmts, " = _A_[", icmt, "];")
  rinit <- paste0(cmts, "_0 = _A_0_[", icmt, "];")
  rdx   <- paste0("dxdt_", cmts, " = _DADT_[", icmt, "];")
  
  Fdef <- Adef <- Ddef <- Rdef <- cmtndef <- NULL
  etal <- epsl <- NULL
  
  cmtn <- unique(intersect(cvec_cs(cmtn), cmts))
  
  # Using the N_CMT plugin gives you _all_ compartments
  if(!is.null(plugin[["N_CMT"]])) cmtn <- cmts  
  
  if(length(cmtn) > 0 && length(cmts) > 0) {
    cmtnindex <- match(cmtn, cmts)-1
    cmtndef <- paste0("N_",    cmtn, " = ", cmtnindex+1, ";")
    Fdef <-    paste0("F_",    cmtn, " = _F_[",cmtnindex,"];")
    Adef <-    paste0("ALAG_", cmtn, " = _ALAG_[",cmtnindex,"];")
    Ddef <-    paste0("D_",    cmtn, " = _D_[",cmtnindex,"];")
    Rdef <-    paste0("R_",    cmtn, " = _R_[",cmtnindex,"];")
  }
  
  if(npar==0) rpars <- NULL
  if(ncmt==0) cmtndef <- rcmt <- rinit <- rdx <- NULL
  
  ans <- list()
  ans$global <- character(0)
  ans$param <- rpars
  ans$cmt <- rcmt
  ans$init <- rinit
  ans$init_const <- rinit
  ans$dxdt <- rdx
  ans$frda <- c(Fdef, Rdef, Ddef, Adef)
  ans$frda_const <- ans$frda
  ans$cmtn <- cmtndef
  
  eta <- generate_matrix_label_rd(omat(x))
  eps <- generate_matrix_label_rd(smat(x))
  
  tokens <- lapply(ans, strsplit, split = " ")
  tokens <- unlist(lapply(tokens, \(x) {lapply(x, "[[", 1L)}), use.names = FALSE)
  ans$tokens <- tokens
  
  # const double reference
  make_ref <- c("init", "dxdt", "frda")
  ans[make_ref] <- lapply(ans[make_ref], generate_rdef_ref)
  
  make_const_ref <- c("param", "cmt", "init_const", "frda_const")
  ans[make_const_ref] <- lapply(ans[make_const_ref], generate_rdef_const_ref)
  
  if(!is.character(cmtn)) {
    ans$cmtn <- generate_rdef_const(ans$cmtn)
  }
  
  ans$global <- c(
    paste0("#define __INITFUN___ ", init_fun),
    paste0("#define __ODEFUN___ ", func),
    paste0("#define __TABLECODE___ ", table_fun),
    paste0("#define __EVENTFUN___ ", event_fun), 
    paste0("#define __CONFIGFUN___ ", config_fun),
    paste0("#define __REGISTERFUN___ ", register_fun(model)),
    paste0("#define _nEQ ", ncmt),
    paste0("#define _nPAR ", npar), 
    eta, 
    eps
  )

  discard <- sapply(ans, is.null)
  
  if(any(discard)) {
    ans <- ans[!discard]  
  }
  
  ans
}

relocate_funs <- function(x,PACKAGE) {
  x@package <- PACKAGE
  x
}

build_version <- function(x) {
  x@shlib[["version"]] 
}

compiled <- function(x,...) UseMethod("compiled")
#' @export
compiled.default <- function(x,...) return(FALSE)
#' @export
compiled.mrgmod <- function(x, status = NULL, ...) {
  if(is.null(status)) return(x@shlib$compiled)
  x@shlib$compiled <- status
  return(x)
}

win_def_name <- function(x) {
  paste0(compbase(model(x)),"-win.def")
}

write_win_def <- function(x) {
  if(.Platform$OS.type != "windows") return(NULL)
  cat(file=win_def_name(x), c("EXPORTS",paste0(" ", funs(x))),sep="\n")
}

rm_win_def <- function(x) {
  if(is.character(x)) {
    if(file_exists(x)) {
      file.remove(x)
    }
  }
  return(invisible(NULL))
}


SAFE_WAIT_TIME <- 1.5

update_wait_time <- function(n) {
  assignInMyNamespace("SAFE_WAIT_TIME",n)
}


check_and_copy <- function(from,to) {
  
  if(!file_exists(to)) {
    file.copy(from,to)
    same <- TRUE
  } else {
    same <- tools::md5sum(from) == tools::md5sum(to)
    if(!same) {
      file.copy(from,to,overwrite=TRUE)
    }
  }
  z <- file.remove(from)
  return(same)
}

# Wait a certain amount of time before re-compiling
# and loading a model
safe_wait <- function(x) {
  
  target <- file.path(soloc(x),compout(model(x)))
  if(!file_exists(target)) return(invisible(NULL))
  mt <- file.info(target)[["mtime"]]
  age <- as.numeric(as.POSIXct(Sys.time())) - as.numeric(as.POSIXct(mt))
  if(age > SAFE_WAIT_TIME) return(invisible(NULL))
  return(Sys.sleep(SAFE_WAIT_TIME-age))
}


# Clean up model shared objects
# 
# @param x model object
# @param where directory to clean up
# 
# @details
# \code{cleanso} removes (deletes) shared objects from the model compile directory and 
# attempts to unload shared objects which appear to be loaded.
# 
#
# 
cleanso <- function(x,where=soloc(x)) {
  so <- list.files(where, 
                   pattern =  paste0("*\\", .Platform$dynlib.ext), full.names=TRUE)
  so <- so[so != file.path(where,compout(model(x)))]
  lo <- sapply(getLoadedDLLs(), "[[", "path")
  y <- intersect(lo,so)
  for(w in y) foo <- try(silent=TRUE,dyn.unload(w))
  for(w in y) foo <- try(silent=TRUE,file.remove(w))
  return(invisible(NULL))
}

##' Get inits from compiled function
##' 
##' This function is usually for internal use.
##'
##' @param x mrgmod model object
##' @param keep_pointers should function pointers be returned?
##' 
##' @export
##' @keywords internal
touch_funs <- function(x, keep_pointers = TRUE) {
  funp <- pointers(x)
  out <- .Call(`_mrgsolve_TOUCH_FUNS`, funp, x, PACKAGE = "mrgsolve")
  names(out$init) <- Cmt(x)
  if(keep_pointers) {
    out[["pointers"]] <- funp
  }
  out
}
