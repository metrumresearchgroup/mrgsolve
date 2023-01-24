# Copyright (C) 2013 - 2023  Metrum Research Group
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

#' @include Aaaa.R

# @param model model name
# @param project project directory; where the model is read from
# @param soloc the build directory
# @param code model code
# @param udll logical; if FALSE, a random dll name is generated
new_build <- function(file=NULL, model, project, soloc=getwd(), code = NULL, 
                      preclean = FALSE, udll = FALSE, recover=FALSE) {
  
  if(length(model) == 0 | nchar(model)==0) {
    stop("Invalid model name.", call. = FALSE)
  }
  
  if(charthere(model," ")) {
    stop("Model name cannot contain spaces.", call. = FALSE)
  }
  
  if(is.null(file)) {
    file <- paste0(model,".cpp")
  }
  
  new_model <- gsub(".", "_", model, fixed = TRUE)
  
  if(any(charthere(project,"\n"))) {
    stop(
      "project argument contains newline(s); did you mean to call mcode?",
      call.=FALSE
    ) 
  }
  
  env <- new.env()
  
  env$win <- .Platform$OS.type=="windows"
  
  
  ## Both project and soloc get normalized
  if(!file_writeable(soloc)) {
    if(file_writeable(dirname(soloc))) {
      message("Creating build directory: ", soloc)
      dir.create(soloc)  
      if(!file_writeable(soloc)) {
        stop("soloc directory '",soloc,"' must exist and be writeable.",call.=FALSE)  
      }
    } else {
      stop("soloc directory '",soloc,"' must exist and be writeable.",call.=FALSE) 
    }
  } 
  
  soloc <- normalizePath(soloc, mustWork=TRUE, winslash="/")
  
  env$soloc <- as.character(create_soloc(soloc,new_model,preclean))
  
  if(!file_readable(project)) {
    stop("project directory '", project, "' must exist and be readable.",call.=FALSE) 
  }
  
  env$project <- normalizePath(project, mustWork=TRUE, winslash="/")  
  
  env$modfile <- file.path(env$project,file)
  
  ## If code is passed in as character:
  if(is.character(code)) {
    mod.con <- file(env$modfile, open="w")
    cat(code, "\n", file=mod.con)
    close(mod.con)
  }
  
  if(!file_exists(env$modfile)) {
    new_modfile <- file.path(env$project,model)
    if(!file_exists(new_modfile)) {
      stop("the model file ", 
           basename(env$modfile), 
           " does not exist.",call.=FALSE) 
    } else {
      env$modfile <- new_modfile  
    }
  }
  
  env$md5 <- md5sum(env$modfile)
  
  env$root <- file_path_sans_ext(basename(env$modfile))

  env$package <- ifelse(udll, rfile(model), new_model)
  
  env$compfile <- compfile(new_model)
  env$compbase <- compbase(new_model)
  env$compout <- compout(new_model)
  env$cachfile <- cachefile()
  env$model <- new_model
  env$stdout <- "build_exec_std_out"
  env$stderr <- "build_exec_std_err"
  env$recover <- recover
  env$cmd <- paste0(
    R.home(component="bin"),
    .Platform$file.sep,
    "R"
  )
  
  env$args <- c(
    "CMD", "SHLIB",
    ifelse(preclean, "--preclean ", ""),
    env$compfile
  )
  
  return(env)
  
}

rfile <- function(pattern="",tmpdir=normalizePath(getwd(),winslash="/")){
  basename(tempfile(pattern=so_stem(pattern),tmpdir='.'))
}

## A random file name
so_stem <- function(x) paste0(x,"-so-")

## Form a file name / path for the file that is actually compiled
comppart <- "-mread-source"

compdir <- function(loc,model) {
  compversion <- as.character(GLOBALS[["version"]])
  compplat <- paste0(
    c("mrgsolve","so",
      compversion,
      R.version$platform
    ),
    collapse="-"
  )
  file.path(loc,compplat,model)
}

compbase <- function(model) paste0(model, comppart)

compfile <- function(model) paste0(model, comppart,".cpp")

compout  <- function(model) paste0(model, comppart, .Platform$dynlib.ext)

cachefile <- function(model) "model-cache.RDS"

create_soloc <- function(loc,model,preclean) {
  
  soloc <- compdir(loc,model)
  
  if(preclean) unlink(soloc,recursive=TRUE)
  
  if(!file_exists(soloc)) dir.create(soloc,recursive=TRUE)
  
  return(soloc)
}

setup_soloc <- function(build_loc) {
  files <- c("mrgsolv.h", "modelheader.h")
  ans <- file.copy(mrgsolve_file("include", files), build_loc, overwrite = TRUE)
  if(!all(ans)) {
    stop("Failed to copy build headers to build directory.", call.=FALSE)  
  }
}

# nocov start
msub <- function(pattern,replacement,x,...) {
  sapply(
    x, 
    pattern = pattern, 
    replacement = replacement, 
    FUN = sub, 
    USE.NAMES=FALSE
  )
}

# mgsub <- function(pattern,replacement,x,...) {
#   sapply(
#     x, 
#     pattern = pattern, 
#     replacement = replacement, 
#     FUN = gsub,
#     USE.NAMES = FALSE
#   )
# }


build_exec <- function(build) {
  system4(
    build$cmd, 
    args = build$args,
    pattern = build$model,
    path = build$soloc
  )
}

build_output_cleanup <- function(x,build) {
  x[["stdout"]] <- strwrap(x[["stdout"]],width=60)
  errr <- x[["stderr"]]
  patt <- paste0("^", build[["compfile"]], ":")
  errr <- msub(pattern = patt, replacement = "", x = errr)
  patt <- "^ *In function .*void _model.*:$"
  errr <- msub(pattern = patt, replacement = "", x = errr)
  x[["stderr"]] <- errr
  x <- structure(x, class = c("mrgsolve-build-error", "list"))
  x
}

build_failed <- function(out,build,mod,spec,ignore.stdout) {
  out <- build_output_cleanup(out,build)
  if(isTRUE(build[["recover"]])) {
    warning("returning object for debugging purposes only.")
    build <- as.list(build)
    mod <- as.list(mod)
    mod[["envir"]] <- as.list(mod[["envir"]])
    mod[["shlib"]][["version"]] <- unlist(mod[["shlib"]][["version"]])
    mod[["shlib"]][["compiled"]] <- FALSE
    ans <- list(
      out = out,
      build = build, 
      mod = mod, 
      spec = spec
    )
    return(structure(ans, class = c("mrgsolve-build-recover", "list")))
  }
  if(!ignore.stdout) {
    msg <- divider_msg("stdout")
    cat("\n", msg, "\n", sep="")
    cat(out[["stdout"]],sep="\n")
  }
  header <- "\n---:: stderr ::---------------------"
  footer <- paste0(rep("-",nchar(header)),collapse = "")
  msg <- divider_msg("stderr")
  cat("\n",msg,"\n",sep="")
  xx <- paste0(out[["stderr"]],collapse="\n")
  message(xx,appendLF=FALSE)
  msg <- divider_msg()
  cat("\n",msg,"\n",sep="")
  build_handle_127(out)
  stop("the model build step failed.",call.=FALSE)
}

build_format_recover <- function(data, path = NULL) {
  if(!requireNamespace("yaml")) {
    stop("please install the yaml package to format recovery data", call.=FALSE)  
  }
  ans <- yaml::as.yaml(data)
  if(is.character(path)) {
    writeLines(text = ans, con = path)
    return(invisible(ans))
  }
  ans
}

build_handle_127 <- function(out) {
  found127 <- any(grepl("Error +127", out[["stderr"]]))
  if(found127) {
    msg <- readLines(system.file("msg", "error127.txt", package="mrgsolve"))
    null <- lapply(msg,message)
    cat(divider_msg(),"\n")
  }
  return(invisible(NULL))  
}

build_source_code <- function(x, cache = TRUE) {
  stopifnot(is.mrgmod(x))
  if(inherits(x, "packmod")) {
    stop("x is a packmod object")  
  }
  if(!dir.exists(soloc(x))) {
    stop("could not find build directory.")  
  }
  cwd <- getwd()
  on.exit(setwd(cwd))
  setwd(soloc(x))
  h <- list.files(pattern = "\\.h$")
  h <- lapply(h, readLines)
  names(h) <- paste0("h", seq_along(h))
  cpp <- list.files(pattern = "\\.cpp$")
  cpp <- lapply(cpp, readLines)
  names(cpp) <- paste0("cpp", seq_along(cpp))
  sources <- list(h = h, cpp = cpp)
  if(file.exists(rds <- "mrgmod_cache.RDS") && isTRUE(cache)) {
    sources$cache <- as.list(readRDS(rds))  
  }
  sources
}
# nocov end