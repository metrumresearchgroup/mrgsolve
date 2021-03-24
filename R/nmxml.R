# Copyright (C) 2013 - 2019  Metrum Research Group, LLC
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


##' Import model estimates from a NONMEM xml file
##'
##' @param run run number
##' @param project project directory
##' @param path the complete path to the \code{run.xml} file
##' @param file deprecated; use \code{path} instead
##' @param root the directory that `path` and `project` are relative to; this is
##' currently limited to the `working` directory or `cppdir`, the directory 
##' where the model file is located
##' @param theta logical; if TRUE, the \code{$THETA} vector is returned
##' @param omega logical; if TRUE, the \code{$OMEGA} matrix is returned
##' @param sigma logical; if TRUE, the \code{$SIGMA} matrix is returned
##' @param olabels labels for \code{$OMEGA}
##' @param slabels labels for \code{$SIGMA}
##' @param oprefix prefix for \code{$OMEGA} labels
##' @param sprefix prefix for \code{$SIGMA} labels
##' @param tname name for \code{$THETA}
##' @param oname name for \code{$OMEGA}
##' @param sname name for \code{$SIGMA}
##' @param index the estimation number to return;  "last" will return the 
##' last estimation results; otherwise, pass an integer indicating which 
##' estimation results to return
##' @param xpath xml path containing run results; if the default doesn't work, 
##' consider using \code{.//estimation} as an alternative; see details
##' @param env internal
##' @aliases NMXML
##' @details
##' If \code{run} and \code{project} are supplied, the .xml file is 
##' assumed to be located in \code{run.xml}, in directory \code{run} 
##' off the \code{project} directory.  If \code{file} is supplied, 
##' \code{run} and \code{project} arguments are ignored.
##' 
##' This function requires that the \code{xml2} package 
##' be installed and loadable.  If \code{requireNamespace("xml2")}
##' fails, an error will be generated. 
##' 
##' \code{nmxml} usually expects to find run results in the xpath called
##' \code{.//nm:estimation}.  Occasionally, the run results are not stored in 
##' this namespace but no namespaces are found in the xml file.  In this case, 
##' the user can specify the xpath containing run results.  Consider trying 
##' \code{.//estimation} as an alternative if the default fails. 
##' 
##' @return A list with theta, omega and sigma elements, 
##' depending on what was requested
##'  
##' @seealso nmext
##'  
##' @examples
##' 
##' if(requireNamespace("xml2")) {
##'   proj <- system.file("nonmem", package = "mrgsolve")
##'   mrgsolve:::nmxml(run = 1005, project = proj)
##' }
##' 
nmxml <- function(run = numeric(0), project = character(0),
                  file = character(0), path = character(0),
                  root = c("working", "cppfile"), 
                  theta = TRUE, omega = TRUE, sigma = TRUE,
                  olabels = NULL, slabels = NULL,
                  oprefix = "", sprefix="",
                  tname = "THETA", oname = "...", sname = "...",
                  index = "last", xpath = ".//nm:estimation", env = NULL) {
  
  
  if(!missing(file)) {
    lifecycle::deprecate_soft(
      "0.10.2", 
      "mrgsolve::nmxml(file = )", 
      "nmxml(path = )"
    )
    path <- file
  }
  
  root <- match.arg(root)
  if(root == "cppfile" & !is.null(env)) {
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(env[["project"]])
  }
  
  if(!missing(path)) {
    target <- path
  } else {
    if(missing(run) | missing(project)) {
      wstop("both file and run or project are missing")
    }
    target <- file.path(project, run, paste0(run, ".xml"))
  }

  if(!requireNamespace("xml2")) {
    stop("could not load namespace for package xml2.", call. = FALSE)
  }
  tree <- xml2::read_xml(target)
  tree <- try(xml2::xml_find_all(tree,xpath))
  if(inherits(tree, "try-error")) {
    msg <- sprintf(
      c("failed to parse xml from nonmem run with xpath", 
        " %s,\n  See the xpath argument under ?nmxml for help."), 
      xpath
    )
    stop(msg, call.=FALSE) 
  }
  
  tree <- xml2::as_list(tree)
  
  if(length(tree)==0) {
    stop("could not recover any data from the xml file", call.=FALSE)
  }
  
  if(index=="last") index <- length(tree)
  
  if(!is.numeric(index)) {
    stop("nmxml: index must be 'last' or a numeric value.",call.=FALSE)  
  }
  if(index > length(tree)) {
    stop("nmxml: index is out of bounds.",call.=FALSE)
  }
  
  tree <- tree[[index]]
  
  theta <- theta | !missing(tname)
  omega <- omega | !missing(oname)
  sigma <- sigma | !missing(sname)
  
  th <- list()
  om <- matrix(0,0,0)
  sg <- matrix(0,0,0)
  
  if(theta) {
    stopifnot(nchar(tname) > 0)
    th <- sapply(tree$theta, "[", USE.NAMES=FALSE)
    th <- as.list(as.numeric(th))
    
    if(length(tname) > 1) {
      if(length(tname) != length(th)) {
        wstop(
          "'tname' length (", 
          length(tname), 
          ") is not equal to ",
          "number of THETAs (", 
          length(th), 
          ")"
        )
      }
    }
    
    if(length(th) == length(tname)) {
      names(th) <- tname
    } else {
      names(th) <- paste0(tname, seq(length(th)))
    }
  }
  
  if(omega) {
    stopifnot(nchar(oname) > 0)
    om <- nm_xml_matrix(tree$omega)
    if(is.null(olabels)) {
      olabels <- rep('.', nrow(om))
    } else {
      olabels <- paste0(oprefix,olabels)
    }
    olabels <- list(olabels)
  } else {
    olabels <- list()
  }
  
  if(sigma) {
    stopifnot(nchar(sname) > 0)
    sg <- nm_xml_matrix(tree$sigma)
    if(is.null(slabels)) {
      slabels <- rep('.', nrow(sg))
    } else {
      slabels <- paste0(sprefix,slabels)
    }
    slabels <- list(slabels)
  } else {
    slabels <- list()
  }
  
  om <- create_matlist(
    setNames(list(om),oname), 
    labels=olabels, 
    class="omegalist"
  )
  
  sg <- create_matlist(
    setNames(list(sg),sname), 
    labels=slabels, 
    class="sigmalist"
  )
  
  ans <- list(theta = th, omega = om, sigma = sg, file = target)
  
  return(structure(ans, class = "NMXMLDATA"))
  
}

#' Import model estimates from a NONMEM ext file
#' 
#' @inheritParams nmxml
#' @param path full path to NONMEM `ext` file
#' @param read_fun function to use when reading the `ext` file
#' 
#' @seealso [nmxml], [read_nmext]
#' 
#' @md
nmext <- function(run = NA_real_, project = getwd(), 
                  file = paste0(run,".ext"), path = NULL,
                  root = c("working", "cppfile"),
                  index = "last",
                  theta = TRUE, omega = TRUE, sigma = TRUE,
                  olabels = NULL, slabels = NULL,
                  oprefix = "", sprefix = "",
                  tname = "THETA", oname = "...", sname = "...", 
                  read_fun = "data.table", env = NULL) {
  
  if(missing(run) && !is.character(path)) {
    wstop("either 'run' or 'path' argument must be specified")  
  }
  
  root <- match.arg(root)
  if(root == "cppfile" & !is.null(env)) {
    cwd <- getwd()
    on.exit(setwd(cwd))
    setwd(env[["project"]])
  }
  
  ans <- read_nmext(
    run = run,
    project = project,
    file = file,
    path = path,
    read_fun = read_fun,
    index = index
  )
  
  theta <- theta | !missing(tname)
  omega <- omega | !missing(oname)
  sigma <- sigma | !missing(sname)
  
  th <- list()
  om <- matrix(0,0,0)
  sg <- matrix(0,0,0)
  
  if(theta) {
    th <- ans[["param"]]
    if(tname!="THETA") {
      names(th) <- sub("THETA", tname, names(th), fixed = TRUE)  
    }
  }
  
  if(omega) {
    stopifnot(nchar(oname) > 0)
    om <- ans[["omega"]]
    if(is.null(olabels)) {
      olabels <- rep('.', nrow(om))
    } else {
      olabels <- paste0(oprefix,olabels)
    }
    olabels <- list(olabels)
  } else {
    olabels <- list()
  }
  
  if(sigma) {
    stopifnot(nchar(sname) > 0)
    sg <- ans[["sigma"]]
    if(is.null(slabels)) {
      slabels <- rep('.', nrow(sg))
    } else {
      slabels <- paste0(sprefix,slabels)
    }
    slabels <- list(slabels)
  } else {
    slabels <- list()
  }
  
  om <- create_matlist(
    setNames(list(om), oname), 
    labels = olabels, 
    class = "omegalist"
  )
  
  sg <- create_matlist(
    setNames(list(sg), sname), 
    labels = slabels, 
    class = "sigmalist"
  )
  
  file <- attributes(ans)[["file"]]
  ans <- list(theta = th, omega = om, sigma = sg, file = file)
  
  return(structure(ans,class="NMXMLDATA"))
}

nm_xml_matrix <- function(x) {
  m <- matrix(0,nrow = length(x), ncol = length(x))
  for(i in seq(x)) {
    row <- x[[i]]
    ri <- as.integer(attr(row, "rname"))
    for(j in seq(i)) {
      ci <- as.integer(attr(row[[j]], "cname"))
      m[ri,ci] <- m[ci,ri] <- as.numeric(row[[j]][[1]])
    }
  } 
  m
}

##' Extract estimates from NONMEM ext file
##' 
##' 
##' @param run a run number or run identifier
##' @param project the NONMEM project directory
##' @param file the `ext` file name
##' @param path full path and file name for `ext` file
##' @param read_fun function to read the `ext` file
##' @param index selects the table number whose results will be returned;
##' use value "last" to select the last table in the `.ext` file; or pass an 
##' integer specifying the table number; in case there is exactly
##' one table in the `.ext` file, pass the value "single" to bypass parsing 
##' the file to look for sub tables (this might be useful when BAYES analysis 
##' was performed as the only estimation method and there are 10000s of 
##' posterior samples in the file)
##' 
##' @return A list with param, omega, and sigma in a format ready to be used to 
##' update a model object.
##' 
##' @examples
##' project <- system.file("nonmem", package = "mrgsolve")
##' 
##' est <- read_nmext(1005, project = project)
##' 
##' est$param
##' 
##' est$omega
##' 
##' est$sigma
##' 
##' est <- read_nmext(2005, project = project, index = 3)
##' 
##' @md
##' @export 
read_nmext <- function(run = NA_real_, 
                       project = getwd(), 
                       file = paste0(run,".ext"), 
                       path = NULL, 
                       read_fun = c("data.table","read.table"), 
                       index = "last") {
  
  if(is.character(path)) {
    extfile <- path  
  } else {
    extfile <- file.path(project,run,file)
  }
  
  if(!file.exists(extfile)) {
    wstop("[read_nmext] could not find the requested 'ext' file ", 
          shQuote(basename(extfile)))
  }
  
  read_fun <- match.arg(read_fun)
  
  use_dt <- requireNamespace("data.table",quietly=TRUE) & read_fun=="data.table"
  
  all_rows <- FALSE
  if(index == "single") {
    index <- 1
    all_rows <- TRUE
  }
  
  m <- map_ext_file(extfile, all_rows = all_rows)
  
  if(index == "last") index <- length(m)
  if(!is.numeric(index)) wstop("index did not resolve to a numeric value")
  if(index < 1) wstop("index must resolve to a value that is at least 1")
  if(index > length(m)) {
    msg <- c(glue("[read_nmext] table {index} was requested, "), 
             glue("but only {length(m)} tables were found in the ext file"))
    stop(msg,call.=FALSE)
  }
  m <- m[[index]]
  
  if(use_dt) {
    df <- data.table::fread(
      file = extfile, 
      na.strings = '.', 
      data.table = FALSE,
      skip = m$skip, 
      nrows = m$nrows
    )
  } else {
    df <- read.table(
      file = extfile,
      na.strings = '.',
      stringsAsFactors = FALSE,
      skip = m$skip,
      nrows = m$nrows,
      header = TRUE
    )
  }
  
  ans <- df[df[["ITERATION"]] == -1E9,]
  
  if(nrow(ans)==0) {
    wstop(
      "[read_nmext] could not find final estimates",
      " while reading 'ext' file ", shQuote(basename(extfile))
    )
  }
  
  ans <- as.list(ans)
  names(ans) <- gsub("[[:punct:]]", "", names(ans))
  ans <- list(
    raw = ans,
    param = ans[grepl("THETA", names(ans))],
    omega = as_bmat(ans, "OMEGA"), 
    sigma = as_bmat(ans, "SIGMA")
  )
  return(structure(ans, table = m$table, index = index, file = extfile))
}

map_ext_file <- function(file, all_rows = FALSE) {
  if(isTRUE(all_rows)) {
    ans <- list(list(skip = 1, nrows = Inf, table = "single table"))
    return(ans)
  }
  x <- readLines(file, warn = FALSE)
  start <- which(substr(x, 1, 5) == "TABLE")
  if(length(start) == 1) {
    ans <- list(list(skip = 1, nrows = Inf, table = x[start[1]]))
    return(ans)
  }
  end <- c(start[-1]-1,length(x))
  ans <- vector(mode = "list", length = length(start))
  for(i in seq_along(start)) {
    ans[[i]] <- list(
      skip  = start[i], 
      nrows = end[i] - start[i] - 1, 
      table = x[start[i]]
    )
  }
  ans
}
