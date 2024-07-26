.quotes <- function(x) paste0("\"", x, "\"")

tocode <- function(l) {
  if(!length(l)) return(character(0))
  sc <- sapply(l, function(x) is.character(x) && length(x)==1)
  l[sc] <- sapply(l[sc], .quotes)
  paste0(names(l), " = ", as.character(l))
}

# block name is $OMEGA or $SIGMA
mwrite_matrix <- function(x, block_name) {
  code <- character(0)
  for(i in seq_along(x$data)) {
    datai <-  x$data[[i]]
    labelsi <- x$labels[[i]]
    namei <- x$names[[i]]
    code <- c(code, block_name)
    header <- "@block"
    if(namei != "...") {
      header <- c(header, paste0("@name ", namei))  
    }
    if(any(labelsi != ".")) {
      labels_ <- paste0(labelsi, collapse = " ")
      header <- c(header, paste0("@labels ", labels_))
    } 
    code <- c(code, header)
    for(i in seq_along(datai)) {
      tag <- paste0("// row ", i)
      code <- c(code,  tag, datai[[i]])  
    }
    code <- c(code, "")
  }
  code
}

get_upper_tri <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)
  x <- x[upper.tri(x, diag = TRUE)]
  l <- vector(mode = "list", length = n)
  w <- 1
  for(i in seq(n)) {
    s <- seq(w, w+i-1)
    l[[i]] <- x[s]
    w <- w + i
  }
  names(l) <- paste0("row", seq(n))
  l
}

require_yaml <- function() {
  if(!requireNamespace("yaml", quietly = TRUE)) {
    abort("The package \"yaml\" is required.") # nocov
  } 
}

mwrite_model_to_list <- function(x) {
  l <- list()
  # Header
  l$source <- "mrgsolve::mwrite"
  l$mrgsolve <- as.character(packageVersion("mrgsolve"))
  l$format <- "list"
  l$version <- 1
  l$model <- x@model
  # Problem
  l$prob <- ""
  # Critical items
  l$param <- as.list(param(x))
  l$init <- as.list(init(x))
  l$capture <- character(0)
  if(length(x@capture)) {
    capture <- names(x@capture)
    new_names <- unname(x@capture)
    same_name <- capture == new_names
    l$capture <- paste0(new_names, " = ", capture)
    l$capture[same_name] <- capture[same_name]
  }
  # Omega and Sigma
  l$omega <- list()
  l$omega$data <- lapply(as.list(omat(x)), get_upper_tri)
  l$omega$labels <- labels(omat(x))
  l$omega$names <- names(omat(x))
  if(length(l$omega$data)) {
    names(l$omega$data) <- paste0("matrix", seq_along(l$omega$data))
    names(l$omega$labels) <- paste0("matrix",seq_along(l$omega$labels))
  }
  l$sigma <- list()
  l$sigma$data <- lapply(as.list(smat(x)), get_upper_tri)
  l$sigma$labels <- labels(smat(x))
  l$sigma$names <- names(smat(x))
  if(length(l$sigma$data)) {
    names(l$sigma$data) <- paste0("matrix", seq_along(l$sigma$data))
    names(l$sigma$labels) <- paste0("matrix",seq_along(l$sigma$labels))
  }
  # Other
  l$envir <- as.list(x@envir)
  l$plugin <- x@plugin
  # These items will get directly passed to update()
  l$update <- list()
  l$update$start <- x@start
  l$update$end <- x@end
  l$update$delta <- x@delta
  l$update$add <- x@add
  l$update$atol <- x@atol
  l$update$rtol <- x@rtol
  l$update$ss_atol <- x@ss_atol
  l$update$ss_rtol <- x@ss_rtol
  l$update$maxsteps <- x@maxsteps
  l$update$hmax <- x@hmax
  l$update$hmin <- x@hmin
  l$update$maxsteps <- x@maxsteps
  l$update$mxhnil <- x@mxhnil
  l$update$ixpr <- x@ixpr
  l$update$mindt <- x@mindt
  l$update$digits <- x@digits
  l$update$tscale <- x@tscale
  l$update$outvars <- unlist(outvars(x), use.names = FALSE)
  
  code <- gsub("\t", "  ", x@code, fixed = TRUE)
  code <- modelparse(code, comment_re = character(0))
  code <- lapply(code, trimws, which = "right")
  
  l$set <- tolist(code$SET)
  
  if(nrow(x@annot$data)) {
    annot <- x@annot$data
    annot$options <- NULL
    if(!requireNamespace("knitr", quietly = TRUE)) { 
      abort("The package \"knitr\" is required.") #nocov
    } 
    annot <- knitr::kable(annot, format = "simple")
    annot <- c("# Model Annotations: ", "", annot)
    if("PROB" %in% names(code)) {
      code$PROB <- c(code$PROB, "", annot)  
    } else {
      code <- c(list(PROB = annot), code)
    }
  }
  
  if("PROB" %in% names(code)) {
    l$prob <- code$PROB
    code$PROB <- NULL
  }
  
  clob <- c("PARAM", "INPUT", "THETA", "CMT", "INIT", "OMEGA", "SIGMA", 
            "NMEXT", "NMXML", "VCMT", "SET", "CAPTURE")
  
  w <- which(names(code) %in% clob)
  
  if(length(w)) {
    code <- code[-w]
  }
  
  # Need special handling here in case compartments are declared 
  # as a part of PKMODEL
  if("PKMODEL" %in% names(code)) {
    pk <- tolist(code$PKMODEL)
    parsed <- do.call(PKMODEL, pk)
    pk$ncmt <- parsed$n
    pk$cmt <- NULL
    code$PKMODEL <- tocode(pk)
  }
  # Put block names back into code
  code <- Map(code, names(code), f = function(text, name) {
    c(glue("${name}"), text, " ")
  })
  l$code <- unlist(code, use.names = FALSE)
  l
}

#' Write model code to yaml  format
#' 
#' Model code is written to a readable, transport format. This transport format
#' can be useful for (1) breaking connection to NONMEM modeling outputs that 
#' are imported by `$NMXML` or `$NMEXT` and (2) saving model updates (e.g., 
#' an updated parameter list). Models can be read back using [mread_yaml()] or 
#' converted to mrgsolve cpp format with [yaml_to_cpp()].
#' 
#' @param x a model object. 
#' @param file output file name; if non-character (e.g., `NULL`), no output 
#' will be written to file. 
#' @param digits precision to use when writing outputs. 
#' 
#' @details
#' Parameters and omega and sigma matrices that were imported via `$NMXML`
#' or `$NMEXT` will be written into the yaml file and the NONMEM import blocks 
#' will be dropped. This allows the user to load a model based on a NONMEM run 
#' without having a connection to that output (e.g., `root.xml` or `root.ext`). 
#' Given that the connection to the NONMEM modeling outputs is broken when 
#' writing to yaml, any update to the NONMEM run will only be propagated to 
#' the yaml file when `mwrite_yaml()` is run again. 
#' 
#' The yaml file does not currently have the ability to track 
#' other external dependencies, such as user-defined header files or other 
#' code that might be sourced in by the user when the model is loaded via 
#' [mread()]. NONMEM xml and ext files imported by `$NMXML` or `$NMEXT` are 
#' the _only_ external dependencies that are accounted for in the yaml 
#' transport file. 
#' 
#' @return 
#' A list containing data that was written out to the yaml file, with added 
#' item `file`, is returned invisibly. 
#' 
#' @examples
#' mod <- house()
#' 
#' temp1 <- tempfile(fileext = ".yaml")
#' 
#' x <- mwrite_yaml(mod, temp1)
#' 
#' readLines(temp1)
#' 
#' @seealso [mread_yaml()], [yaml_to_cpp()]
#' 
#' @md
#' @export
mwrite_yaml <- function(x, file, digits = 8) {
  require_yaml()
  l <- mwrite_model_to_list(x)
  if(is.character(file)) { 
    l$format <- "yaml" 
    yaml::write_yaml(l, file = file, precision = digits) 
  }
  l$file <- file
  invisible(l)
}

#' Write a model to native mrgsolve format
#' 
#' Model code is written to a file in native mrgsolve format. This 
#' can be useful for (1) breaking connection to NONMEM modeling outputs that 
#' are imported by `$NMXML` or `$NMEXT` and (2) saving model updates (e.g., 
#' an updated parameter list). Models can be read back using [mread()].
#' 
#' @inheritParams mwrite_yaml
#' @inheritParams yaml_to_cpp
#' 
#' @details
#' See important details in [mwrite_yaml()].
#' 
#' @return 
#' A list containing data that was written out to the cpp file, with added 
#' item `file`, is returned invisibly. 
#' 
#' @examples
#' temp <- tempfile(fileext = ".mod")
#' 
#' mod <- modlib("pk1", compile = FALSE)
#' 
#' x <- mwrite_cpp(mod, file = temp)
#' 
#' mod <- mread(x$file, compile = FALSE)
#' 
#' mod
#' 
#' @seealso [mwrite_yaml()], [yaml_to_cpp()]
#' 
#' @md
#' @export
mwrite_cpp <- function(x, file, update = TRUE) {
  l <- mwrite_model_to_list(x)
  temp <- tempfile()
  model <- basename(temp)
  project <- dirname(temp)
  l <- parsed_to_cppfile(l, model = model, project = project,  update = update)
  if(is.character(file)) {
    file.copy(l$cppfile, file, overwrite = TRUE)  
  }
  unlink(temp)
  l$cppfile <- NULL
  l$file <- file
  invisible(l)
}

#' Read a model from yaml format
#' 
#' Read back models written to file using [mwrite_yaml()]. Function 
#' `yaml_to_cpp()` is also provided to convert the yaml file to mrgsolve cpp 
#' file format. 
#' 
#' @param file the yaml file name.
#' @param model a new model name to use when calling `mread_yaml()`.
#' @param project the directory where the model should be built.
#' @param update `TRUE` if model settings should be written into the cpp file in 
#' a `$SET` block.
#' @param ... passed to [mread()].
#' 
#' @details
#' Note that `yaml_to_cpp()` by default writes model settings into the cpp 
#' file. `mread_yaml()` does not write model settings into the file but rather 
#' update the model object directly with data read back from the `yaml` file.
#' 
#' @examples
#' mod <- house()
#' 
#' temp <- tempfile(fileext = ".yaml")
#' 
#' mwrite_yaml(mod, file = temp)
#' 
#' # Note: this model is not compiled
#' mod <- mread_yaml(temp, model = "new-house", compile = FALSE)
#' mod
#' 
#' cppfile <- yaml_to_cpp(temp, project = tempdir())
#' 
#' readLines(cppfile)
#' 
#' @return 
#' A model object. 
#' 
#' @seealso [mwrite_yaml()]
#' 
#' @md
#' @export
mread_yaml <- function(file, model = basename(file), project = tempdir(),
                       update = FALSE, ...) {
  x <- mwrite_parse_yaml(file)
  parsed_to_model(x, model = model, project = project, update = update, ...)
}

mwrite_parse_yaml <- function(file) {
  require_yaml()
  l <- yaml::yaml.load_file(file)
  l <- mwrite_read_cleanup(l)
  if(!identical(l$source, "mrgsolve::mwrite")) {
    abort("the yaml source file was not written by `mwrite_yaml()`.")  
  }
  l
}

# Right after reading from yaml, there is usually a bunch of little oddities
# that need to be cleaned up so we can keep working in R
mwrite_read_cleanup <- function(x) {
  # This usually is rendered as an empty list, but needs to be numeric
  x$update$add <- as.numeric(x$update$add)
  if(length(x$omega$data)) {
    x$omega$labels <- lapply(x$omega$labels, as.character)
    x$omega$names <- lapply(x$omega$names, as.character)
  }
  if(length(x$sigma$data)) {
    x$sigma$labels <- lapply(x$sigma$labels, as.character)
    x$sigma$names <- lapply(x$sigma$names, as.character)
  }
  x
}

#' @rdname mread_yaml
#' @export
yaml_to_cpp <- function(file, model = basename(file), project = getwd(), 
                        update = TRUE) {
  x <- mwrite_parse_yaml(file)
  x <- parsed_to_cppfile(x, model, project, update)
  invisible(x$cppfile)
}

# Take in content parsed from yaml file, clean up, write to cpp file
# @return a cleaned-up version of x with slot for `cppfile` added 
parsed_to_cppfile <- function(x, model, project, update = FALSE) {
  prob <- character(0)
  if(sum(nchar(x$prob))) {
    prob <- c("$PROB", x$prob, "")  
  }
  param <- character(0)
  if(length(x$param)) {
    param <- c("$PARAM", tocode(x$param), "")
  }
  init <- character(0)
  if(length(x$init)) {
    init <- c("$INIT", tocode(x$init), "")
  }
  capture <- character(0)
  if(length(x$capture)) {
    capture <- c("$CAPTURE", x$capture, "")
  }
  omega <- mwrite_matrix(x$omega, "$OMEGA")
  sigma <- mwrite_matrix(x$sigma, "$SIGMA")
  
  code <- c(prob, param, init, omega, sigma, x$code, capture)
  
  set <- tocode(x$set)
  if(isTRUE(update)) {
    if(length(set)) {
      set <- c(set, " ")
    }
    set <- c(set, tocode(x$update))
  }
  if(length(set)) {
    set <- c("$SET", set, "")
    code <- c(code, set)
  }
  
  cppfile <- file.path(project, paste0(model, ".mod"))
  writeLines(con = cppfile, code) 
  x$cppfile <- cppfile
  x
}

# Take in parsed content from yaml file
# Write to cpp file 
# mread back in and update
# @param x model object
# @param model a new model name
# @param project where to build the model; defaults to tempdir()
parsed_to_model <- function(x, model, project, update, ...) {
  x <- parsed_to_cppfile(x, model = model, project = project, update = update)
  mod <- mread(x$cppfile, ...)
  # If we want dynamic capture, force that into outvars
  mread_args <- list(...)
  if("capture" %in% names(mread_args)) {
    x$update$outvars <- c(x$update$outvars, mread_args$capture) 
  }
  update(mod, data = x$update)
}
