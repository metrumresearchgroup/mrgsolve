tocode <- function(l) {
  paste0(names(l), " = ", as.character(l))
}

get_upper_tri <- function(x) {
  x <- as.matrix(x)
  x[upper.tri(x, diag = TRUE)]
}

mwrite_model_to_list <- function(x) {
  l <- list()
  # Header
  l$format <- "list"
  l$mrgsolve <- as.character(packageVersion("mrgsolve"))
  l$transport <- 1
  l$model <- x@model
  # Problem
  l$prob <- ""
  # Critical items
  l$param <- as.list(param(x))
  l$init <- as.list(init(x))
  # Omega and Sigma
  l$omega <- list()
  l$omega$data <- lapply(as.list(omat(x)), get_upper_tri)
  l$omega$labels <- labels(omat(x))
  names(l$omega$labels) <- seq_along(l$omega$labels)
  l$omega$names <- names(omat(x))
  l$sigma$data <- lapply(as.list(smat(x)), get_upper_tri)
  l$sigma$labels <- labels(smat(x))
  names(l$sigma$labels) <- seq_along(l$sigma$labels)
  l$sigma$names <- names(smat(x))
  # Other
  l$env <- as.list(x@envir)
  l$plugin <- x@plugin
  l$capture <- x@capture
  # These items will get directly passed to update()
  l$set <- list()
  l$set$start <- x@start
  l$set$end <- x@end
  l$set$delta <- x@delta
  l$set$add <- x@add
  l$set$atol <- x@atol
  l$set$rtol <- x@rtol
  l$set$ss_atol <- x@ss_atol
  l$set$ss_rtol <- x@ss_rtol
  l$set$maxsteps <- x@maxsteps
  l$set$hmax <- x@hmax
  l$set$hmin <- x@hmin
  l$set$maxsteps <- x@maxsteps
  l$set$mxhnil <- x@mxhnil
  l$set$ixpr <- x@ixpr
  l$set$digits <- x@digits
  l$set$tscale <- x@tscale
  l$set$outvars <- unlist(outvars(x), use.names = FALSE)
  
  code <- gsub("\\t", "  ", x@code, perl = TRUE)
  code <- modelparse(code, comment_re = character(0))
  code <- lapply(code, trimws, which = "right")
  
  if(nrow(x@annot$data)) {
    annot <- x@annot$data
    annot$options <- NULL
    if(!requireNamespace("knitr", quietly = TRUE)) {
      abort("The package \"knitr\" is required.")
    }
    annot <- knitr::kable(annot, format = "simple")
    annot <- c("# Annotations: ", "", annot)
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
            "NMEXT", "NMXML", "VCMT")
  for(block in clob) {
    while(block %in% names(code)) {
      code[[block]] <- NULL
    }
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
  
  code <- Map(code, names(code), f = function(text, name) {
    c(glue("${name}"), text, " ")
  })
  
  l$code <- unlist(code, use.names = FALSE)
  
  l
}

#' Write model code to yaml or json format
#' 
#' Model code is written to a readable, transport format. Models can 
#' be read back using [mread_yaml()] or [mread_json()] or converted to 
#' mrgsolve cpp format with [yaml_to_cpp()] or [json_to_cpp()].
#' 
#' @param x a model object. 
#' @param file output file name. 
#' @param digits precision to use when writing outputs. 
#' 
#' @return 
#' A list containing data that was written out to the yaml or json file. 
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
#' temp2 <- tempfile(fileext = ".json")
#' 
#' y <- mwrite_json(mod, temp2)
#' 
#' code <- readLines(temp2) 
#' 
#' @md
#' @name mwrite
#' @export
mwrite_yaml <- function(x, file = NULL, digits = 8) {
  if(!requireNamespace("yaml", quietly = TRUE)) {
    abort("The package \"yaml\" is required.")
  }
  l <- mwrite_model_to_list(x)
  l$format <- "yaml"
  out <- yaml::as.yaml(l, precision = digits)
  if(is.character(file)) {
    writeLines(con = file, out)  
  }
  l$format <- "list"
  l
}

#' @rdname mwrite
#' @export
mwrite_json <- function(x, file = NULL, digits = 8) {
  if(!requireNamespace("jsonlite", quietly = TRUE)) {
    abort("The package \"jsonlite\" is required.")
  }  
  l <- mwrite_model_to_list(x)
  l$format <- "json"
  out <- jsonlite::toJSON(
    l, 
    digits = digits, 
    pretty = TRUE
  )
  if(is.character(file)) {
    writeLines(con = file, out)  
  }
  l$format <- "list"
  l
}

#' Read a model from yaml or json format
#' 
#' Read back models written to file using [mwrite_yaml()] or [mwrite_json()].
#' Functions `yaml_to_cpp()` and `json_to_cpp()` are also provided to convert
#' the yaml or json file to mrgsolve cpp file format. 
#' 
#' @param file the yaml or json file name.
#' @param model a new model name to use when calling `mread_yaml` or 
#' `mread_json`.
#' @param project the directory where the model should be built.
#' @param set `TRUE` if model setting should be written into the cpp file in a
#' `$SET` block.
#' @param ... passed to [mread()].
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
#' cppfile <- yaml_to_cpp(temp)
#' 
#' readLines(cppfile)
#' 
#' @details
#' Note that `yaml_to_cpp()` and `json_to_cpp()` by default write model settings
#' into the cpp file. `mread_yaml()` and `mread_json()` do not write model 
#' settings into the file but rather update the model object directly with data
#' read back from the `yaml` or `json` file.
#' 
#' @return 
#' A model object. 
#' 
#' @md
#' @export
mread_yaml <- function(file, model = basename(file), project = tempdir(), ...) {
  x <- parse_yaml(file)
  parsed_to_model(x, model, project, ...)
}

#' @rdname mread_yaml
#' @export
mread_json <- function(file, model = basename(file), project = tempdir(), ...) {
  x <- parse_json(file)
  parsed_to_model(x, model, project, ...)
}

parse_yaml <- function(file) {
  if(!requireNamespace("yaml", quietly = TRUE)) {
    abort("The package \"yaml\" is required.")
  }
  text <- readLines(file)
  yaml::yaml.load(text)
}

parse_json <- function(file) {
  if(!requireNamespace("jsonlite", quietly = TRUE)) {
    abort("The package \"jsonlite\" is required.")
  }
  text <- readLines(file)  
  jsonlite::fromJSON(text)
}

#' @rdname mread_yaml
#' @export
yaml_to_cpp <- function(file, model = basename(file), project = getwd(), 
                        set = TRUE) {
  x <- parse_yaml(file)
  x <- parsed_to_cppfile(x, model, project, set)
  invisible(x$cppfile)
}

#' @rdname mread_yaml
#' @export
json_to_cpp <- function(file, model = basename(file), project = getwd(), 
                        set = TRUE) {
  x <- parse_json(file)
  x <- parsed_to_cppfile(x, model, project, set)
  invisible(x$cppfile)
}

# Take in content parsed from yaml or json file, clean up, write to cpp file
# @return a cleaned-up version of x with `cppfile` slot added
parsed_to_cppfile <- function(x, model, project, set = FALSE) {
  prob <- NULL
  if(sum(nchar(x$prob))) {
    prob <- c("$PROB", x$prob, "")  
  }
  
  param <- c("$PARAM", tocode(x$param), "")
  init <- c("$INIT", tocode(x$init), "")
  
  x$set$add <- as.numeric(x$set$add)
  x$capture <- as.character(x$capture)
  x$omega$labels <- lapply(x$omega$labels, as.character)
  x$omega$names <- lapply(x$omega$names, as.character)
  
  omega <- c()
  for(i in seq_along(x$omega$data)) {
    header <- "@block"
    if(any(x$omega$labels[[i]] != "...")) {
      o_labels <- paste0(x$omega$labels[[i]], collapse = " ")
      header <- c(header, paste0("@labels ", o_labels))
    }
    omega <- c(omega, "$OMEGA", header, x$omega$data[[i]], "")  
  }
  
  x$sigma$labels <- lapply(x$sigma$labels, as.character)
  x$sigma$names <- lapply(x$sigma$names, as.character)
  
  sigma <- c()
  for(i in seq_along(x$sigma$data)) {
    header <- "@block"
    if(any(x$sigma$labels[[i]] != "...")) {
      s_labels <- paste0(x$sigma$labels[[i]], collapse = " ")
      header <- c(header, paste0("@labels ", s_labels))
    }
    sigma <- c(sigma, "$SIGMA", header, x$sigma$data[[i]], "")  
  }
  
  code <- c(prob, param, init, omega, sigma, x$code)
  
  if(isTRUE(set)) {
    set <- c("$SET", tocode(x$set), "")
    code <- c(code, set)
  }
  
  cppfile <- file.path(project, paste0(model, ".mod"))
  writeLines(con = cppfile, code) 
  x$cppfile <- cppfile
  x
}

# Take in parsed content from yaml or json file
# Write to cpp file 
# mread back in and update
# @param x model object
# @param model a new model name
# @param project where to build the model; defaults to tempdir()
parsed_to_model <- function(x, model, project, ...) {
  x <- parsed_to_cppfile(x, model, project)
  mod <- mread(x$cppfile, ...)
  # If we want dynamic capture, force that into outvars
  mread_args <- list(...)
  if("capture" %in% names(mread_args)) {
    x$set$outvars <- c(x$set$outvars, mread_args$capture) 
  }
  update(mod, data = x$set)
}
