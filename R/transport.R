tocode <- function(l) {
  paste0(names(l), " = ", unlist(l, use.names=FALSE))
}

get_upper_tri <- function(x) {
  x <- as.matrix(x)
  x[upper.tri(x, diag = TRUE)]
}

#' @export
mod_transport <- function(x, file = NULL, format = c("yaml", "json"), 
                          digits = 8) {
  
  format <- match.arg(format)
  
  l <- list()
  # Header
  l$format <- "text"
  l$mrgsolve <- as.character(packageVersion("mrgsolve"))
  l$transport <- 1
  l$model <- x@model
  # Critical items
  l$param <- as.list(param(x))
  l$init <- as.list(init(x))
  # Omega and Sigma
  l$omega <- list()
  l$omega$data <- lapply(as.list(omat(x)), get_upper_tri)
  l$omega$labels <- labels(omat(x))
  l$omega$names <- names(omat(x))
  l$sigma$data <- lapply(as.list(smat(x)), get_upper_tri)
  l$sigma$labels <- labels(smat(x))
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
  
  code <- gsub("\\t", "  ", x@code, perl = TRUE)
  code <- modelparse(code, comment_re = character(0))
  code <- lapply(code, trimws, which = "right")
  
  if(nrow(x@annot$data)) {
    annot <- x@annot$data
    annot$options <- NULL
    if(!requireNamespace("knitr")) {
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
  
  clob <- c("PARAM", "INPUT", "THETA", "CMT", "INIT", "OMEGA", "SIGMA", 
            "NMEXT", "NMXML", "VCMT")
  for(block in clob) {
    while(block %in% names(code)) {
      code[[block]] <- NULL
    }
  }
  
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
  
  if(is.character(file)) {
    if(format=="yaml") {
      if(!requireNamespace("yaml")) {
        abort("The package \"yaml\" is required.")
      }
      l$format <- "yaml"
      out <- yaml::as.yaml(l, precision = digits)
    } else {
      l$format <- "json"
      if(!requireNamespace("jsonlite")) {
        abort("The package \"jsonlite\" is required.")
      }
      out <- jsonlite::toJSON(l, digits = digits, pretty = TRUE)
    }
    writeLines(con = file, out)
  }
  out
}

#' @export
mread_xport <- function(file, format = c("yaml", "json"),
                        project = tempdir(), ...) {
  format <- match.arg(format)
  text <- readLines(file)
  if(format=="yaml") {
    if(!requireNamespace("yaml")) {
      abort("The package \"yaml\" is required.")
    }
    x <- yaml.load(text)
  } else {
    if(!requireNamespace("jsonlite")) {
      abort("The package \"jsonlite\" is required.")
    }
    x <- jsonlite::fromJSON(text)  
  }
  
  x$set$add <- as.numeric(x$set$add)
  x$capture <- as.character(x$capture)
  
  param <- c("$PARAM", tocode(x$param), "")
  
  init <- c("$INIT", tocode(x$init), "")
  
  omega <- sigma <- c()
  for(i in seq_along(x$omega$data)) {
    header <- "@block"
    if(any(x$omega$labels[[i]] != "...")) {
      header <- c(header, paste0("@labels ", paste0(x$omega$labels[[i]], collapse = " ")))    
    }
    omega <- c(omega, "$OMEGA", header, x$omega$data[[i]], "")  
  }
  
  for(i in seq_along(x$sigma$data)) {
    header <- "@block"
    if(any(x$sigma$labels[[i]] != "...")) {
      header <- c(header, paste0("@labels ", paste0(x$sigma$labels[[i]], collapse = " ")))    
    }
    sigma <- c(sigma, "$SIGMA", header, x$sigma$data[[i]], "")  
  }
  
  code <- c(param, init, omega, sigma, x$code)
  modelfile <- file.path(project, paste0(x$model, "-xport.mod"))
  writeLines(con = modelfile, code)
  mod <- mread(modelfile, ...)
  update(mod, data = x$set)
}
