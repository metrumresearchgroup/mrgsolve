
#' @export
mod_transport <- function(x, file = NULL, 
                          format = c("yaml", "json"), 
                          digits = 8) {
  
  format <- match.arg(format)
  
  l <- list()
  l$format <- "text"
  l$mrgsolve <- as.character(packageVersion("mrgsolve"))
  l$xptversion <- 1
  l$model <- x@model
  l$param <- as.list(param(x))
  l$cmt <- as.list(init(x))
  l$start <- x@start
  l$end <- x@end
  l$delta <- x@delta
  l$add <- x@add
  l$env <- as.list(x@envir)
  l$atol <- x@atol
  l$rtol <- x@rtol
  l$ss_atol <- x@ss_atol
  l$ss_rtol <- x@ss_rtol
  l$maxsteps <- x@maxsteps
  l$hmax <- x@hmax
  l$hmin <- x@hmin
  l$maxsteps <- x@maxsteps
  l$mxhnil <- x@mxhnil
  l$plugin <- x@plugin
  l$capture <- x@capture
  
  get_lower <- function(x) {
    x <- as.matrix(x)
    x[lower.tri(x, diag = TRUE)]
  }
  
  l$omega <- list()
  l$omega$data <- lapply(as.list(omat(x)), get_lower)
  l$omega$labels <- labels(omat(x))
  l$omega$names <- names(omat(x))
  l$sigma$data <- lapply(as.list(smat(x)), get_lower)
  l$sigma$labels <- labels(smat(x))
  l$sigma$names <- names(smat(x))
  
  code <- gsub("\\t", "  ", x@code, perl = TRUE)
  code <- modelparse(code)
  code <- lapply(code, trimws, which = "right")
  
  clob <- c("PARAM", "INPUT", "THETA", "CMT", "INIT", 
            "OMEGA", "SIGMA", "NMEXT", "NMXML", "VCMT")
  for(block in clob) {
    while(block %in% names(code)) {
      code[[block]] <- NULL
    }
  }
  
  if("PKMODEL" %in% names(code)) {
    tocode <- function(l) {
      paste0(names(l), " = ", unlist(l, use.names=FALSE))
    }
    pk <- tolist(code$PKMODEL)
    parsed <- do.call(PKMODEL, pk)
    pk$ncmt <- parsed$n
    pk$cmt <- NULL
    code$PKMODEL <- tocode(pk)
  }
  
  code <- Map(code, names(code), f = function(text, name) {
    c(glue("${name}"), text)
  })
  l$code <- unlist(code, use.names = FALSE)
  if(is.character(file)) {
    if(format=="yaml") {
      l$format <- "yaml"
      out <- yaml::as.yaml(l, precision=digits)
    } else {
      l$format <- "json"
      out <- jsonlite::toJSON(l, digits=digits, pretty = TRUE)
    }
    writeLines(con = file, out)
  }
  l
}
