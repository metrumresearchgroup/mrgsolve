sim_file <- function(model, path) {
  e <- new.env(parent = emptyenv())
  e$path <- path
  reg.finalizer(e, function(e) {
    unlink(e$path, recursive = TRUE)
  }, onexit = FALSE)
  structure(path, class = "mrgsims_arrow", env = e)
}

#' @export
print.mrgsims_arrow <- function(x, ...) {
  print(as.character(x))
}




