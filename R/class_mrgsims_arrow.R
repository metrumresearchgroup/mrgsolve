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

setClass("mrgsimsds")

require_arrow <- function() {
  if(!requireNamespace("arrow", quitely = TRUE)) {
    abort("this function requires the arrow package to be installed.")  
  }
}

#' @export
#' @md
is.mrgsimsds <- function(x) {
  inherits(x, "mrgsimsds")  
}

#' @export
#' @md
mrgsim_ds <- function(x,  ..., file = tempfile(), verbose = FALSE) {
  
  require_arrow()
  
  verbose <- isTRUE(verbose)
  
  if(verbose) message("Simulating.")
  out <- mrgsim(x, ...)
  
  if(verbose) message("Writing to parquet.")
  arrow::write_parquet(x = out@data, sink = file)
  
  if(verbose) message("Wrapping up.")
  ans <- list()

  ans$file <- file
  ans$ds <- arrow::open_dataset(file)
  ans$mod <- x
  ans$head <- out@data[seq(20), ]
  ans$names <- names(ans$head)
  
  rm(out)
  
  class(ans) <- c("mrgsimsds", "list")
  
  ans
}

#' @export
#' @md
as_arrow_sims <- function(x, ...) {
  require_arrow()
  as_arrow_table(x$ds)
}

#' @export
#' @md
as_tibble_sims <- function(x, ...) {
  tibble::as_tibble(as_arrow_sims(x))  
}

#' @export
#' @md
print.mrgsimsds <- function(x, n = 8, ...) {
  file <- sub(tempdir(), "tempdir()", x$file)
  dm <- dim(x$ds)
  message("Model: ", x$mod@model)
  message("Dim  : ", dm[1L], " ", dm[2L])
  message("File : ", file)
  chunk <- head(x$head, n = n)
  rownames(chunk) <- paste0(seq(nrow(chunk)), ": ")
  print(chunk)
  return(invisible(NULL))
}

#' Return the first several rows of the object. 
#' 
#' @param x a object with class `mrgsimsds`.
#' @param n number of rows to show.  
#' @param ... passed to [head()].
#' @export
#' @md
setMethod("head", "mrgsimsds", function(x, n = 6L, ...) {
  head(x$head, ...)
})

#' @export
#' @md
setMethod("tail", "mrgsimsds", function(x,...) {
  abort("there is no `tail()` method for this object (mrgsimsds).") 
})

#' 
#' @export
#' @md
dim.mrgsimsds <- function(x) {
  dim(x$ds)
}

#' @export
#' @md
nrow.mrgsimsds <- function(x) {
  nrow(x$ds)
}

#' @export
#' @md
ncol.mrgsimsds <- function(x) {
  ncol(x$ds)
}

#' @export
#' @md
setMethod("plot", "mrgsimsds", function(x,...) {
  abort("there is no `plot()` method for this object (mrgsimsds).")
})
