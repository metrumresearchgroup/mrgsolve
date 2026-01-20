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



#' @export
#' @md
is.mrgsims_big <- function(x) {
  inherits(x, "mrgsims_big")  
}

#' @export
#' @md
mrgsim_big <- function(x,  ..., file = tempfile(), verbose = FALSE) {
  
  stopifnot(requireNamespace("arrow", quietly = TRUE))
  
  verbose <- isTRUE(verbose)
  
  if(verbose) message("Simulating.")
  out <- mrgsim(x, ...)
  
  if(verbose) message("Writing to parquet.")
  arrow::write_parquet(x = out@data, sink = file)
  
  if(verbose) message("Wrapping up.")
  ans <- list()

  ans$file <- file
  ans$mapped <- arrow::mmap_open(file)
  ans$reader <- arrow::ParquetFileReader$create(ans$mapped)
 
  ans$mod <- x
  ans$head <- out@data[seq(20), ]
  ans$request <- out@request
  ans$outnames <- out@outnames
  
  rm(out)
  
  class(ans) <- c("mrgsims_big", "list")
  
  ans
}

#' @export
#' @md
as_arrow_sims <- function(x, ...) {
  stopifnot(requireNamespace("arrow", quietly = TRUE))
  sims <- x$reader$ReadTable()
  sims
}

#' @export
#' @md
as_tibble_sims <- function(x, ...) {
  stopifnot(requireNamespace("arrow", quietly = TRUE))
  tibble::as_tibble(as_arrow_sims(x))  
}

#' @export
#' @md
print.mrgsims_big <- function(x, n = 8, ...) {
  file <- sub(tempdir(), "tempdir()", x$file)
  message("Model: ", x$mod@model)
  message("Dim  : ", x$reader$num_rows, " ", x$reader$num_columns)
  message("File : ", file)
  chunk <- head(x$head, n = n)
  rownames(chunk) <- paste0(seq(nrow(chunk)), ": ")
  print(chunk)
  return(invisible(NULL))
}

#' @export
#' @md
head.mrgsims_big <- function(x, ...) {
  head(x$head, ...)  
}

#' @export
#' @md
tail.mrgsims_big <- function(x, ...) {
  abort("there is no `tail()` method for `mrgsims_big` objects.") 
}

#' @export
#' @md
dim.mrgsims_big <- function(x) {
  c(x$reader$num_rows, x$reader$num_columns)  
}

#' @export
#' @md
plot.mrgsims_big <- function(x, y, ...) {
  abort("there is no `plot()` method for `mrgsims_big` objects.")
}
