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

#' Complete nmtran-like data item columns
#' 
#' Add common nmtran-like data item columns to a data frame using conservative
#' default values. Existing columns are not modified, and row order is
#' preserved.
#' 
#' @param data a data frame.
#' @param ... not used; for future extensions.
#' @param case output case for columns that are added; use `"preserve"` to
#' infer from existing nmtran-like names in `data`.
#' @param id if `TRUE`, add `ID` / `id` when missing.
#' 
#' @details
#' This function does not validate whether the resulting data set is
#' scientifically meaningful for simulation; it only fills absent standard
#' columns with default values.
#' 
#' Default values for added columns are:
#' 
#' - `ID` / `id`: `1` (only when `id = TRUE`)
#' - `TIME` / `time`: `0`
#' - `AMT` / `amt`: `0`
#' - `CMT` / `cmt`: `1`
#' - `EVID` / `evid`: `0`
#' - `RATE` / `rate`: `0`
#' - `II` / `ii`: `0`
#' - `ADDL` / `addl`: `0`
#' - `SS` / `ss`: `0`
#' 
#' @return A data frame with missing nmtran-like columns added.
#' 
#' @examples
#' complete_tran(data.frame(time = c(0, 12, 24)))
#' 
#' complete_tran(data.frame(time = c(0, 12), amt = c(100, 0), cmt = 1))
#' 
#' complete_tran(data.frame(TIME = 0), case = "upper")
#' 
#' complete_tran(data.frame(time = c(0, 24)), id = FALSE)
#' 
#' @md
#' @export
complete_tran <- function(data, ..., case = c("lower", "upper", "preserve"), id = TRUE) {
  if(!is.data.frame(data)) {
    abort("`data` must be a data frame.")
  }
  case <- match.arg(case)
  if(!isTRUE(id) && !identical(id, FALSE)) {
    abort("`id` must be either TRUE or FALSE.")
  }
  nmtran <- c("ID", "TIME", "AMT", "CMT", "EVID", "RATE", "II", "ADDL", "SS")
  defaults <- list(
    ID = 1L, TIME = 0, AMT = 0, CMT = 1L, EVID = 0L,
    RATE = 0, II = 0, ADDL = 0L, SS = 0L
  )
  if(!id) {
    nmtran <- setdiff(nmtran, "ID")
    defaults <- defaults[nmtran]
  }
  if(identical(case, "preserve")) {
    n <- names(data)
    nupper <- sum(n %in% nmtran)
    nlower <- sum(n %in% tolower(nmtran))
    case <- if(nupper > nlower) "upper" else "lower"
  }
  target <- if(identical(case, "upper")) nmtran else tolower(nmtran)
  source <- if(identical(case, "upper")) tolower(nmtran) else toupper(nmtran)
  existing <- names(data)
  for(i in seq_along(target)) {
    if(!(target[i] %in% existing) && !(source[i] %in% existing)) {
      data[[target[i]]] <- rep(defaults[[nmtran[i]]], nrow(data))
    }
  }
  data
}
