# Copyright (C) 2013 - 2024  Metrum Research Group
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


#' Make addl doses explicit in an event object or data set
#' 
#' When doses are scheduled with `ii` and `addl`, the object is expanded to 
#' include one record for every dose. In the result, no record with have 
#' `ii` or `addl` set to non-zero value.
#' 
#' @param x a `data_set` data frame or an event object (see **Details**).
#' @param warn if `TRUE` a warning is issued if no `ADDL` or
#' `addl` column is found.
#' @param mark_new if `TRUE`, a flag is added to indicate new columns.
#' @param fill specifies how to handle non-dose related data columns
#' in new data set records; this option is critical when handling 
#' data sets with time-varying, non-dose-related data items; see **Details**.
#' @param ... not used.
#' 
#' @examples
#' e <- ev(amt = 100, ii = 12, addl = 3)
#' 
#' realize_addl(e)
#' 
#' a <- ev(amt = 100, ii = 12, addl = 2, WT = 69)
#' b <- ev(amt = 200, ii = 24, addl = 2, WT = 70)
#' c <- ev(amt =  50, ii =  6, addl = 2, WT = 71) 
#' 
#' e <- ev_seq(a,b,c)
#' realize_addl(e, mark_new = TRUE)
#' 
#' @return 
#' A data.frame or event object, consistent with the type of `x`.
#' The `ii` and `addl` columns will all be set to zero. The result is always
#' ungrouped.
#' 
#' @details
#' 
#' If no `addl` column is found the data frame is returned and 
#' a warning is issued if `warn` is true. If `ii`, 
#' `time`, or `evid` are missing, an error is generated.
#' 
#' If a grouped data.frame (via [dplyr::group_by()]) is passed, it will be
#' ungrouped.
#' 
#' Use caution when passing in data that has non-dose-related data 
#' columns that vary within a subject and pay special attention
#' to the `fill` argument. By definition, `realize_addl()`
#' will add new rows to your data frame and it is not obvious 
#' how the non-dose-related data should be handled in these new 
#' rows.  When `inherit` is chosen, the new records have
#' non-dose-related data that is identical to the originating 
#' dose record. This should be fine when these data items are not 
#' varying with time, but will present a problem when the data
#' are varying with time.  When `locf` is chosen, 
#' the missing data are filled in with `NA` and an
#' last observation carry forward operation is applied to 
#' **every** column in the data set.  This may not be what 
#' you want if you already had missing values in the input 
#' data set and want to preserve that missingness. When `na`
#' is chosen, the missing data are filled in with `NA` and 
#' no `locf` operation is applied.  But note that these
#' missing values may be problematic for a mrgsolve simulation 
#' run. If you have any time-varying columns or missing data
#' in your data set, be sure to check that the output from 
#' this function is what you were expecting. 
#' 
#' @md 
#' @export
realize_addl <- function(x,...) UseMethod("realize_addl")

#' @rdname realize_addl
#' @export
realize_addl.data.frame <- function(x, warn = FALSE, mark_new = FALSE, 
                                    fill = c("inherit", "na", "locf"), 
                                    ...) {
  
  x <- ungroup(x)
  
  fill <- match.arg(fill)
  locf <- fill=="locf"
  fill_na <- fill %in% c("locf", "na")
  
  hasid <- has_ID(x)
  
  addlcol <- which(names(x) %in% c("ADDL", "addl"))[1]
  
  if(is.na(addlcol)) {
    if(warn) warning("missing addl/ADDL column", call. = FALSE)
    return(x)
  }
  
  if(all(x[[addlcol]] <= 0)) {
    return(x)  
  }
  
  iicol <- which(names(x) %in% c("II", "ii"))[1]
  evidcol <- which(names(x) %in% c("evid", "EVID"))[1]
  timecol <- which(names(x) %in% c("TIME", "time"))[1]
  if(is.na(iicol)) stop("missing ii/II column", call.=FALSE)
  if(is.na(timecol)) stop("missing time/TIME column", call.=FALSE)
  if(is.na(evidcol)) stop("missing evid/EVID column", call.=FALSE)
  
  time_name <- names(x)[timecol]
  
  rown <- seq(nrow(x))
  
  add <- x[[addlcol]]
  
  expand <- lapply(rown, function(i) rep(i, add[i]))
  
  addl <- mutate(x, ..rown_  = rown)
  
  addl <- addl[unlist(expand),]
  
  addl <- group_by__(addl,"..rown_") 
  addl <- mutate(addl, ..dosen_ = seq(n())) 
  addl <- ungroup(addl)
  
  addl[[timecol]] <- addl[[timecol]] + addl[[iicol]] * addl[["..dosen_"]]
  
  addl[["..rown_"]] <- addl[["..dosen_"]] <- NULL
  
  sscol <- which(names(addl) %in% c("ss", "SS"))[1]
  if(!is.na(sscol)) {
    addl[[sscol]] <- 0
    x[[iicol]] <- x[[iicol]] * as.integer(x[[sscol]]!=0)
  } else {
    x[[iicol]] <- 0 
  }
  addl[[iicol]] <- 0
  
  addl[[evidcol]] <- ifelse(
    addl[[evidcol]] == 4, 
    1, 
    addl[[evidcol]]
  )  
  
  if(fill_na) {
    tran_cols <- GLOBALS[["TRAN_UPPER"]]
    tran_cols <- c("ID",tran_cols, tolower(tran_cols))
    addl <- addl[,intersect(names(addl),tran_cols)]  
  }
  
  addl <- mutate(addl, .addl_row_ = 1)
  x <- mutate(x, .addl_row_ = 0)
  
  df <- bind_rows(x,addl)
  
  df[[addlcol]] <- 0
  
  .arrang <- c(time_name, ".addl_row_")
  if(hasid) {
    .arrang <- c("ID", .arrang)  
  }
  
  df <- arrange__(df,.dots=.arrang)
  
  if(!mark_new) {
    df <- mutate(df, .addl_row_  = NULL)  
  }
  
  if(locf) {
    has_na <- any(is.na(x))
    if(has_na & hasid) {
      df <- locf_tibble(group_by__(df,"ID"))
    } else {
      df <- locf_tibble(df) 
    }
  }
  as.data.frame(df)
}

#' @rdname realize_addl
#' @export
realize_addl.ev <- function(x,...) {
  x@data <- realize_addl(x@data,...)
  return(x)
}
