# Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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


##' @importFrom utils capture.output example str type.convert packageVersion assignInMyNamespace
##' @importFrom stats as.formula
##' @importFrom dplyr mutate mutate_  slice slice_ group_by_ summarise_ as.tbl select
##' @importFrom dplyr filter_ summarise_each_ select_ bind_rows mutate_each arrange data_frame 
##' @importFrom dplyr intersect filter select rename slice distinct  do_ everything
##' @importFrom dplyr distinct_ first data_frame arrange
##' @importFrom lazyeval lazy_dots lazy
##' @importFrom magrittr %>% %T>%
##' @importFrom Rcpp evalCpp
##' @importFrom stats setNames
##' @importFrom RcppArmadillo armadillo_version
##' @importFrom stats rnorm
##' @importFrom tibble as_data_frame
##' @include class_mrgsims.R
##' @include class_mrgmod.R
##' @include class_derived.R
##' @include class_ev.R
##' @include class_matlist.R
##' @include class_numericlist.R
##' @include class_tgrid.R  
##' @include class_modlist.R
##' @include generics.R
##' @include package.R
##' @include utils.R 
##' @include RcppExports.R  
##' 
##' 
NULL

GLOBALS <- new.env()

GLOBALS$ADVAN_PARMS <- list(
  "1" = c("CL","V"),
  "2" = c("CL","V","KA"),
  "3" = c("CL","V1","Q","V2"),
  "4" = c("CL","V2","Q","V3","KA")
)
GLOBALS$CARRY_TRAN_UC <- c("AMT", "CMT", "EVID", "II", "ADDL", "RATE", "SS")
GLOBALS$CARRY_TRAN_LC <- tolower(GLOBALS[["CARRY_TRAN_UC"]])
GLOBALS$CARRY_TRAN <- c("a.u.g", GLOBALS[["CARRY_TRAN_UC"]], GLOBALS[["CARRY_TRAN_LC"]])
GLOBALS$PKMODEL_NOT_FOUND <- "Required PK parameters not found: "

block_list <- c("ENV", "PROB", "PARAM", "INIT",
                "CMT", "ODE", "DES", "MAIN", "TABLE",
                "FIXED", "CMTN", "THETA", "NMXML", "VCMT",
                "PKMODEL", "PLUGIN", "INCLUDE", "NAMESPACE",
                "OMEGA", "SIGMA", "SET","GLOBAL", "CAPTURE", 
                "PREAMBLE")

Reserved_cvar <- c("SOLVERTIME","table","ETA","EPS",
                   "ID", "TIME", "EVID","simeps", "self", "simeta",
                   "NEWIND", "DONE", "CFONSTOP", "DXDTZERO",
                   "CFONSTOP","INITSOLV","_F", "_R","_ALAG",
                   "SETINIT", "report")

Reserved <- c("ID", "amt", "cmt", "ii", "ss","evid",
              "addl", "rate","time", Reserved_cvar,
              "AMT", "CMT", "II", "SS", "ADDL", "RATE",
              paste0("pred_", c("CL", "VC", "V", "V2", "KA", "Q", "VP", "V3")),
              "double", "int", "bool", "capture")

globalVariables(c("test_package","time", "ID","block", "descr",
                  "everything", "TIME", "address","x",
                  "func", "loaded", "name", "not_found"))



#' Forward pipe.
#'
#' @name %>%
#' @export
#' @rdname zchain
NULL


#' Tee.
#'
#' @name %T>%
#' @export
#' @rdname zchain
NULL


