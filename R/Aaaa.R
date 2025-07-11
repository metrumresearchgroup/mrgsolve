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

#' @import methods
#' 
#' @importFrom utils capture.output example str type.convert .DollarNames
#' @importFrom utils packageVersion assignInMyNamespace read.table
#' @importFrom stats as.formula setNames rnorm
#' @importFrom dplyr filter mutate mutate_at mutate_all distinct first
#' @importFrom dplyr bind_rows arrange summarise summarise_at mutate_if
#' @importFrom dplyr intersect select rename do slice pull
#' @importFrom dplyr if_else summarise_each is.tbl select
#' @importFrom dplyr group_by ungroup n left_join
#' @importFrom tidyselect vars_select everything
#' @importFrom magrittr %>%
#' @importFrom tibble tibble as_tibble
#' @importFrom rlang quos enquo enquos quo_name syms !!! !! eval_tidy as_label
#' @importFrom rlang is_named .data abort warn inform caller_env
#' @importFrom lifecycle deprecate_soft deprecate_warn
#' @importFrom glue glue
#' @importFrom Rcpp evalCpp
#' @importFrom tools md5sum file_path_sans_ext
#  @importFrom RcppArmadillo armadillo_version

#' @include Aaaa.R
#' @include class_mrgsims.R
#' @include class_mrgmod.R
#' @include class_derived.R
#' @include class_ev.R
#' @include class_matlist.R
#' @include class_numericlist.R
#' @include class_tgrid.R  
#' @include generics.R
#' @include package.R
#' @include utils.R 
#' @include RcppExports.R  
#' 
NULL

#' @export
dplyr::filter

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
GLOBALS$TRAN_UPPER <- c("AMT", "II", "SS", "CMT", "ADDL", "RATE", "EVID", "TIME")
GLOBALS$TRAN_LOWER <- tolower(GLOBALS$TRAN_UPPER)
GLOBALS$TRAN_FILL_NA <- c(
  "AMT", "CMT", "EVID", "II", "ADDL", "RATE", "SS", 
  "amt", "cmt", "evid", "ii", "addl", "rate", "ss"
)
GLOBALS$UPDATE_SINGLE <- c(
  "atol", "rtol", "ss_rtol", "ss_atol", "verbose", "debug", "preclean", "mindt",
  "digits", "ixpr", "mxhnil", "start", "end", "add", "delta", "maxsteps",
  "hmin", "hmax", "tscale", "request"
 )
GLOBALS$UPDATE_OTHER <- c("param", "init", "omega", "sigma", "outvars")
GLOBALS$UPDATE_ALL <- c(GLOBALS$UPDATE_SINGLE, GLOBALS$UPDATE_OTHER)
GLOBALS$SET_EXTRA <- c(
  "collapse_omega", "collapse_sigma", "ss_cmt", "clink", "fixed_type"
)
# Arguments to mrgsim that can be stated in $SET and then 
# passed to mrgsim
GLOBALS$SET_ARGS <- c("Req", "obsonly", "recsort", "carry.out", "Trequest", 
 "trequest", "carry_out", "Request"
)
GLOBALS[["version"]] <- utils::packageVersion("mrgsolve")

block_list <- c("ENV", "PROB", "PARAM", "INIT",
                "CMT", "ODE", "DES", "MAIN", "TABLE",
                "FIXED", "CMTN", "THETA", "NMXML", "VCMT",
                "PKMODEL", "PLUGIN", "INCLUDE", "NAMESPACE",
                "OMEGA", "SIGMA", "SET","GLOBAL", "CAPTURE", 
                "PREAMBLE", "PRED", "BLOCK", "TRANSIT", "YAML", "NMEXT", 
                "INPUT", "EVENT")

block_list_single <- c("MAIN", "SET", "GLOBAL", "PREAMBLE", "PRED", "PKMODEL", 
                       "ENV", "CMTN", "INCLUDE", "NAMESPACE", "BLOCK", 
                       "TRANSIT", "YAML", "EVENT")

Reserved_cvar <- c("SOLVERTIME","table","ETA","EPS", "AMT", "CMT",
                   "ID", "TIME", "EVID","simeps", "self", "simeta",
                   "NEWIND", "DONE", "CFONSTOP", "DXDTZERO",
                   "CFONSTOP","INITSOLV","_F", "_R","_ALAG",
                   "SETINIT", "report", "_VARS_", "VARS", 
                   "SS_ADVANCE", "END_OF_INFUSION")

Reserved <- c("ID", "amt", "cmt", "ii", "ss", "evid",
              "addl", "rate","time", Reserved_cvar,
              "AMT", "CMT", "II", "SS", "ADDL", "RATE", "THETA",
              paste0("pred_", c("CL", "VC", "V", "V2", "KA", "Q", "VP", "V3")),
              "double", "int", "bool", "capture", 
              "until", "now")

Reserved_nm <- c("A", "DADT", "A_0", "T")

globalVariables(c("test_package","time", "ID","block", "descr",
                  "everything", "TIME", "address","x", 
                  "self",
                  "func", "loaded", "name", "not_found"))


VERSION <- utils::packageVersion("mrgsolve")

DPLYR_1_0_0 <- packageVersion("dplyr") >= '0.8.99.9000'

#' Forward pipe
#' 
#' 
#'
#' @name %>%
#' @rdname zchain
#' @export
#' @keywords internal
NULL


