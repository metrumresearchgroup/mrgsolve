##' @importFrom utils capture.output example str type.convert packageVersion
##' @importFrom stats as.formula
##' @importFrom dplyr mutate mutate_  slice slice_ group_by_ summarise_ as.tbl select
##' @importFrom dplyr filter_ summarise_each_ select_ bind_rows mutate_each arrange data_frame as_data_frame
##' @importFrom dplyr intersect filter select rename slice distinct  do_ everything
##' @importFrom lazyeval lazy_dots lazy
##' @importFrom magrittr %>% %T>%
##' @importFrom Rcpp evalCpp
##' @importFrom stats setNames
##' @importFrom RcppArmadillo armadillo_version
##'

# 
# Collate order
# The bottom of the list is the most base / fundamental
# Working up includes files that depend on files that are lower down
# Once this list is implemented, no further includes
# of these files should be required
# 


##'
##' 
##' @include class_mrgsims.R
##' @include class_mrgmod.R
##' @include class_derived.R
##' @include class_ev.R
##' @include class_matlist.R
##' @include class_numericlist.R
##' @include class_tgrid.R  
##' @include class_modlist.R
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
                "PKMODEL", "PLUGIN", "INCLUDE",
                "OMEGA", "SIGMA", "SET","GLOBAL", "CAPTURE")

Reserved <- c("ID", "amt", "cmt", "ii", "ss","evid",
              "addl", "rate","time", "TIME",
              "AMT", "CMT", "II", "SS", "ADDL", "RATE","EVID",
              "SOLVERTIME","table","ETA","EPS",
              "NEWIND","DONE","DXDTZERO", 
              "CFONSTOP","INITSOLV","_F", "_R","_ALAG",
              paste0("pred_", c("CL", "VC", "V", "V2", "KA", "Q", "VP", "V3")),
              "_SETINIT","report","double", "int", "bool", "capture")


globalVariables(c("test_package","time", "ID",
                  "everything", "TIME", "address",
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


