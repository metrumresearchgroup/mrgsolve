##' @importFrom utils capture.output example str type.convert
##' @importFrom stats as.formula
##' @importFrom dplyr mutate mutate_  slice slice_ group_by_ summarise_ as.tbl select
##' @importFrom dplyr filter_ summarise_each_ select_ bind_rows mutate_each arrange data_frame as_data_frame
##' @importFrom dplyr intersect filter select rename slice distinct  do_
##' @importFrom lazyeval lazy_dots lazy
##' @importFrom magrittr %>% %T>%
##' @importFrom Rcpp evalCpp
NULL

GLOBALS <- new.env()

GLOBALS$ADVAN_PARMS <- list(
  "1" = c("CL","V"),
  "2" = c("CL","V","KA"),
  "3" = c("CL","V2","Q","V3"),
  "4" = c("CL","V2","Q","V3","KA")
)
GLOBALS$CARRY_TRAN_UC <- c("AMT", "CMT", "EVID", "II", "ADDL", "RATE", "SS")
GLOBALS$CARRY_TRAN_LC <- tolower(GLOBALS[["CARRY_TRAN_UC"]])
GLOBALS$CARRY_TRAN <- c("a.u.g", GLOBALS[["CARRY_TRAN_UC"]], GLOBALS[["CARRY_TRAN_LC"]])

GLOBALS$PKMODEL_NOT_FOUND <- "Required PK parameters not found: "

block_list <- c("ENV", "PROB", "PARAM", "INIT",
                "CMT", "ODE", "DES", "MAIN", "TABLE",
                "FIXED", "CMTN", "THETA", "NMXML", "VCMT",
                "ADVAN2", "ADVAN4", "PKMODEL", "INCLUDE",
                "OMEGA", "SIGMA", "SET","GLOBAL", "CAPTURE")


globalVariables(c("test_package","time", "ID","everything"))


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


