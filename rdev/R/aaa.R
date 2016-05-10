##'
##' @importFrom utils capture.output example str type.convert
##' @importFrom stats as.formula


GLOBALS <- new.env()

GLOBALS$ADVAN_PARMS <- list(
  "1" = c("CL","V"),
  "2" = c("CL","V","KA"),
  "3" = c("CL","V2","Q","V3"),
  "4" = c("CL","V2","Q","V3","KA")
)

GLOBALS$PKMODEL_NOT_FOUND <- "Required PK parameters not found: "

block_list <- c("ENV", "PROB", "PARAM", "INIT",
                "CMT", "ODE", "DES", "MAIN", "TABLE",
                "FIXED", "CMTN", "THETA", "NMXML", "VCMT",
                "ADVAN2", "ADVAN4", "PKMODEL",
                "OMEGA", "SIGMA", "SET","GLOBAL", "CAPTURE")


globalVariables(c("test_package","time"))


