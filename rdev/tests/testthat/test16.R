library(mrgsolve)
library(testthat)
require(dplyr)

Sys.setenv(R_TESTS="")

options("mrgsolve_mread_quiet"=TRUE)
project <- file.path(system.file(package="mrgsolve"), "models")

context("rename columns in output")
mod <- mrgsolve:::house()
data(exTheoph) 
df <- exTheoph





